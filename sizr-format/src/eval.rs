/**
 * Evaluator for the sizr-format language
 */
use crate::parser;
use std::cell::Cell;
use std::string::String;

struct EvalCtx<'a> {
    result: String,

    // state
    currentLineLen: Cell<usize>,
    currentIndentLevel: Cell<u16>,
    cursor: tree_sitter::TreeCursor<'a>,

    // config
    targetLineLen: usize,
    indentString: &'a str,
}

impl<'a> EvalCtx<'a> {
    pub fn new(cursor: tree_sitter::TreeCursor<'a>) -> Self {
        EvalCtx {
            result: String::new(),

            // consider separating these?
            currentLineLen: Cell::new(0),
            cursor,
            currentIndentLevel: Cell::new(0),

            targetLineLen: 80,
            indentString: "  ",
        }
    }

    pub fn wrap(&mut self) {
        self.result.push_str("\n");
        for _ in 0..self.currentIndentLevel.get() {
            self.result.push_str(self.indentString);
        }
        self.currentLineLen
            .set(self.indentString.len() * self.currentIndentLevel.get() as usize);
    }
}

pub fn eval(tree: tree_sitter::TreeCursor, fmt: parser::File) -> Result<(), &'static str> {
    let mut ctx = EvalCtx::new(tree);
    let cmd_result = fmt
        .nodes
        .get(ctx.cursor.node().kind())
        .ok_or("couldn't find node");
    if cfg!(debug_assertions) && cmd_result.is_err() {
        eprintln!(
            "couldn't find node declaration for '{}'",
            ctx.cursor.node().kind()
        );
        eprintln!("fmt was {:#?}", fmt);
        //eprintln!("node was {:#?}", ctx.cursor.node());
    }
    let cmd = cmd_result?;
    return eval_cmd(cmd, &mut ctx, &fmt);
}

fn eval_cmd(
    cmd: &parser::WriteCommand,
    ctx: &mut EvalCtx,
    fmt: &parser::File,
) -> Result<(), &'static str> {
    use parser::IndentMark::*;
    use parser::WriteCommand::*;

    let node = ctx.cursor.node();
    let cmd = &fmt.nodes[node.kind()];
    // TODO: need to verify in a semantic analysis step that node children are written in order
    ctx.cursor.goto_first_child();
    match cmd {
        Raw(s) => ctx.result.push_str(s),
        NodeReference { name, .. } => {
            let child_result = node
                .child_by_field_name(name)
                .ok_or("couldn't find child field");
            if cfg!(debug_assertions) && child_result.is_err() {
                eprintln!("couldn't find child field: '{}'", name);
            }
            let child = child_result?;
            let child_cmd = &fmt.nodes[child.kind()];
            eval_cmd(child_cmd, ctx, fmt)?;
            ctx.cursor.goto_next_sibling();
        }
        WrapPoint => {
            if ctx.currentLineLen.get() > ctx.targetLineLen {
                ctx.wrap();
            }
        }
        Conditional { .. } => unimplemented!(),
        IndentMark(mark) => match mark {
            Indent(amt) => ctx
                .currentIndentLevel
                .set(ctx.currentIndentLevel.get() + amt),
            Outdent(amt) => ctx
                .currentIndentLevel
                .set(ctx.currentIndentLevel.get() - amt),
            TokenAnchor(..) => unimplemented!(),
            NumericAnchor(..) => unimplemented!(),
        },
        Sequence(cmds) => {
            for cmd in cmds {
                eval_cmd(cmd, ctx, fmt)?;
            }
        }
    };
    ctx.cursor.goto_parent();
    Ok(())
}
