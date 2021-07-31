/**
 * Evaluator for the sizr-format language
 */
use crate::parser;
use std::cell::Cell;
use std::option::Option;
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
    fmt: parser::File<'a>,
}

impl<'a> EvalCtx<'a> {
    pub fn new(cursor: tree_sitter::TreeCursor<'a>, fmt: parser::File<'a>) -> Self {
        EvalCtx {
            result: String::new(),

            // I clearly don't know how to do mutable variables, I should consider separating these I suppose
            currentLineLen: Cell::new(0),
            cursor,
            currentIndentLevel: Cell::new(0),

            targetLineLen: 80,
            indentString: "  ",
            fmt,
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

pub fn eval(tree: tree_sitter::TreeCursor, fmt: parser::File) -> Option<&'static str> {
    let mut ctx = EvalCtx::new(tree, fmt);
    return eval_cmd(&mut ctx);
}

fn eval_cmd<'a>(ctx: &mut EvalCtx<'a>) -> Option<&'static str> {
    use parser::IndentMark::*;
    use parser::WriteCommand::*;

    let node = ctx.cursor.node();
    let cmd = &ctx.fmt.nodes[node.kind()];
    // TODO: need to verify in a semantic analysis step that node children are written in order
    ctx.cursor.goto_first_child();
    match cmd {
        Raw(s) => ctx.result.push_str(s),
        NodeReference { .. } => {
            eval_cmd(ctx)?;
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
                unsafe {
                    if let Some(err) = eval_cmd(ctx) {
                        return Some(err);
                    }
                }
            }
            /*
            unsafe {
                cmds.iter().any(|_| eval_cmd(ctx).is_some());
            }
            */
            //cmds.map()
        }
    };
    ctx.cursor.goto_parent();
    None
}
