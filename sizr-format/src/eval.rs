/**
 * Evaluator for the sizr-format language
 */
use crate::parser;
use std::cell::Cell;
use std::string::String;

struct EvalCtx<'a> {
    text: &'a str,
    result: String,

    // state
    current_indent_level: Cell<u16>,
    cursor: tree_sitter::TreeCursor<'a>,
    current_line: String,

    // config
    target_line_len: usize,
    indent_str: &'a str,
}

impl<'a> EvalCtx<'a> {
    pub fn new(text: &'a str, cursor: tree_sitter::TreeCursor<'a>) -> Self {
        let target_line_len = 80;
        EvalCtx {
            text,
            result: String::new(),

            // consider separating these?
            cursor,
            current_indent_level: Cell::new(0),
            current_line: String::with_capacity(target_line_len + 20),

            target_line_len,
            indent_str: "  ",
        }
    }

    pub fn inc_indent(&mut self, amt: i32) {
        self.current_indent_level
            .set((self.current_indent_level.get() as i32 + amt) as u16);
    }
    pub fn wrap(&mut self) {
        for _ in 0..self.current_indent_level.get() {
            self.result.push_str(self.indent_str);
        }
        self.result.push_str(&self.current_line);
        self.result.push_str("\n");
        self.current_line.clear();
    }

    pub fn write(&mut self, text: &str) {
        // TODO: use intersperse
        let line_count = text.chars().filter(|c| c == &'\n').count() + 1;
        for (line_no, line) in text.lines().enumerate() {
            self.current_line.push_str(line);
            if line_no != line_count - 1 {
                self.wrap();
            }
        }
    }
}

pub fn eval(
    text: &str,
    tree: tree_sitter::TreeCursor,
    fmt: parser::File,
) -> Result<String, &'static str> {
    let mut ctx = EvalCtx::new(text, tree);
    let cmd_result = fmt
        .nodes
        .get(ctx.cursor.node().kind())
        .ok_or("couldn't find node");
    if cfg!(debug_assertions) && cmd_result.is_err() {
        eprintln!(
            "couldn't find node declaration for '{}'",
            ctx.cursor.node().kind()
        );
    }
    let cmd = cmd_result?;
    eval_cmd(cmd, &mut ctx, &fmt)?;
    return Ok(ctx.result);
}

fn eval_cmd(
    cmd: &parser::WriteCommand,
    ctx: &mut EvalCtx,
    fmt: &parser::File,
) -> Result<(), &'static str> {
    use parser::IndentMark::*;
    use parser::WriteCommand::*;

    let node = ctx.cursor.node();

    // TODO: need to verify in a semantic analysis step that node children are written in order
    if cfg!(debug_assertions) {
        //println!("node kind is {}, evaluating cmd {:?}", node.kind(), cmd);
    }
    if node.named_child_count() == 0 {
        ctx.write(
            node.utf8_text(ctx.text.as_bytes())
                .expect("utf8 should be valid"),
        );
    } else {
        match cmd {
            Raw(s) => ctx.write(s),
            // NOTE: need some kind of special handling for arrays, not just filters
            // but a way to specify special delimiting of array elements, as well as language-defaults
            NodeReference { ref name, .. } => {
                // TEMP HACK
                // haven't decided how to handle unnamed child lists from tree_sitter yet so
                // $CHILDREN is a stop-gap for testing existing work
                // HACK:
                if *name == "CHILDREN" || *name == "NAMED_CHILDREN" {
                    //ctx.cursor.reset(node);
                    let had_children = ctx.cursor.goto_first_child();
                    if had_children {
                        loop {
                            if ctx.cursor.node().is_named() || *name == "CHILDREN" {
                                let cmd_result = fmt
                                    .nodes
                                    .get(ctx.cursor.node().kind())
                                    .ok_or("couldn't find node");
                                if cfg!(debug_assertions) && cmd_result.is_err() {
                                    eprintln!(
                                        "couldn't find node declaration for '{}'",
                                        ctx.cursor.node().kind()
                                    );
                                }
                                let cmd = cmd_result?;
                                eval_cmd(cmd, ctx, fmt)?;
                            }
                            if !ctx.cursor.goto_next_sibling() {
                                break;
                            }
                        }
                        ctx.cursor.goto_parent();
                    } else {
                        //HACK:
                        let raw = node
                            .utf8_text(ctx.text.as_bytes())
                            .expect("utf8 should be valid");
                        println!("\tprinting {}", raw);
                        ctx.write(raw);
                    }
                } else {
                    let child_result = node
                        .child_by_field_name(name)
                        .ok_or("couldn't find child field");
                    if cfg!(debug_assertions) && child_result.is_err() {
                        eprintln!("couldn't find child field: '{}'", name);
                        eprintln!("cursor was: '{:#?}'", node);
                        for child in node.children(&mut node.walk()) {
                            eprintln!("had named child: '{:#?}'", child);
                        }
                    }
                    let child = child_result?;
                    let child_cmd = &fmt.nodes[child.kind()];
                    ctx.cursor.reset(child);
                    eval_cmd(child_cmd, ctx, fmt)?;
                    ctx.cursor.reset(node);
                }
            }
            WrapPoint => {
                if ctx.current_line.len() > ctx.target_line_len {
                    ctx.wrap();
                }
            }
            Conditional { .. } => unimplemented!(),
            IndentMark(mark) => match mark {
                Indent(amt) => ctx.inc_indent(*amt as i32),
                Outdent(amt) => ctx.inc_indent(-(*amt as i32)),
                TokenAnchor(..) => unimplemented!(),
                NumericAnchor(..) => unimplemented!(),
            },
            Sequence(cmds) => {
                for cmd in cmds {
                    eval_cmd(cmd, ctx, fmt)?;
                }
            }
        };
    }

    /*
    if had_children {
        ctx.cursor.goto_parent();
    }
    */

    Ok(())
}
