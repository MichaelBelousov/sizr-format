/**
 * Evaluator for the sizr-format language
 */

struct EvalCtx<'a> {
    result: String,

    // state
    currentLineLen: Cell<usize>,
    currentNode: tree_sitter::Node,
    currentIndentLevel: Cell<usize>,

    // config
    targetLineLen: usize,
    indentString: &'a str,
}


pub fn eval(tree: tree_sitter::TreeCursor, fmt: parser::File) -> String {
    let mut result = String::new();
    //tree.node
    //tree.
    return evalAux(tree.node, fmt, result)
}

fn evalAux(nodeRef: tree_sitter::TreeCursor, fmt: parser::File, result: String) -> Result<String, &'static str> {
    let node = nodeRef.node();
    // can guarantee earlier that all node types are accounted for
    let fmter = fmt.nodes[node.kind()]?;
    let nextResult = evalFmt(fmter, fmt, result)
    return evalFmt(fmter, node, )
}

fn evalFmt(node: tree_sitter::Node, fmter: parser::Node) -> Result<String, &'static str> {
    for cmd in fmter.commands {
        
    }
}

fn evalCmd(evalCtx: &EvalCtx, cmd: &parser::WriteCommand, result: String) {
    use parser::WriteCommand;
    match cmd {
        Raw(ref s) => result.push_str(s),
        NodeReference { ref name, filters } => { },
        WrapPoint => {}
        Conditional { test, then, r#else, } => 
        IndentMark(ref mark) => {},
        Sequence(cmds) => { for cmd in cmds { evalCmd(cmd) } },
    }
}