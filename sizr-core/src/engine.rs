/**
 * Engine for the sizr ast transformation language
 */

use std::collections::HashMap;
use std::result::Result;
use crate::parser::{PropValue, Ast}; // TODO: move to code module

use tree_sitter::{Parser, Language, Tree};

extern "C" { fn tree_sitter_python() -> Language; }

fn init() {
    let language = unsafe { tree_sitter_python() };
    let mut parser = Parser::new();
    parser.set_language(language).expect("python parser failed to load");
}

pub enum AstOps<'a> {
    Up,
    Link(str),
    Test{
        pattern: str,
        props: HashMap<&'a str, PropValue<'a>>,
    },
    Next
}

fn select<'a>(ast: Tree, transform: Ast<'a>) -> Result<TransformResult> {
    //if 
    //root: cst
}

fn assert(Result<TransformResult>) -> Result<Tree> {
    // check if there are references, if there aren't,
    // run once, otherwise run once per match basically
}

fn exec_transform<'a>(src: &'a str, transform: Ast<'a>) -> Result<String> {
    let ast = parser.parse(src, None)?; // Result or option?
    let result = assert(select(ast, transform));
    result
}

