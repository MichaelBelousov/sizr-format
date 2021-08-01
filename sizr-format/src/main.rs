#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate tree_sitter;
extern crate tree_sitter_python;

use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;

//mod vm;
mod eval;
mod parser;

fn main() -> io::Result<()> {
    let mut python_parser = tree_sitter::Parser::new();
    let python_language = tree_sitter_python::language();
    python_parser.set_language(python_language).unwrap();

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: sizr-format ./python_src.py ./tree_format.szf")
    }
    let python_src_path = Path::new(args.get(1).expect("expected a python source file path"));
    let tree_fmt_path = Path::new(args.get(2).expect("expected a tree format file path"));

    let python_src = fs::read_to_string(python_src_path).expect(&format!(
        "could not read python src at '{}'",
        python_src_path.to_str().expect("path wasn't valid unicode")
    ));
    let tree_format = fs::read_to_string(tree_fmt_path).expect(&format!(
        "could not read python src at '{}'",
        tree_fmt_path.to_str().expect("path wasn't valid unicode")
    ));

    let python_ast = python_parser
        // should remove this clone :(
        .parse(python_src.clone(), None)
        .expect("invalid python code passed!");

    //let mut buffer = String::new();
    //io::stdin().read_to_string(&mut buffer)?;

    let ctx = parser::ParseContext::new(&tree_format);
    let node_fmt_ast = parser::parse_text(&ctx);
    println!("treefmt_ast: {:#?}", node_fmt_ast);
    println!("python_ast: {}", python_ast.root_node().to_sexp());

    match node_fmt_ast.and_then(|fmt| eval::eval(&python_src, python_ast.walk(), fmt)) {
        Ok(fmted_python_src) => {
            println!("fmted src:\n{}", fmted_python_src);
        }
        Err(err) => {
            println!("formatting had error: '{}'", err);
        }
    };

    Ok(())
}
