/**
 * Parser for the sizr ast transformation language
 */

 extern crate regex;

 use regex::Regex;
 use std::option::Option;
 use std::vec::Vec;
 use std::collections::HashMap;
 use crate::parser;
 use tree_sitter::{Tree, Node};

// NOTE: I will add a tokenize function and rewrite this crud in
// terms of tokens when rust adds stable coroutines/generators

// lazy_static!
// static pattern_any: Regex = Regex::new("").expect("trivial 'any' regex failed");

// TODO: provide a macro for non-redundant implementation (or custom Derive!)
/** language backends wrap */
pub trait Elem<'a, T> {
    fn new(src: &'a str, start: usize, ctx: T) -> Self;
    fn src(&self) -> &'a str;
    fn start(&self) -> usize;
}

#[derive(Debug)]
struct ElemExpr<'a> {
    pattern: Regex,
    name: &'a str,
}

// not sure yet how to in rust "move" this instance into the result, may be automatic
impl<'a> ElemExpr<'a> {
    fn contextualize<T, R: Elem<'a, T>>(&self, capture_ctx: T) -> R {
        R::new("none", 0, capture_ctx)
    }
}

/** */ // maybe rename to Element?
#[derive(Debug)]
struct Capture<'a> {

}

#[derive(Debug)]
struct Selection<'a> {
    expr: parser::Ast<'a>,
    captures: Vec<Capture<'a>>;
    //references: HashMap<Tree, >;
}

#[derive(Debug)]
struct Transform<'a> {
    expr: parser::Ast<'a>,
    selection: Selection<'a>,
}

impl<'a> Transform<'a> {
    // TODO: make every variant of Ast point to an individual concrete type which
    // can be specified as an argument type
    fn from_expr(expr: &parser::Ast<'a>, captures: ) {

    }
}

