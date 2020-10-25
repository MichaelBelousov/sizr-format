/**
 * Parser for the sizr ast transformation language
 */

 extern crate regex;

 use regex::Regex;
 use std::option::Option;

// NOTE: I will add a tokenize function and rewrite this crud in
// terms of tokens when rust adds stable coroutines/generators

// lazy_static!
// static pattern_any: Regex = Regex::new("").expect("trivial 'any' regex failed");

// TODO: provide a macro for non-redundant implementation (or custom Derive!)
/** language backends wrap */
pub trait Elem<'a> {
    fn new(src: &'a str, start: usize) -> Self;
    fn src(&self) -> &'a str;
    fn start(&self) -> usize;
}

struct PythonElem<'a> {
    _src: &'a str,
    _start: usize,
}

impl<'a> Elem<'a> for PythonElem<'a> {
    fn new(_src: &'a str, _start: usize) -> Self { PythonElem{ _src, _start } }
    fn src(&self) -> &'a str { self._src }
    fn start(&self) -> usize { self._start }
}

#[derive(Debug)]
struct ElemExpr<'a> {
    pattern: Regex,
    name: &'a str,
    // not sure yet how to in rust "move" this instance into the result, may be automatic
}
impl<'a> ElemExpr<'a> {
    fn contextualize<T, R: Elem<'a>>(&self, capture_ctx: T) -> R {
        R::new("none", 0)
    }
}

