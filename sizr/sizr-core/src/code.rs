/**
 * Parser for the sizr ast transformation language
 */

 extern crate regex;

 use regex::Regex;
 use std::option::Option;

// NOTE: I will add a tokenize function and rewrite this crud in
// terms of tokens when rust adds stable coroutines/generators

static pattern_any = Regex::new("").expect("trivial 'any' regex failed");

/** a capturable is a blueprint for a program element that can be captured
 * by receiving a context
 */
trait Capturable<T> {
    fn contextualize(&self, capture_ctx: T) -> &Self;
    fn context(&self) -> Option<T>;
}

// TODO: provide a macro for non-redundant implementation (or custom Derive!)
/** language backends wrap */
trait Elem<'a> {
    fn src(): &'a str;
    fn start(): usize;
}

struct PythonElem<'a> {
    _src: &'a str,
    _start: usize,
}

#[Derive(Debug)]
struct ElemExpr<'a> {
    pattern: Regex,
    name: &'str
    // not sure yet how to in rust "move" this instance into the result, may be automatic
    fn contextualize<T, R: Elem>(&self, capture_ctx: T) -> R {
        hell
    }
}

