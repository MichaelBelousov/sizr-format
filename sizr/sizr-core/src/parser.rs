/**
 * Parser for the sizr ast transformation language
 */
extern crate regex;
//extern crate lazy_static;

use regex::Regex;
use std::boxed::Box;
use std::cell::Cell;
use std::option::Option;
use std::vec::Vec;

#[derive(Debug)]
pub(crate) enum NestingType {
    Arg,    // (
    Next,   // ,
    Member, // .
    Impl,   // {
}

impl NestingType {
    fn from_str(s: &str) -> Option<NestingType> {
        match s.chars().nth(0) {
            Some('(') => Some(NestingType::Arg),
            Some(',') => Some(NestingType::Next),
            Some('.') => Some(NestingType::Member),
            Some('{') => Some(NestingType::Impl),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub(crate) enum TransformType {
    Additive,  // >>>
    Replacing, // >>!
}

impl TransformType {
    // XXX: this matches for example ">>>@#$^$#", which should be unexpected
    // and yet I rely on it else where, should change
    fn from_str(s: &str) -> Option<TransformType> {
        match &s[0..3] {
            ">>>" => Some(TransformType::Additive),
            ">>!" => Some(TransformType::Replacing),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub(crate) enum PropValue<'a> {
    Boolean(bool),
    Integer(i64),
    Real(f64),
    String(&'a str),
}

#[derive(Debug)]
pub(crate) enum Ast<'a> {
    Transform {
        selector: Option<Box<Ast<'a>>>,
        r#type: TransformType,
        assertor: Option<Box<Ast<'a>>>,
    },
    Query {
        path: Vec<Box<Ast<'a>>>,
    },
    Elem {
        capture: Box<Ast<'a>>,
        nester: NestingType,
        props: Vec<Box<Ast<'a>>>,
    },
    Prop {
        key: &'a str,
        value: PropValue<'a>,
    },
    Capture {
        name: &'a str,
        pattern: Option<Regex>,
    },
}

#[derive(Debug)]
struct ParseContext<'a> {
    pub src: &'a str,
    // TODO: replace with mutex?
    pub loc: Cell<usize>,
}

impl<'a> ParseContext<'a> {
    pub fn new(in_src: &'a str) -> Self {
        ParseContext {
            src: in_src,
            loc: Cell::new(0),
        }
    }

    pub fn remaining_src(&self) -> &'a str {
        &self.src[self.loc.get()..]
    }

    pub fn next_char(&self) -> Option<char> {
        self.remaining_src().chars().nth(0)
    }

    pub fn inc_loc(&self, inc: usize) -> usize {
        // FIXME: find idiomatic rust solution for this stuff
        // I think a mutex might be correct
        &self.loc.set(self.loc.get() + inc);
        self.loc.get()
    }

    fn skip_whitespace(&mut self) {
        if let Some(jump) = &self.remaining_src().find(|c: char| !c.is_whitespace()) {
            &self.inc_loc(*jump);
        }
    }
}

pub mod matchers {
    use super::*;

    pub(super) fn is_capture<'a>(ctx: &ParseContext<'a>) -> bool {
        ctx.remaining_src()
            .chars()
            .nth(0)
            .map(|c| c == '$')
            .unwrap_or(false)
    }

    pub(super) fn is_scope_prop<'a>(ctx: &ParseContext<'a>) -> bool {
        ctx.remaining_src()
            .chars()
            .nth(0)
            .map(|c| c.is_ascii_alphabetic() || c == '!')
            .unwrap_or(false)
    }

    pub(super) fn is_query<'a>(ctx: &ParseContext<'a>) -> bool {
        is_scope_prop(&ctx) || is_capture(&ctx)
    }

    pub(super) fn is_scope_expr<'a>(ctx: &ParseContext<'a>) -> bool {
        is_query(&ctx)
    }
}

pub mod try_parse {
    use super::*;

    pub(super) fn nesting_op(ctx: &ParseContext) -> Option<NestingType> {
        NestingType::from_str(&ctx.remaining_src())
    }

    pub(super) fn transform_op(ctx: &ParseContext) -> Option<TransformType> {
        TransformType::from_str(&ctx.remaining_src())
    }

    pub(super) fn scope_expr<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
        None
    }

    pub(super) fn query<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
        let mut path: Vec<Box<Ast>> = Vec::new();
        while let Some(expr) = try_parse::scope_expr(&ctx) {
            path.push(Box::new(expr));
        }
        Some(Ast::Query { path })
    }

    pub(super) fn transform<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
        let maybe_selector = try_parse::query(&ctx);
        let transform_op = try_parse::transform_op(&ctx)?;
        let maybe_assertor = try_parse::query(&ctx);
        if maybe_selector.is_none() && maybe_assertor.is_none() {
            return None;
        }
        let result = Ast::Transform {
            selector: match maybe_selector {
                Some(selector) => Some(Box::new(selector)),
                _ => None,
            },
            r#type: transform_op,
            assertor: match maybe_assertor {
                Some(assertor) => Some(Box::new(assertor)),
                _ => None,
            },
        };
        println!("{:?}", result);
        Some(result)
    }
}

pub(crate) fn parse_command<'a>(src: &'a str) -> Ast<'a> {
    let ctx = ParseContext::new(src);
    let result = try_parse::transform(&ctx);
    result.expect("invalid transform")
}
