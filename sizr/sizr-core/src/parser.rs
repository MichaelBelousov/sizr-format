/**
 * Parser for the sizr ast transformation language
 */

extern crate regex;
//extern crate lazy_static;

use regex::Regex;
use std::vec::Vec;
use std::boxed::Box;
use std::cell::Cell;
use std::option::Option;

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
            _ => None
        }
    }
}

#[derive(Debug)]
pub(crate) enum TransformType {
    Additive, // >>>
    Replacing // >>!
}

impl TransformType {
    // XXX: this matches for example ">>>@#$^$#", which should be unexpected
    // and yet I rely on it else where, should change
    fn from_str(s: &str) -> Option<TransformType> {
        match &s[0..3] {
            ">>>" => Some(TransformType::Additive),
            ">>!" => Some(TransformType::Replacing),
            _ => None
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
        assertor: Option<Box<Ast<'a>>>
    },
    Query {
        path: Vec<Box<Ast<'a>>>
    },
    Elem {
        capture: Box<Ast<'a>>,
        nester: NestingType,
        props: Vec<Box<Ast<'a>>>
    },
    Prop {
        key: &'a str,
        value: PropValue<'a>
    },
    Capture {
        name: &'a str,
        pattern: Option<Regex>
    }
}

#[derive(Debug)]
struct ParseContext<'a> {
    pub src: &'a str,
    // TODO: replace with mutex?
    pub loc: Cell<usize>
}

impl<'a> ParseContext<'a> {
    pub fn new(in_src: &'a str) -> Self {
        ParseContext{
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


fn try_parse_nesting_op(ctx: &ParseContext) -> Option<NestingType> {
    NestingType::from_str(&ctx.remaining_src())
}

fn try_parse_transform_op(ctx: &ParseContext) -> Option<TransformType> {
    TransformType::from_str(&ctx.remaining_src())
}

fn try_parse_query<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
    let result = Ast::Query{ path: vec![] };
    println!("{:?}", result);
    None
}

fn try_parse_transform<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
    let maybe_selector = try_parse_query(&ctx);
    let transform_op = try_parse_transform_op(&ctx)?;
    let maybe_assertor = try_parse_query(&ctx);
    let result = Ast::Transform {
        selector: match maybe_selector {
            Some(selector) => Some(Box::new(selector)),
            _ => None
        },
        r#type: transform_op,
        assertor: match maybe_assertor {
            Some(assertor) => Some(Box::new(assertor)),
            _ => None
        },
    };
    println!("{:?}", result);
    Some(result)
}


pub(crate) fn parse_command<'a>(src: &'a str) -> Ast<'a> {
    let ctx = ParseContext::new(src);
    let result = try_parse_transform(&ctx);
    result.expect("invalid transform")
}
