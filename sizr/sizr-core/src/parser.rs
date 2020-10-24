/**
 * Parser for the sizr ast transformation language
 */
extern crate regex;
//extern crate lazy_static;

use regex::Regex;
use std::boxed::Box;
use std::cell::Cell;
use std::collections::HashMap;
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
        capture: Option<Box<Ast<'a>>>,
        nester: Option<NestingType>,
        props: HashMap<&'a str, PropValue<'a>>,
    },
    Prop {
        key: &'a str,
        value: PropValue<'a>,
    },
    Capture {
        name: Option<&'a str>,
        // TODO: probably need a Pattern enum containing regex or str or any
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

    pub fn inc_loc(&self, inc: usize) -> usize {
        // FIXME: find idiomatic rust solution for this stuff
        // I think a mutex might be correct
        &self.loc.set(self.loc.get() + inc);
        self.loc.get()
    }

    fn skip_whitespace(&self) {
        if let Some(jump) = &self.remaining_src().find(|c: char| !c.is_whitespace()) {
            &self.inc_loc(*jump);
        }
    }

    fn cur_token_end(&self) -> usize {
        self.remaining_src()
            .find(|c: char| c.is_whitespace())
            .unwrap_or(self.remaining_src().len())
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

// TODO: move to utils
//end_slash_offset = ctx.remaining_src().find(|c, i| c == '/')
trait StrUtils {
    fn find_test<F>(&self, f: F) -> Option<usize>
    where
        F: Fn(char, usize) -> bool;
}

impl StrUtils for str {
    fn find_test<F>(&self, f: F) -> Option<usize>
    where
        F: Fn(char, usize) -> bool,
    {
        // XXX: incorrect on unicode because str.find returns byte offset,
        // this is character offset
        for (i, c) in self.chars().enumerate() {
            if f(c, i) {
                return Some(i);
            }
        }
        return None;
    }
}

// this whole module thing might demonstrate a need for an enum...
// like impl<Ast> with parse methods... sounds way better but I'll do that
// after the initial conversion
pub mod try_parse {
    use super::*;

    pub(super) fn nesting_op(ctx: &ParseContext) -> Option<NestingType> {
        NestingType::from_str(&ctx.remaining_src())
    }

    pub(super) fn transform_op(ctx: &ParseContext) -> Option<TransformType> {
        TransformType::from_str(&ctx.remaining_src())
    }

    pub(super) fn capture<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
        //if !ctx.remaining_src().len() > 0 panic!()
        let is_regex_capture = &ctx.remaining_src()[..2] == "$/";
        let is_named_capture = ctx
            .remaining_src()
            .chars()
            .nth(1)
            .map(|c| c.is_ascii_alphabetic())
            .unwrap_or(false);
        let is_anonymous_capture = ctx
            .remaining_src()
            .chars()
            .nth(0)
            .map(|c| c == '$')
            .map(|b| b && !is_named_capture)
            .unwrap_or(false);
        if is_regex_capture {
            let end_slash_offset = ctx
                .remaining_src()
                .find_test(|c, i| {
                    c == '/' && ctx.remaining_src().chars().nth(i - 1).unwrap() != '\\'
                })
                .expect("end slash not found")
                + 2;
            let regex_src = &ctx.remaining_src()[2..end_slash_offset - 1];
            ctx.inc_loc(end_slash_offset);
            ctx.skip_whitespace();
            // XXX: handle bad regex panic
            return Some(Ast::Capture {
                pattern: Some(Regex::new(regex_src).expect("illegal regular expression")),
                name: None,
            });
        } else if is_named_capture {
            let next_space_offset = ctx.cur_token_end();
            let name = &ctx.remaining_src()[1..next_space_offset];
            ctx.inc_loc(next_space_offset);
            ctx.skip_whitespace();
            return Some(Ast::Capture {
                pattern: None,
                name: Some(name),
            });
        } else if is_anonymous_capture {
            ctx.inc_loc(1);
            ctx.skip_whitespace();
            return Some(Ast::Capture {
                pattern: None,
                name: None,
            });
        } else {
            panic!("should be unreachable! a caller didn't check matchers::is_capture first")
        }
    }

    pub(super) fn scope_prop<'a>(ctx: &ParseContext<'a>) -> Option<(&'a str, PropValue<'a>)> {}

    pub(super) fn scope_expr<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
        let mut props = HashMap::new();
        let mut capture = None;
        while matchers::is_scope_prop(&ctx) {
            let (key, val) = try_parse::scope_prop(&ctx)
                .expect("checked it was a scope prop then parsed but it wasn't!");
            props.insert(key, val);
            if matchers::is_capture(&ctx) {
                capture = try_parse::capture(&ctx).map(|c| Box::new(c));
                break;
            }
        }
        let nester = try_parse::nesting_op(&ctx);
        Some(Ast::Elem {
            capture,
            nester,
            props,
        })
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
