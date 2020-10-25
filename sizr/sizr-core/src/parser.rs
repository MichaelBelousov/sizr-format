/**
 * Parser for the sizr ast transformation language
 */
extern crate regex;

use regex::Regex;
use std::boxed::Box;
use std::cell::Cell;
use std::collections::HashMap;
use std::option::Option;
use std::vec::Vec;

// NOTE: I will add a tokenize function and rewrite this crud in
// terms of tokens when rust adds stable coroutines/generators

#[derive(Debug)]
struct ParseContext<'a> {
    pub src: &'a str,
    // TODO: consider alternatives to cell
    pub loc: Cell<usize>,
}

impl<'a> ParseContext<'a> {
    // WHY aren't these functions pub?
    fn new(in_src: &'a str) -> Self {
        ParseContext {
            src: in_src,
            loc: Cell::new(0),
        }
    }

    fn remaining_src(&self) -> &'a str {
        &self.src[self.loc.get()..]
    }

    fn inc_loc(&self, inc: usize) -> usize {
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

#[derive(Debug, PartialEq)]
pub(crate) enum NestingType {
    Arg,    // (
    Next,   // ,
    Member, // .
    Impl,   // {
}

impl NestingType {
    fn from_str(s: &str) -> Option<NestingType> {
        // there's probably an Option method that can remove the double Some (filter+map)
        match s.chars().nth(0) {
            Some('(') => Some(NestingType::Arg),
            Some(',') => Some(NestingType::Next),
            Some('.') => Some(NestingType::Member),
            Some('{') => Some(NestingType::Impl),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum TransformType {
    Additive,  // >>>
    Replacing, // >>!
}

impl TransformType {
    // XXX: this matches for example ">>>@#$^$#", which should be unexpected
    // and yet I rely on it else where, should change
    fn from_str(s: &str) -> Option<TransformType> {
        if s.starts_with(">>>") { Some(TransformType::Additive) }
        else if s.starts_with(">>!") { Some(TransformType::Replacing) }
        else { None }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum PropValue<'a> {
    Boolean(bool),
    #[allow(dead_code)]
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
    Capture {
        name: Option<&'a str>,
        // TODO: probably need a Pattern enum containing regex or str or any
        pattern: Option<Regex>,
    },
}

// TODO: see if there's a way to only need to specify equivalence for Option<Regex>
impl<'a> PartialEq for Ast<'a> {
    fn eq(&self, other: &Self) -> bool {
        use Ast::*;
        match (self, other) {
            (Transform{selector: lsel, r#type: ltype, assertor: lasser},
             Transform{selector: rsel, r#type: rtype, assertor: rasser})
              => lsel == rsel && ltype == rtype && lasser == rasser,
            (Query{path: lpath}, Query{path: rpath})
              => lpath == rpath,
            (Elem{capture: lcap, nester: lnest, props: lprops},
             Elem{capture: rcap, nester: rnest, props: rprops})
              => lcap == rcap && lnest == rnest && lprops == rprops,
            (Capture{name: lname, pattern: lpat},
             Capture{name: rname, pattern: rpat})
              => lname == rname && match (lpat, rpat) {
                  (Some(l), Some(r)) => l.as_str() == r.as_str(),
                  (None, None) => true,
                  _ => false
              },
            _ => false
        }
    }
}

// probably ought to replace this in favor of just running try_parse and checking for None
pub mod matchers {
    use super::*;

    // tokenizer would remove the complexity here
    pub(super) fn is_eof(ctx: &ParseContext) -> bool {
        ctx.remaining_src().len() == 0 || ctx.remaining_src().find(|c: char| !c.is_whitespace()).is_none()
    }

    pub(super) fn is_nesting_op(ctx: &ParseContext) -> bool {
        NestingType::from_str(&ctx.remaining_src()).is_some()
    }

    pub(super) fn is_transform_op(ctx: &ParseContext) -> bool {
        TransformType::from_str(&ctx.remaining_src()).is_some()
    }

    pub(super) fn is_capture<'a>(ctx: &ParseContext<'a>) -> bool {
        ctx.remaining_src().chars().nth(0).map(|c| c == '$').unwrap_or(false)
    }

    pub(super) fn is_scope_prop<'a>(ctx: &ParseContext<'a>) -> bool {
        ctx.remaining_src()
            .chars()
            .nth(0)
            .map(|c| c.is_ascii_alphabetic() || c == '!')
            .unwrap_or(false)
    }

    #[allow(dead_code)]
    pub(super) fn is_query<'a>(ctx: &ParseContext<'a>) -> bool {
        is_scope_prop(&ctx) || is_capture(&ctx)
    }

    #[allow(dead_code)]
    pub(super) fn is_scope_expr<'a>(ctx: &ParseContext<'a>) -> bool {
        is_query(&ctx)
    }
}

// TODO: move to a separate module?
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
        // XXX: incorrect on multi-byte chars because str.find returns byte offset,
        // this is character offset
        for (i, c) in self.chars().enumerate() {
            if f(c, i) {
                return Some(i);
            }
        }
        return None;
    }
}

// TODO: switch to using Result over Option, so we don't need to panic!
// would be nice if you could associate a function with an enum variant
// like Ast::Transform::parse() but alas
pub mod try_parse {
    use super::*;

    pub(super) fn nesting_op(ctx: &ParseContext) -> Option<NestingType> {
        let result = NestingType::from_str(&ctx.remaining_src());
        if result.is_some() {
            ctx.inc_loc(1);
            ctx.skip_whitespace();
        }
        result
    }

    pub(super) fn transform_op(ctx: &ParseContext) -> Option<TransformType> {
        let result = TransformType::from_str(&ctx.remaining_src());
        if result.is_some() {
            ctx.inc_loc(3);
            ctx.skip_whitespace();
        }
        result
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
        let is_anonymous_capture = ctx.remaining_src().chars().nth(0).map(|c| c == '$').unwrap_or(false) && !is_named_capture;
        if is_regex_capture {
            let end_slash_offset = ctx
                .remaining_src()[2..]
                .find_test(|c, i| c == '/' && &ctx.remaining_src()[i - 1..i] != "\\")
                .expect("end slash not found")
                + 2;
            let regex_src = &ctx.remaining_src()[2..end_slash_offset];
            ctx.inc_loc(end_slash_offset+1);
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

    pub(super) fn value<'a>(ctx: &ParseContext<'a>) -> Option<PropValue<'a>> {
        let is_quote = ctx.remaining_src().chars().nth(0).map(|c| c == '"').unwrap_or(false);
        // should probably expect nth(0) since otherwise we're at EOF
        let is_num = ctx.remaining_src().chars().nth(0).map(|c| c.is_numeric()).unwrap_or(false);
        if is_quote {
            let end_quote_offset = ctx.remaining_src().find_test(|c, i| c == '"' && &ctx.remaining_src()[i-1..i] != "\\").expect("unterminated quote") + 1;
            ctx.inc_loc(end_quote_offset);
            ctx.skip_whitespace();
            Some(PropValue::String(&ctx.remaining_src()[1..end_quote_offset-1]))
        } else {
            let next_space_offset = ctx.cur_token_end();
            let sentence = &ctx.remaining_src()[..next_space_offset];
            ctx.inc_loc(next_space_offset);
            ctx.skip_whitespace();
            if is_num {
                use std::str::FromStr;
                let num = f64::from_str(sentence).expect("bad number literal");
                Some(PropValue::Real(num))
            } else {
                Some(PropValue::String(sentence))
            }
        }
    }

    pub(super) fn scope_prop<'a>(ctx: &ParseContext<'a>) -> Option<(&'a str, PropValue<'a>)> {
        let after_token = ctx.cur_token_end();
        let if_boolean_sentence = &ctx.remaining_src()[..after_token];
        let is_boolean_no_prop = ctx.remaining_src().chars().nth(0).map(|c| c == '!').unwrap_or(false);
        if is_boolean_no_prop {
            ctx.inc_loc(if_boolean_sentence.len());
            ctx.skip_whitespace();
            return Some((&if_boolean_sentence[1..], PropValue::Boolean(false)));
        }
        let is_specific_value_prop = if_boolean_sentence.contains("=");
        let is_boolean_yes_prop = !is_specific_value_prop;
        if is_boolean_yes_prop {
            ctx.inc_loc(if_boolean_sentence.len());
            ctx.skip_whitespace();
            return Some((&if_boolean_sentence, PropValue::Boolean(true)));
        } else if is_specific_value_prop {
            let value_delim_index = ctx.remaining_src().find("=").expect("was supposed to have '='");
            let key = &ctx.remaining_src()[..value_delim_index];
            ctx.inc_loc(value_delim_index + 1);
            ctx.skip_whitespace();
            let value = try_parse::value(ctx).expect("expected prop value");
            return Some((key, value));
        }
        None
    }

    pub(super) fn scope_expr<'a>(ctx: &ParseContext<'a>) -> Option<Ast<'a>> {
        if !matchers::is_scope_expr(&ctx) {
            return None;
        }
        let mut props = HashMap::new();
        let mut capture = None;
        while matchers::is_scope_prop(&ctx) {
            let (key, val) = try_parse::scope_prop(&ctx)
                .expect("checked it was a scope prop then parsed but it wasn't!");
            // switching to tokenizer ought to help clean this up weird bit
            // where the parsed scope_expr might actually have been an explicit name
            if matchers::is_nesting_op(&ctx) || matchers::is_transform_op(&ctx) || matchers::is_eof(&ctx) {
                // last parsed scope_prop was actually a capture, set the capture and break before adding the prop
                capture = Some(Box::new(Ast::Capture {
                    name: Some(key),
                    pattern: Some(Regex::new(key).expect("unreachable: identifiers are always valid regex"))
                }));
                break;
            }
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
        Some(Ast::Transform {
            selector: match maybe_selector {
                Some(selector) => Some(Box::new(selector)),
                _ => None,
            },
            r#type: transform_op,
            assertor: match maybe_assertor {
                Some(assertor) => Some(Box::new(assertor)),
                _ => None,
            },
        })
    }
}

pub(crate) fn parse_command<'a>(src: &'a str) -> Ast<'a> {
    let ctx = ParseContext::new(src);
    let result = try_parse::transform(&ctx);
    result.expect("invalid transform")
}

// TODO: see about moving companion test module to a separate file
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_full_command() {
        assert_eq!(
            parse_command("class $c . !func $/log_.*/ >>! range=5.02 item"),
            Ast::Transform{
                selector: Some(Box::new(Ast::Query{
                    path: vec![
                        Box::new(Ast::Elem{
                            capture: Some(Box::new(Ast::Capture{
                                name: Some("c"),
                                pattern: None,
                            })),
                            nester: Some(NestingType::Member),
                            props: {
                                let mut map = HashMap::new();
                                map.insert("class", PropValue::Boolean(true));
                                map
                            }
                        }),
                        Box::new(Ast::Elem{
                            capture: Some(Box::new(Ast::Capture{
                                name: None,
                                pattern: Some(Regex::new("log_.*").unwrap())
                            })),
                            nester: None,
                            props: {
                                let mut map = HashMap::new();
                                map.insert("func", PropValue::Boolean(false));
                                map
                            }
                        })
                    ]
                })),
                r#type: TransformType::Replacing,
                assertor: Some(Box::new(Ast::Query{
                    path: vec![
                        Box::new(Ast::Elem{
                            capture: Some(Box::new(Ast::Capture{
                                name: Some("item"),
                                pattern: Some(Regex::new("item").unwrap())
                            })),
                            nester: None,
                            props: {
                                let mut map = HashMap::new();
                                map.insert("range", PropValue::Real(5.02));
                                map
                            }
                        })
                    ]
                })),
            }
        );
    }
}

