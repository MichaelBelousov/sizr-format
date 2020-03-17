/**
 * Parser for the sizr-format language
 */

extern crate regex;
extern crate lazy_static;

use std::vec::Vec;
use std::boxed::Box;
use std::cell::{Cell, RefCell};


#[derive(Debug)]
pub enum Ast<'a> {
    Indent,
    Outdent,
    Align(Option<regex::Regex>),
    Add(Box<Ast<'a>>, Box<Ast<'a>>),
    Quote(&'a str),
    Regex(regex::Regex),
    Number(f64),
    Variable{ name: &'a str },
    Lambda{ property: &'a str, equals: Option<Box<Ast<'a>>> },
    Underscore,
    Group(Box<Ast<'a>>),
    Cond{
        cond: Box<Ast<'a>>, // Group
        then: Option<Box<Ast<'a>>>,
        else_: Option<Box<Ast<'a>>>
    },
    WrapPoint,
    NodeFormat(Vec<Box<Ast<'a>>>),
    File(Vec<Ast<'a>>),
}

#[derive(Debug)]
struct ParseContext<'a> {
    pub src: &'a str,
    pub loc.get(): Cell<usize>,
}

impl<'a> ParseContext<'a> {
    fn remainingSrc(&self) -> &'a str {
        &self.src[self.loc.get()..]
    }

    fn incLoc(&self, inc: usize) -> usize {
        /** FIXME: find idiomatic rust solution for this stuff */
        &self.loc.set(loc.get() + inc);
        &self.loc
    }
}

pub mod matcher {
    use super::*;

    fn identifier(ctx: &ParseContext) -> bool {
        if let Some(c) = ctx.remainingSrc().chars().nth(0) {
            c.is_ascii_alphabetic()
        } else { false }
    }

    fn number(ctx: &ParseContext) -> bool {
        if let Some(c) = ctx.remainingSrc().chars().nth(0) {
            c.is_ascii_digit()
        } else { false }
    }

    fn quote(ctx: &ParseContext) -> bool {
        ctx.remainingSrc().chars().nth(0) == Some('"')
    }

    fn regex(ctx: &ParseContext) -> bool {
        ctx.remainingSrc().chars().nth(0) == Some('/')
    }

    fn eol(ctx: &ParseContext) -> bool {
        ctx.remainingSrc().chars().nth(0) == Some('\n')
    }

    fn eof(ctx: &ParseContext) -> bool {
        ctx.remainingSrc().chars().nth(0) == None
    }

    fn lambda(ctx: &ParseContext) -> bool {
        ctx.remainingSrc().chars().nth(0) == Some('.')
    }

    fn binary_op(ctx: &ParseContext) -> bool {
        if let Some(end) = ctx.remainingSrc().find(
            |c| c.is_whitespace() || c.is_ascii_alphanumeric()
        ) {
            ops::BIN_OP_PRECEDENCE_MAP.has(ctx.remainingSrc()[..end]);
        } else { false }
    }

    fn unary_op(ctx: &ParseContext) -> bool {
        match ctx.remainingSrc().chars().nth(0) {
            Some('-') | Some('~') | Some('!') => true,
            None => false
        }
    }
}

fn skipToChar(ctx: &ParseContext, to: char) {
    match &ctx.remainingSrc().find(|c| c == to) {
        Some(jump) => ctx.incLoc(jump),
        None => panic!("EOI")
    }
}

fn skipChar(ctx: &ParseContext, to: char) {
    skipToChar(ctx, to);
    ctx.incLoc(1);
}

fn skipWhitespace(ctx: &ParseContext) {
    match &ctx.remainingSrc().find(|c: char| !c.is_whitespace()) {
        Some(jump) => ctx.incLoc(jump),
        None => panic!("reached EOI")
    }
}

pub mod atoms {
    use super::*;

    // NOTE: maybe take immutable context ref?
    pub fn readIdentifier<'a>(ctx: &'a ParseContext) -> &'a str {
        // TODO: verify first char is not numeric in debug mode
        if let Some(after) = ctx.remainingSrc().find(
            |c: char| !c.is_ascii_alphanumeric() && c != '_'
        ) {
            // TODO: convert escape sequences i.e. \n, \\, etc
            let content = &ctx.remainingSrc()[..after];
            ctx.incLoc(after);
            content
        } else {
            panic!("debug");
        }
    }


    fn readQuoted<'a>(ctx: &'a ParseContext, delim: char) -> &'a str {
        // TODO: in debug mode check explicitly for delimiter match
        ctx.incLoc(1); //skip delimiter
        let start = ctx.loc.get();
        loop {
            match ctx.remainingSrc().find(|c: char| c == '\\' || c == delim) {
                Some(jump) => {
                    match &ctx.remainingSrc().chars().nth(jump) {
                        Some('\\')  => { ctx.incLoc(jump + 2); },
                        Some(delim) => { ctx.incLoc(jump + 1); break; },
                        _ => panic!("unreachable")
                    }
                },
                None => break
            }
        }
        &ctx.src[start..ctx.loc.get()-1]
    }

    pub fn parseNumber<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        // TODO: support scientific notation
        if let Some(end) =
            ctx.remainingSrc().find(|c: char| !c.is_ascii_digit() && c != '.'
        ) {
            let src = &ctx.remainingSrc()[..end];
            let parsed = src.parse::<f64>().unwrap();
            Ast::Number(parsed)
        } else {
            panic!("failed to parse number");
        }
    }

    pub fn parseQuote<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        Ast::Quote(readQuoted(ctx, '"'))
    }

    pub fn parseRegex<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        Ast::Regex(regex::Regex::new(readQuoted(ctx, '/')).unwrap())
    }

    pub fn parseParenGroup<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        ctx.incLoc(1); //skip opener
        // TODO: in debug mode check explicitly for delimiter match
        let expr = exprs::parseExpression(ctx);
        ctx.incLoc(1); //skip closer
        // TODO: in debug mode check explicitly for delimiter match
        Ast::Group(Box::new(expr))
    }

    pub fn parseLambda<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        // TODO: in debug mode explicitly check for "." start
        ctx.incLoc(1);
        let name = readIdentifier(ctx);
        Ast::Lambda{
            property: name,
            equals: match ctx.remainingSrc().chars().nth(0) {
                Some(c @ '=') => {
                    let expr = exprs::parseExpression(ctx);
                    Some(Box::new(expr))
                },
                Some(_) => None,
                _ => panic!("unexpected during lambda parsing")
            }
        }
    }

    // TODO: allow wrap points to form implicit conditions
    pub fn parseWrapPoint<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        ctx.incLoc(1);
        Ast::WrapPoint
    }

    pub fn parseVariable<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        ctx.incLoc(1);
        Ast::Variable{ name: readIdentifier(ctx) }
    }
}

//pub mod writes
pub mod exprs {
    use super::*;

    pub fn parseCond<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        ctx.incLoc(1); //skip "?"
        skipWhitespace(ctx);
        let cond = Box::new(atoms::parseParenGroup(ctx));
        skipWhitespace(ctx);
        if ctx.remainingSrc().chars().nth(0) == Some(':') {
            Ast::Cond {
                cond,
                then: None,
                else_: Some(Box::new(parseExpression(ctx)))
            }
        } else {
            let then = Some(Box::new(parseExpression(ctx)));
            skipChar(ctx, ':');
            skipWhitespace(ctx);
            let else_ = Some(Box::new(parseExpression(ctx)));
            Ast::Cond { cond, then, else_ }
        }
    }

    // parseExpression?
    pub fn parseExpression<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        match &ctx.remainingSrc().chars().nth(0) {
            Some(c) => match c {
                '"'  => atoms::parseQuote(ctx),
                '\\' => atoms::parseWrapPoint(ctx),
                '?'  => parseCond(ctx),
                '$'  => atoms::parseVariable(ctx),
                _ => panic!("Unknown token, expected write command")
            },
            None => panic!("Unknown token, expected write command")
        }
    }
}



pub fn parseFile(ctx: &ParseContext) {
    while ctx.loc.get() < ctx.src.len() {
        skipWhitespace(ctx);
        parseFormatDef(ctx);
    }
}

pub fn parseFormatDef(ctx: &ParseContext) {
    skipWhitespace(ctx);
    let name = atoms::readIdentifier(ctx);
    skipToChar(ctx, '\'');
    if let Some(idxAfterDelim) = ctx.remainingSrc().find(|c: char| c != '\'') {
        let delim = &(ctx.remainingSrc()[..idxAfterDelim]);
        while &ctx.remainingSrc()[..idxAfterDelim] != delim {
            skipWhitespace(ctx);
            exprs::parseExpression(ctx);
        }
    } else {
        panic!("bad format definition syntax");
    }
}

pub mod ops {
    use super::*;

    /*
    fn parseSlice(ctx: &ParseContext) {
    }
    */

    pub enum Precedence {
        Logic = 0, Comp, Add, Mult, Exp, Dot,
    }

    use std::collections::BTreeMap;

    lazy_static! {
        static ref BIN_OP_PRECEDENCE_MAP: BTreeMap<&'static str, Precedence> = {
            let mut m = BTreeMap::new();
            m.insert("&",  Precedence::Logic);
            m.insert("|",  Precedence::Logic);
            m.insert("^",  Precedence::Logic);
            m.insert(">",  Precedence::Comp);
            m.insert(">=", Precedence::Comp);
            m.insert("=",  Precedence::Comp);
            m.insert("!=", Precedence::Comp);
            m.insert("<=", Precedence::Comp);
            m.insert("<",  Precedence::Comp);
            m.insert("+",  Precedence::Add);
            m.insert("-",  Precedence::Add);
            m.insert("*",  Precedence::Mult);
            m.insert("/",  Precedence::Mult);
            m.insert("//", Precedence::Mult);
            m.insert("%",  Precedence::Mult);
            m.insert("**", Precedence::Exp);
            m.insert(".",  Precedence::Dot);
            m
        };
    }
}

// TODO: use precedence climbing for bin ops
pub fn parseBinOp(ctx: &ParseContext) {
  skipWhitespace(ctx);
  //parseAtom(ctx);
  //parseBinOp(ctx);
  //parseAtom(ctx);
  //while match ctx.remainingSrc() {
  //}
  // match () {
  // }
}

pub fn parseUnaryOp(ctx: &ParseContext) {
}

pub fn _parseExpression(ctx: &ParseContext) {
}

pub fn parseIndentCtxDecl<'a>(ctx: &'a ParseContext) -> Ast<'a> {
    match &ctx.src[ctx.loc.get()..ctx.loc.get()+2] {
        "|>" => { ctx.incLoc(2); Ast::Indent },
        ">/" => { ctx.incLoc(1); atoms::parseRegex(ctx) },
        "<|" => { ctx.incLoc(2); Ast::Outdent },
        _ => panic!("Unknown token, expected indentation context")
    }
}
