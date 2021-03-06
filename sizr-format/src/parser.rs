/**
 * Parser for the sizr-format language
 */

extern crate regex;
extern crate lazy_static;

use std::vec::Vec;
use std::boxed::Box;
use std::cell::Cell;
use std::option::Option;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;


#[derive(Debug)]
pub enum Ast<'a> {
    Indent,
    Outdent,
    Align(Option<regex::Regex>),
    Identifier(&'a str),
    Quote(&'a str),
    Regex(regex::Regex),
    Number(f64),
    Variable{ name: &'a str },
    Lambda{property: &'a str, equals: Option<Box<Ast<'a>>>},
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
    BinaryOp{
        left: Box<Ast<'a>>,
        right: Box<Ast<'a>>,
        op: &'static ops::BinOpDef
    },
    UnaryOp{
        op: &'static ops::UnaryOpDef,
        inner: Box<Ast<'a>>
    },
}

// TODO: make private to this module
#[derive(Debug)]
pub struct ParseContext<'a> {
    pub src: &'a str,
    // TODO: replace with mutex?
    pub loc: Cell<usize>,
}

#[derive(Debug)]
pub enum Token<'a>
{
    LPar,
    RPar,
    Indent,
    Outdent,
    Align(Option<regex::Regex>),
    WrapPoint,
    Identifier(&'a str),
    Number(f64),
    Quote(&'a str),
    Regex(regex::Regex),
    Variable{ name: &'a str },
    // TODO: rename to lambdaStart or something
    SimpleLambda{property: &'a str},
    Op(&'a str),
}

impl<'a> PartialEq for Token<'a> {
    fn eq (&self, other: &Self) -> bool {
        use Token::*;
        match (self, other) {
            (LPar, LPar) => true,
            (RPar, RPar) => true,
            (Indent, Indent) => true,
            (Outdent, Outdent) => true,
            (Align(None), Align(None)) => true,
            (Align(Some(left_regex)), Align(Some(right_regex)))
            | (Regex(left_regex), Regex(right_regex))
                => left_regex.as_str() == right_regex.as_str(),
            (Align(_), Align(_)) => false,
            (WrapPoint, WrapPoint) => true,
            (Identifier(l), Identifier(r)) => l == r,
            (Number(l), Number(r)) => l == r,
            (Quote(l), Quote(r)) => l == r,
            (Variable{name: lname}, Variable{name: rname}) => lname == rname,
            (SimpleLambda{property: lprop}, SimpleLambda{property: rprop}) => lprop == rprop,
            (Op(l), Op(r)) => l == r,
            _ => false
        }
    }
}

impl<'a> ParseContext<'a> {
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

    pub fn new(in_src: &'a str) -> Self {
        ParseContext{
            src: in_src,
            loc: Cell::new(0),
        }
    }

    pub fn next_token(&self) -> Option<Token> {
        self.remaining_src().chars().nth(0).and_then(|c| match c {
            '(' => Some(Token::LPar),
            ')' => Some(Token::RPar),
            '\\' => Some(Token::WrapPoint),
            '"' => Some(atoms::parse_quote(self)),
            _ => None
        })
        .or(atoms::try_lex_variable(self))
        .or(atoms::try_lex_indent_ctx_decl(self))
        .or(atoms::try_lex_op(self))
    }
}

pub mod matcher {
    use super::*;

    pub fn identifier(ctx: &ParseContext) -> bool {
        if let Some(c) = ctx.remaining_src().chars().nth(0) {
            c.is_ascii_alphabetic()
        } else { false }
    }

    pub fn number(ctx: &ParseContext) -> bool {
        if let Some(c) = ctx.remaining_src().chars().nth(0) {
            c.is_ascii_digit()
        } else { false }
    }

    pub fn quote(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('"')
    }

    pub fn regex(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('/')
    }

    pub fn eol(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('\n')
    }

    pub fn eof(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == None
    }

    pub fn lambda(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('.')
    }

    pub fn cond(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('?')
    }

    pub fn variable(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('$')
    }

    pub fn wrap_point(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('\\')
    }

    pub fn indent_ctx_decl(ctx: &ParseContext) -> bool {
        match &ctx.src[ctx.loc.get()..ctx.loc.get()+2] {
            "|>" | ">/" | "<|" | ">|"  => true,
            _ => false
        }
    }
}

fn skip_to_char(ctx: &ParseContext, to: char) {
    match &ctx.remaining_src().find(|c| c == to) {
        Some(jump) => ctx.inc_loc(*jump),
        None => panic!("EOI")
    };
}

fn skip_char(ctx: &ParseContext, to: char) {
    skip_to_char(ctx, to);
    ctx.inc_loc(1);
}

fn skip_whitespace(ctx: &ParseContext) {
    match &ctx.remaining_src().find(|c: char| !c.is_whitespace()) {
        Some(jump) => ctx.inc_loc(*jump),
        None => panic!("reached EOI")
    };
}

pub mod atoms {
    use super::*;

    pub fn read_identifier<'a>(source: &'a str) -> &'a str {
        // TODO: verify first char is not numeric in debug mode
        if let Some(after) = source.find(
            |c: char| !c.is_ascii_alphanumeric() && c != '_'
        ) {
            // TODO: convert escape sequences i.e. \n, \\, etc
            let content = &source[..after];
            content
        } else {
            panic!("debug");
        }
    }


    fn read_quoted<'a>(source: &'a str, delim: char) -> &'a str {
        let mut i = 1; //skip delimiter
        loop {
            if let Some(jump) = source.find(|c: char| c == '\\' || c == delim) {
                match source.chars().nth(jump) {
                    Some('\\')  => { i += jump + 2; },
                    Some(_) => { i += jump + 1; break; },
                    _ => panic!("error while reading delimited text")
                }
            } else {
                break;
            }
        }
        &source[1..i]
    }

    pub fn parse_number<'a>(ctx: &'a ParseContext) -> Token<'a> {
        // TODO: support scientific notation
        if let Some(end) =
            ctx.remaining_src().find(|c: char| !c.is_ascii_digit() && c != '.'
        ) {
            let src = &ctx.remaining_src()[..end];
            let parsed = src.parse::<f64>().unwrap();
            Token::Number(parsed)
        } else {
            panic!("failed to parse number");
        }
    }

    pub fn parse_quote<'a>(ctx: &'a ParseContext) -> Token<'a> {
        Token::Quote(read_quoted(ctx.remaining_src(), '"'))
    }

    // XXX: maybe shouldn't be an atom?
    pub fn try_lex_simple_lambda<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        // TODO: in debug mode explicitly check for "." start
        if let Some(c @ '.') = ctx.remaining_src().chars().nth(0) {
            ctx.inc_loc(1);
            let name = read_identifier(ctx.remaining_src());
            ctx.inc_loc(name.len());
            Some(Token::SimpleLambda{
                property: name
            })
        } else {
            None
        }
    }

    pub fn try_lex_variable<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        if let Some(c @ '$') = ctx.remaining_src().chars().nth(0) {
            ctx.inc_loc(1);
            let name = read_identifier(ctx.remaining_src());
            ctx.inc_loc(name.len());
            Some(Token::Variable{
                name: read_identifier(&ctx.remaining_src()[1..])
            })
        } else {
            None
        }
    }

    pub fn try_lex_indent_ctx_decl<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        match &ctx.remaining_src()[..2] {
            "|>" => Some(Token::Indent),
            ">|" => Some(Token::Align(None)),
            ">/" => Some(Token::Align(
                Some(
                    regex::Regex::new(
                        read_quoted(
                            ctx.remaining_src(),
                            '/'
                        )
                    ).unwrap()
                )
            )),
            "<|" => Some(Token::Outdent),
            _ => None
        }
    }

    pub fn try_lex_op<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        let end = ctx.remaining_src()
            // XXX: why can't the rust compiler infer the type?
            .find(|c: char| c.is_whitespace() || c.is_ascii_alphanumeric())?;
        // TODO: use separate pattern, not op definitions to match
        if let Some(op) = ops::UNARY_OPS
            .iter()
            .find(|op| op.symbol == &ctx.remaining_src()[..end])
        {
            return Some(Token::Op(op.symbol));
        }
        else
        {
            let op = ops::BINARY_OPS
                .iter()
                .find(|op| op.symbol == &ctx.remaining_src()[..end])?;
            return Some(Token::Op(op.symbol));
        }
    }

    pub fn parse<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        let tok = ctx.next_token().expect("unexpected end of input");
        match tok {
            Token::LPar => {
                let inner = exprs::parse(ctx);
                let next = ctx.next_token().expect("expected closing parenthesis, found EOI");
                if next != Token::RPar { panic!("expected closing parenthesis"); }
                Ast::Group(Box::new(inner))
            },
            Token::Op(symbol) => {
                let op = ops::UNARY_OPS
                    .iter()
                    .find(|op| op.symbol == symbol)
                    .expect("unexpected binary operator");
                let inner = exprs::parse(ctx);
                Ast::UnaryOp{op, inner: Box::new(inner)}
            },
            Token::Indent => Ast::Indent,
            Token::Outdent => Ast::Outdent,
            Token::Align(val) => Ast::Align(val),
            Token::WrapPoint => Ast::WrapPoint,
            Token::Identifier(val) => Ast::Identifier(val),
            Token::Number(val) => Ast::Number(val),
            Token::Quote(val) => Ast::Quote(val),
            Token::Regex(val) => Ast::Regex(val),
            Token::Variable{ name } => Ast::Variable{ name },
            Token::SimpleLambda{property} => {
                let next = ctx.next_token().expect("unexpected EOI while parsing lambda");
                if next == Token::Op("=") {
                    let equalsExpr = exprs::parse(ctx);
                    Ast::Lambda{
                        property,
                        equals: Some(Box::new(equalsExpr))
                    }
                } else {
                    Ast::Lambda{ property, equals: None }
                }
            },
            _ => panic!("unexpected token '{:?}', during atom parsing"),
        }
    }
}

//pub mod writes
pub mod exprs {
    use super::*;

    // TODO: support slices
    // TODO: support conditional write commands

    pub fn parse_lambda<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        // TODO: in debug mode explicitly check for "." start
        ctx.inc_loc(1);
        let name = atoms::read_identifier(ctx.remaining_src());
        ctx.inc_loc(name.len());
        Ast::Lambda{
            property: name,
            equals: match ctx.remaining_src().chars().nth(0) {
                Some('=') => {
                    let expr = exprs::parse(ctx);
                    Some(Box::new(expr))
                },
                Some(_) => None,
                _ => panic!("unexpected during lambda parsing")
            }
        }
    }

    pub fn parse_aux<'a>(ctx: &'a ParseContext, min_prec: i32) -> Ast<'a> {
        let mut lhs = atoms::parse(ctx);
        loop {
            let tok = ctx.next_token().expect("unexpected end of input");
            if let Token::Op(sym) = tok {
                let op = ops::BINARY_OPS.iter()
                    .find(|op| op.symbol == sym)
                    .expect("unexpected unary operator");
                let min_prec_as_enum = FromPrimitive::from_i32(min_prec)
                    .expect("programmer error: bad enum cast");
                if op.prec >= min_prec_as_enum {
                    let next_prec = op.prec as i32
                        + if op.assoc == ops::Assoc::Left {1} else {0};
                    let rhs = parse_aux(ctx, next_prec);
                    lhs = Ast::BinaryOp {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs)
                    };
                } else {
                    break;
                }
            } else {
                break
            }
        }
        lhs
    }

    pub fn parse<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        parse_aux(ctx, 0)
    }
}

// TODO: rename to like a "parse_NodeSet", parse_file sounds too high-level
fn parse_file(ctx: &ParseContext) {
    while ctx.loc.get() < ctx.src.len() {
        parse_format_def(ctx);
    }
}

fn parse_format_def(ctx: &ParseContext) {
    skip_whitespace(ctx);
    let name = atoms::read_identifier(ctx.remaining_src());
    ctx.inc_loc(name.len());
    skip_to_char(ctx, '\'');
    if let Some(end) = ctx.remaining_src().find(|c: char| c != '\'') {
        let delim = &(ctx.remaining_src()[..end]);
        ctx.inc_loc(end);
        while &ctx.remaining_src()[..end] != delim {
            skip_whitespace(ctx);
            exprs::parse(ctx);
        }
    } else {
        panic!("bad format definition syntax");
    }
}

pub mod ops {
    use super::*;

    #[derive(Debug, PartialEq, PartialOrd, FromPrimitive, Copy, Clone)]
    pub enum Prec {
        And = 0, Or, Comp, Add, Mult, Exp, Dot,
    }

    #[derive(Debug, PartialEq)]
    pub enum Assoc { Left, Right }

    #[derive(Debug)]
    pub struct UnaryOpDef {
        pub symbol: &'static str,
    }

    pub static NEG:  UnaryOpDef = UnaryOpDef{symbol: "-"};
    pub static COMP: UnaryOpDef = UnaryOpDef{symbol: "~"};
    pub static NOT:  UnaryOpDef = UnaryOpDef{symbol: "!"};

    pub static UNARY_OPS: [&UnaryOpDef; 3] = [&NEG, &COMP, &NOT];

    #[derive(Debug)]
    pub struct BinOpDef {
        pub symbol: &'static str,
        pub prec: Prec,
        pub assoc: Assoc,
    }

    pub static AND: BinOpDef =
        BinOpDef{symbol: "&",  prec: Prec::And, assoc: Assoc::Left};
    pub static OR:  BinOpDef =
        BinOpDef{symbol: "|",  prec: Prec::Or, assoc: Assoc::Left};
    pub static XOR: BinOpDef =
        BinOpDef{symbol: "^",  prec: Prec::Or, assoc: Assoc::Left};
    pub static GT:  BinOpDef =
        BinOpDef{symbol: ">",  prec: Prec::Comp, assoc: Assoc::Left};
    pub static GTE: BinOpDef =
        BinOpDef{symbol: ">=", prec: Prec::Comp, assoc: Assoc::Left};
    pub static EQ:  BinOpDef =
        BinOpDef{symbol: "=",  prec: Prec::Comp, assoc: Assoc::Left};
    pub static NEQ: BinOpDef =
        BinOpDef{symbol: "!=", prec: Prec::Comp, assoc: Assoc::Left};
    pub static LTE: BinOpDef =
        BinOpDef{symbol: "<=", prec: Prec::Comp, assoc: Assoc::Left};
    pub static LT:  BinOpDef =
        BinOpDef{symbol: "<",  prec: Prec::Comp, assoc: Assoc::Left};
    pub static ADD: BinOpDef =
        BinOpDef{symbol: "+",  prec: Prec::Add, assoc: Assoc::Left};
    pub static SUB: BinOpDef =
        BinOpDef{symbol: "-",  prec: Prec::Add, assoc: Assoc::Left};
    pub static MUL: BinOpDef =
        BinOpDef{symbol: "*",  prec: Prec::Mult, assoc: Assoc::Left};
    pub static DIV: BinOpDef =
        BinOpDef{symbol: "/",  prec: Prec::Mult, assoc: Assoc::Left};
    pub static IDIV: BinOpDef =
        BinOpDef{symbol: "//", prec: Prec::Mult, assoc: Assoc::Left};
    pub static MOD: BinOpDef =
        BinOpDef{symbol: "%",  prec: Prec::Mult, assoc: Assoc::Left};
    pub static POW: BinOpDef =
        BinOpDef{symbol: "**", prec: Prec::Exp, assoc: Assoc::Right};
    pub static DOT: BinOpDef =
        BinOpDef{symbol: ".",  prec: Prec::Dot, assoc: Assoc::Left};

    // TODO: sort by code points in a macro for binary searches
    pub static BINARY_OPS: [&BinOpDef; 17] = [
        &AND, &OR, &XOR,
        &GT, &GTE, &EQ, &NEQ, &LTE, &LT,
        &ADD, &SUB,
        &MUL, &DIV, &IDIV, &MOD,
        &POW,
        &DOT,
    ];
}

// need interior mutability... why can't I 
pub fn parse_text(text: &str) {
    let ctx = ParseContext::new(text);
    let ast = parse_file(&ctx);
    println!("{:?}", ast);
}

