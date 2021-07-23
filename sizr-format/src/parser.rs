//extern crate lazy_static;
/**
 * Parser for the sizr-format language
 */
extern crate regex;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::boxed::Box;
use std::cell::Cell;
//use std::collections::HashMap;
use std::option::Option;
use std::result::Result;
use std::vec::Vec;

// TODO: move to a separate util module
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

// TODO: make private to this module
#[derive(Debug)]
pub struct ParseContext<'a> {
    pub src: &'a str,
    // TODO: consider other types of cell
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

    pub fn distance_to_eof(&self) -> usize {
        self.src.len() - self.loc.get()
    }

    pub fn inc_loc(&self, amount: usize) -> usize {
        &self.loc.set(self.loc.get() + amount);
        self.loc.get()
    }

    // really this should be done over "Iterator::skip_while" and this whole thing should
    // be an interator, no?
    pub fn skip_whitespace(&self) {
        if let Some(jump) = &self.remaining_src().find(|c: char| !c.is_whitespace()) {
            &self.inc_loc(*jump);
        }
    }

    /**
     * return the distance from the current location to the end of the current token
     */
    pub fn cur_token_end(&self) -> usize {
        self.remaining_src()
            .find(|c: char| c.is_whitespace())
            .unwrap_or(self.remaining_src().len())
    }
}

pub mod ops {
    use super::*;

    #[derive(Debug, PartialEq, PartialOrd, FromPrimitive, Copy, Clone)]
    pub enum Prec {
        Or = 0,
        And,
        Eq,  // ==, !=
        Cmp, // <, >, <=, >=
        Add,
        Mult,
        Exp,
        Dot,
    }

    // XXX: replace with FromStr implementation
    pub trait FromToken {
        fn read<'a>(token: &'a str) -> Self;
    }

    #[derive(Debug, PartialEq)]
    pub enum Assoc {
        Left,
        Right,
    }

    #[derive(Debug)]
    pub enum UnaryOp {
        Negate,
        BitwiseComplement,
        LogicalComplement,
    }

    impl FromToken for UnaryOp {
        fn read<'a>(token: &'a str) -> Self {
            match token {
                "-" => UnaryOp::Negate,
                "~" => UnaryOp::BitwiseComplement,
                "!" => UnaryOp::LogicalComplement,
            }
        }
    }

    pub trait HasAssoc {
        fn assoc<'a>(&self) -> Assoc;
    }

    pub trait HasPrec {
        fn prec<'a>(&self) -> Prec;
    }

    #[derive(Debug)]
    pub enum BinOp {
        And,
        Or,
        Xor,
        Gt,
        Gte,
        Eq,
        Neq,
        Lte,
        Lt,
        Add,
        Sub,
        Mul,
        Div,
        Idiv,
        Mod,
        Pow,
        Dot,
    }

    impl HasAssoc for BinOp {
        fn assoc<'a>(&self) -> Assoc {
            match self {
                BinOp::Pow => Assoc::Right,
                _ => Assoc::Left,
            }
        }
    }

    impl HasPrec for BinOp {
        fn prec<'a>(&self) -> Prec {
            match self {
                BinOp::Or | BinOp::Xor => Prec::Or,
                BinOp::And => Prec::And,
                BinOp::Gt | BinOp::Gte | BinOp::Lte | BinOp::Lt => Prec::Cmp,
                BinOp::Eq | BinOp::Neq => Prec::Eq,
                BinOp::Add | BinOp::Sub => Prec::Add,
                BinOp::Mul | BinOp::Div | BinOp::Idiv | BinOp::Mod => Prec::Add,
                BinOp::Pow => Prec::Exp,
                BinOp::Dot => Prec::Dot,
            }
        }
    }
}

// NOTE: rename to Anchor, or Aligner?
#[derive(Debug)]
pub(crate) enum IndentMark<'a> {
    Indent(u16),          // |>
    Outdent(u16),         // <|
    TokenAnchor(&'a str), // >'"'
    NumericAnchor(u16),   // >10
}

#[derive(Debug)]
pub struct Regex {
    pub regex: regex::Regex,
}

impl Regex {
    pub fn new(val: regex::Regex) -> Self {
        Regex { regex: val }
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str()
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Literal<'a> {
    Boolean(bool),
    String(&'a str),
    Regex(Regex),
    Integer(i64),
    Float(f64),
}

#[derive(Debug)]
pub(crate) enum FilterExpr<'a> {
    Rest,
    BinOp {
        op: ops::BinOp,
        left: Box<FilterExpr<'a>>,
        right: Box<FilterExpr<'a>>,
    },
    UnaryOp {
        op: ops::UnaryOp,
        expr: Box<FilterExpr<'a>>,
    },
    NodeReference {
        name: &'a str,
    },
    Literal(Literal<'a>),
    Group(Box<FilterExpr<'a>>),
    Name(&'a str),
}

// consider a better name
#[derive(Debug)]
pub(crate) enum WriteCommand<'a> {
    Raw(&'a str),
    Node {
        name: &'a str,
        filters: Vec<FilterExpr<'a>>, // comma separated
    },
    Break, // aka WrapPoint, might be a better name
    Conditional {
        test: FilterExpr<'a>,
        then: Option<Box<WriteCommand<'a>>>,
        r#else: Option<Box<WriteCommand<'a>>>,
    },
    IndentMark(IndentMark<'a>),
    Sequence(Vec<WriteCommand<'a>>),
}

#[derive(Debug)]
pub struct Node<'a> {
    pub name: &'a str,
    pub commands: Vec<WriteCommand<'a>>,
}

pub struct File<'a> {
    pub nodes: Vec<Node<'a>>,
}

pub mod try_parse {
    use super::*;

    pub struct Read<T> {
        pub result: T,
        pub len: usize,
    }

    impl<T> Read<T> {
        pub fn new(result: T, len: usize) -> Self {
            Read { result, len }
        }
    }

    // TODO: create an arg struct for this
    fn try_read_chars<'a, FindEnd, Map, Expr>(
        ctx: &'a ParseContext,
        continueReading: FindEnd,
        mapToExpr: Map,
    ) -> Result<Read<Expr>, &'static str>
    where
        FindEnd: Fn(char, usize, bool) -> Result<bool, &'static str>, // Result is whether to stop
        Map: FnOnce(&'a str) -> Result<Expr, &'static str>,
    {
        ctx.remaining_src()
            .chars()
            .nth(0)
            .ok_or("trying to start reading from EOF")
            // find end of token
            .and(
                ctx.remaining_src()
                    .chars()
                    .enumerate()
                    // this should be find_last, maybe scan will be correct?
                    .find_map(|(i, c)| {
                        let test = continueReading(c, i, false);
                        // how do you do Option::filter on Result????
                        match test {
                            Ok(v @ true) => None,             // continue
                            Ok(v @ false) => Some(Ok(i - 1)), // we're done, the previous char was the last one
                            Err(e) => Some(Err(e)),           // error, we're done
                        }
                    })
                    .unwrap_or(Err("failed to find")),
            )
            .or(continueReading('\0', ctx.remaining_src().len(), true)
                .and(Ok(ctx.distance_to_eof())))
            .and_then(|end| {
                let content = &ctx.remaining_src()[..end];
                mapToExpr(content).map(|expr| (expr, content.len()))
            })
            .map(|(expr, len)| Read::<Expr>::new(expr, len))
    }

    pub(super) fn name<'a>(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, eof) => Ok(true),
                (c, 0, _) if c.is_ascii_alphabetic() || c == '_' => Ok(true),
                (c, 0, _) => Err("identifiers must start with /[a-z_]/"),
                (c, i, _) if c.is_ascii_alphanumeric() || c == '_' => Ok(true),
                _ => Ok(false),
            },
            |s| Ok(FilterExpr::Name(s)),
        )
    }

    pub(super) fn integer<'a>(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, eof) => Ok(true),
                (c, 0, _) if c.is_ascii_digit() => Ok(true),
                (c, 0, _) => Err("numbers must start with a digit /[0-9]/"),
                (c, i, _) if c.is_ascii_digit() || c == '.' => Ok(true),
                _ => Ok(false),
            },
            |s| {
                s.parse::<i64>()
                    // NEEDSWORK: combine the parse error rather than swallow it?
                    .map_err(|err| "expected integer literal")
                    .map(|i| Literal::Integer(i))
            },
        )
    }

    pub(super) fn string<'a>(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, eof) => Err("unterminated string literal"),
                (c @ '"', 0, _) => Ok(true),
                (c, 0, _) => Err("strings must start with a quote '\"'"),
                (c, i, _)
                    if &ctx.remaining_src()[i - 1..i] != "\""
                        && &ctx.remaining_src()[i - 2..i] != "\\" =>
                {
                    Ok(false)
                }
                _ => Ok(true),
            },
            |s| Ok(Literal::String(s)),
        )
    }

    pub(super) fn regex<'a>(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, eof) => Err("unterminated regex literal"),
                (c @ '/', 0, _) => Ok(true),
                (c, 0, _) => Err("regex must start with a slash '/'"),
                (c, i, _)
                    if &ctx.remaining_src()[i - 1..i] != "/"
                        && &ctx.remaining_src()[i - 2..i] != "\\" =>
                {
                    Ok(false)
                }
                _ => Ok(true),
            },
            |s| {
                regex::Regex::new(s)
                    // NEEDSWORK: should combine with the regex failure message
                    .map_err(|err| "invalid regex didn't compile")
                    .map(|r| Literal::Regex(Regex::new(r)))
            },
        )
    }

    pub(super) fn cond(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('?')
    }

    pub(super) fn node_ref(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('$')
    }

    pub(super) fn wrap_point(ctx: &ParseContext) -> bool {
        ctx.remaining_src().chars().nth(0) == Some('\\')
    }

    pub(super) fn indent_mark(ctx: &ParseContext) -> bool {
        &ctx.src[ctx.loc.get()..ctx.loc.get()] == ">"
            || match &ctx.src[ctx.loc.get()..ctx.loc.get() + 2] {
                "|>" | "<|" | ">|" => true,
                _ => false,
            }
    }
}

pub mod try_parse {
    use super::*;

    pub fn identifier<'a>(source: &'a str) -> &'a str {
        // TODO: verify first char is not numeric in debug mode
        if let Some(after) = source.find(|c: char| !c.is_ascii_alphanumeric() && c != '_') {
            // TODO: convert escape sequences i.e. \n, \\, etc
            let content = &source[..after];
            content
        } else {
            panic!("debug");
        }
    }

    fn quoted<'a>(source: &'a str, delim: char) -> &'a str {
        let mut i = 1; //skip delimiter
        loop {
            if let Some(jump) = source.find(|c: char| c == '\\' || c == delim) {
                match source.chars().nth(jump) {
                    Some('\\') => {
                        i += jump + 2;
                    }
                    Some(_) => {
                        i += jump + 1;
                        break;
                    }
                    _ => panic!("error while reading delimited text"),
                }
            } else {
                break;
            }
        }
        &source[1..i]
    }

    pub fn number<'a>(ctx: &'a ParseContext) -> Token<'a> {
        // TODO: support scientific notation
        if let Some(end) = ctx
            .remaining_src()
            .find(|c: char| !c.is_ascii_digit() && c != '.')
        {
            let src = &ctx.remaining_src()[..end];
            let parsed = src.parse::<f64>().unwrap();
            Token::Number(parsed)
        } else {
            panic!("failed to parse number");
        }
    }

    pub fn quote<'a>(ctx: &'a ParseContext) -> Token<'a> {
        Token::Quote(read_quoted(ctx.remaining_src(), '"'))
    }

    // XXX: maybe shouldn't be an atom?
    pub fn try_lex_simple_lambda<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        // TODO: in debug mode explicitly check for "." start
        if let Some(c @ '.') = ctx.remaining_src().chars().nth(0) {
            ctx.inc_loc(1);
            let name = read_identifier(ctx.remaining_src());
            ctx.inc_loc(name.len());
            Some(Token::SimpleLambda { property: name })
        } else {
            None
        }
    }

    pub fn try_lex_variable<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        if let Some(c @ '$') = ctx.remaining_src().chars().nth(0) {
            ctx.inc_loc(1);
            let name = read_identifier(ctx.remaining_src());
            ctx.inc_loc(name.len());
            Some(Token::Variable {
                name: read_identifier(&ctx.remaining_src()[1..]),
            })
        } else {
            None
        }
    }

    pub fn try_lex_indent_mark<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        match &ctx.remaining_src()[..2] {
            "|>" => Some(Token::Indent),
            ">|" => Some(Token::Align(None)),
            ">/" => Some(Token::Align(Some(
                regex::Regex::new(read_quoted(ctx.remaining_src(), '/')).unwrap(),
            ))),
            "<|" => Some(Token::Outdent),
            _ => None,
        }
    }

    pub fn try_lex_op<'a>(ctx: &'a ParseContext) -> Option<Token<'a>> {
        let end = ctx
            .remaining_src()
            // XXX: why can't the rust compiler infer the type?
            .find(|c: char| c.is_whitespace() || c.is_ascii_alphanumeric())?;
        // TODO: use separate pattern, not op definitions to match
        if let Some(op) = ops::UNARY_OPS
            .iter()
            .find(|op| op.symbol == &ctx.remaining_src()[..end])
        {
            return Some(Token::Op(op.symbol));
        } else {
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
                let next = ctx
                    .next_token()
                    .expect("expected closing parenthesis, found EOI");
                if next != Token::RPar {
                    panic!("expected closing parenthesis");
                }
                Ast::Group(Box::new(inner))
            }
            Token::Op(symbol) => {
                let op = ops::UNARY_OPS
                    .iter()
                    .find(|op| op.symbol == symbol)
                    .expect("unexpected binary operator");
                let inner = exprs::parse(ctx);
                Ast::UnaryOp {
                    op,
                    inner: Box::new(inner),
                }
            }
            Token::Indent => Ast::Indent,
            Token::Outdent => Ast::Outdent,
            Token::Align(val) => Ast::Align(val),
            Token::WrapPoint => Ast::WrapPoint,
            Token::Identifier(val) => Ast::Identifier(val),
            Token::Number(val) => Ast::Number(val),
            Token::Quote(val) => Ast::Quote(val),
            Token::Regex(val) => Ast::Regex(val),
            Token::Variable { name } => Ast::Variable { name },
            Token::SimpleLambda { property } => {
                let next = ctx
                    .next_token()
                    .expect("unexpected EOI while parsing lambda");
                if next == Token::Op("=") {
                    let equalsExpr = exprs::parse(ctx);
                    Ast::Lambda {
                        property,
                        equals: Some(Box::new(equalsExpr)),
                    }
                } else {
                    Ast::Lambda {
                        property,
                        equals: None,
                    }
                }
            }
            _ => panic!("unexpected token '{:?}', during atom parsing"),
        }
    }
}

pub mod exprs {
    use super::*;

    // TODO: support slices
    // TODO: support conditional write commands

    pub fn parse_lambda<'a>(ctx: &'a ParseContext) -> Ast<'a> {
        // TODO: in debug mode explicitly check for "." start
        ctx.inc_loc(1);
        let name = atoms::read_identifier(ctx.remaining_src());
        ctx.inc_loc(name.len());
        Ast::Lambda {
            property: name,
            equals: match ctx.remaining_src().chars().nth(0) {
                Some('=') => {
                    let expr = exprs::parse(ctx);
                    Some(Box::new(expr))
                }
                Some(_) => None,
                _ => panic!("unexpected during lambda parsing"),
            },
        }
    }

    pub fn parse_aux<'a>(ctx: &'a ParseContext, min_prec: i32) -> Ast<'a> {
        let mut lhs = atoms::parse(ctx);
        loop {
            let tok = ctx.next_token().expect("unexpected end of input");
            if let Token::Op(sym) = tok {
                let op = ops::BINARY_OPS
                    .iter()
                    .find(|op| op.symbol == sym)
                    .expect("unexpected unary operator");
                let min_prec_as_enum =
                    FromPrimitive::from_i32(min_prec).expect("programmer error: bad enum cast");
                if op.prec >= min_prec_as_enum {
                    let next_prec =
                        op.prec as i32 + if op.assoc == ops::Assoc::Left { 1 } else { 0 };
                    let rhs = parse_aux(ctx, next_prec);
                    lhs = Ast::BinaryOp {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    };
                } else {
                    break;
                }
            } else {
                break;
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

// need interior mutability... why can't I
pub fn parse_text(text: &str) {
    let ctx = ParseContext::new(text);
    let ast = parse_file(&ctx);
    println!("{:?}", ast);
}
