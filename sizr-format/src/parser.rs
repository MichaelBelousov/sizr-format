extern crate lazy_static;
/**
 * Parser for the sizr-format language
 */
extern crate regex;

use lazy_static::lazy_static;
use num_derive::FromPrimitive;
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

    pub fn at_eof(&self) -> bool {
        self.src.len() == self.loc.get()
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

    pub fn consume_read_and_space<T>(&self, read: Read<T>) -> T {
        self.inc_loc(read.len);
        self.skip_whitespace();
        return read.result;
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
                _ => unreachable!(),
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

pub mod try_parse {
    use super::*;

    #[allow(unused_macros)]
    macro_rules! static_string {
        ($ctx: expr, $string: expr, $expect_msg: expr, $test_is_after: expr) => {{
            (&$ctx.remaining_src()[..$string.len()] == $string)
                .then(|| ())
                .ok_or($expect_msg)
                .and(
                    $ctx.remaining_src()
                        .chars()
                        .nth($string.len())
                        .filter($test_is_after)
                        .ok_or($expect_msg) // NEEDSWORK
                        .and(Ok(Read::new((), $string.len()))),
                )
        }};
    }

    #[macro_export]
    macro_rules! try_parse_keyword {
        ($ctx: expr, $keyword: expr) => {{
            static_string!(
                $ctx,
                $keyword,
                concat!("expected keyword '", $keyword, "'"),
                |c: &char| !c.is_ascii_alphabetic()
            )
        }};
    }

    #[macro_export]
    macro_rules! try_parse_operator {
        ($ctx: expr, $operator: expr) => {{
            static_string!(
                $ctx,
                $operator,
                concat!("expected operator '", $operator, "'"),
                |c: &char| !c.is_ascii_punctuation()
            )
        }};
    }

    pub fn name<'a>(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, true) => Ok(true),
                (c, 0, _) if c.is_ascii_alphabetic() || c == '_' => Ok(true),
                (_, 0, _) => Err("identifiers must start with /[a-z_]/"),
                (c, _, _) if c.is_ascii_alphanumeric() || c == '_' => Ok(true),
                _ => Ok(false),
            },
            |s| Ok(FilterExpr::Name(s)),
        )
    }

    pub fn wrap_point<'a>(ctx: &ParseContext) -> Result<Read<WriteCommand<'a>>, &'static str> {
        ctx.remaining_src()
            .chars()
            .nth(0)
            .filter(|c| *c == '\\')
            .map(|_| Read::new(WriteCommand::WrapPoint, 1))
            .ok_or("expected wrap point '\\'")
    }

    pub fn node_reference<'a>(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        (&ctx.remaining_src().starts_with("$"))
            .then(|| ())
            .ok_or("node references must start with a '$'")
            .and(Ok(ctx.inc_loc(1)))
            .and(try_parse::name(&ctx))
            // damn lack of specific variant types...
            .map(|name_expr| match name_expr.result {
                FilterExpr::Name(name) => {
                    Read::new(FilterExpr::NodeReference { name }, 1 + name.len())
                }
                _ => unreachable!(),
            })
    }

    /*
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
    */
}

pub trait Parseable<'a, T> {
    fn try_parse(&self, ctx: &'a ParseContext) -> Result<Read<T>, &'static str>;
}

// NOTE: rename to Anchor, or Aligner?
#[derive(Debug)]
pub enum IndentMark<'a> {
    Indent(u16),          // |>
    Outdent(u16),         // <|
    TokenAnchor(&'a str), // >'"'
    NumericAnchor(u16),   // >10
}

impl<'a> Parseable<'a, IndentMark<'a>> for IndentMark<'a> {
    fn try_parse(&self, ctx: &'a ParseContext) -> Result<Read<IndentMark<'a>>, &'static str> {
        lazy_static! {
            static ref INDENT_MARK_PATTERN: regex::Regex =
                // this is why I didn't want to do regex... maybe I'll rewrite this part later
                regex::Regex::new(r#"^(\|>+)|(<+\|)|(>[1-9][0-9]*)|(>"[^"\\]*(?:\\.[^"\\]*)*"#)
                    .expect("INDENT_MARK_PATTERN regex failed to compile");
        }
        let capture = INDENT_MARK_PATTERN
            .captures(&ctx.remaining_src())
            .map(|captures| {
                captures
                    .iter()
                    .enumerate()
                    .skip(1) // skip the implicit total capture group
                    .find(|(_, capture)| capture.is_some())
                    .expect("INDENT_MARK_PATTERN capture groups are exclusive, one should match")
            })
            .map(|(i, capture)| match capture {
                Some(inner) => Some((i, inner)),
                None => None,
            })
            .flatten()
            .ok_or("expected indent mark");

        return capture.and_then(|(i, capture)| {
            use std::convert::TryInto;
            let len = capture.range().len();
            let len_u16: u16 = len
                .try_into()
                .expect("expected in/outdent jump of less than 2^16");
            match i {
                1 => Ok(Read::new(IndentMark::Indent(len_u16 - 1), len)),
                2 => Ok(Read::new(IndentMark::Outdent(len_u16 - 1), len)),
                3 => {
                    let number = capture.as_str()[1..]
                        .parse::<u16>()
                        .expect("failed to parse a 16-bit unsigned integer in a numeric anchor");
                    Ok(Read::new(IndentMark::NumericAnchor(number), len))
                    // TODO: double check this rust feature
                }
                4 => {
                    // XXX: might be off by a byte... should write a test
                    let content = &capture.as_str()[2..capture.end() - 1];
                    Ok(Read::new(IndentMark::TokenAnchor(content), len))
                }
                _ => unreachable!(),
            }
        });
    }
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
pub enum Literal<'a> {
    Boolean(bool),
    String(&'a str),
    Regex(Regex),
    Integer(i64),
    Float(f64),
}

// TODO: create an arg struct for this
fn try_read_chars<'a, FindEnd, Map, Expr>(
    ctx: &'a ParseContext,
    continue_reading: FindEnd,
    map_to_expr: Map,
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
                    let test = continue_reading(c, i, false);
                    // perhaps there's a better way to do this...
                    match test {
                        Ok(true) => None,             // continue
                        Ok(false) => Some(Ok(i - 1)), // we're done, the previous char was the last one
                        Err(e) => Some(Err(e)),       // error, we're done
                    }
                })
                .unwrap_or(Err("failed to find")),
        )
        .or(continue_reading('\0', ctx.remaining_src().len(), true).and(Ok(ctx.distance_to_eof())))
        .and_then(|end| {
            let content = &ctx.remaining_src()[..end];
            map_to_expr(content).map(|expr| (expr, content.len()))
        })
        .map(|(expr, len)| Read::<Expr>::new(expr, len))
}

impl<'a> Literal<'a> {
    fn try_parse_integer(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, true) => Ok(true),
                (c, 0, _) if c.is_ascii_digit() => Ok(true),
                (_, 0, _) => Err("numbers must start with a digit /[0-9]/"),
                (c, _, _) if c.is_ascii_digit() || c == '.' => Ok(true),
                _ => Ok(false),
            },
            |s| {
                s.parse::<i64>()
                    // NEEDSWORK: combine the parse error rather than swallow it?
                    .map_err(|_err| "expected integer literal")
                    .map(|i| Literal::Integer(i))
            },
        )
    }

    fn try_parse_string(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, true) => Err("unterminated string literal"),
                (c, 0, _) => (c == '"')
                    .then(|| true)
                    .ok_or("strings must start with a quote '\"'"),
                // NEEDSWORK: can be cleaned up probably
                (_, i, _)
                    if &ctx.remaining_src()[i - 1..i] == "\""
                        && &ctx.remaining_src()[i - 2..i] != "\\" =>
                {
                    Ok(false)
                }
                _ => Ok(true),
            },
            |s| Ok(Literal::String(s)),
        )
    }

    fn try_parse_regex(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            ctx,
            |c, i, eof| match (c, i, eof) {
                (_, _, true) => Err("unterminated regex literal"),
                ('/', 0, _) => Ok(true),
                (_, 0, _) => Err("regex must start with a slash '/'"),
                (_, i, _)
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
                    .map_err(|_err| "invalid regex didn't compile")
                    .map(|r| Literal::Regex(Regex::new(r)))
            },
        )
    }

    fn try_parse_boolean(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        if ctx.remaining_src().starts_with("true") {
            Ok(Read::new(Literal::Boolean(true), "true".len()))
        } else if ctx.remaining_src().starts_with("false") {
            Ok(Read::new(Literal::Boolean(false), "false".len()))
        } else {
            Err("expected boolean literal ('true' or 'false')")
        }
    }
}

impl<'a> Parseable<'a, Literal<'a>> for Literal<'a> {
    fn try_parse(&self, ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        Self::try_parse_integer(&ctx)
            .or(Self::try_parse_string(&ctx))
            .or(Self::try_parse_regex(&ctx))
            .map_err(|_| "expected literal") // TODO: consider creating some kind of `Rope` of &str
    }
}

#[derive(Debug)]
pub enum FilterExpr<'a> {
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
pub enum WriteCommand<'a> {
    Raw(&'a str),
    NodeReference {
        name: &'a str,
        filters: Vec<FilterExpr<'a>>, // comma separated
    },
    WrapPoint,
    Conditional {
        test: FilterExpr<'a>,
        then: Option<Box<WriteCommand<'a>>>,
        r#else: Option<Box<WriteCommand<'a>>>,
    },
    IndentMark(IndentMark<'a>),
    Sequence(Vec<WriteCommand<'a>>),
}

impl<'a> WriteCommand<'a> {
    fn unwrap_raw(&self) -> &'a str {
        match self {
            WriteCommand::Raw(content) => content,
            _ => panic!("tried to unwrap a raw"),
        }
    }

    // NOTE: this looks bad, need to figure out move semantics
    fn unwrap_node_reference(&self) -> (&'a str, Vec<FilterExpr<'a>>) {
        match self {
            WriteCommand::NodeReference { name, filters } => (name, *filters),
            _ => panic!("tried to unwrap a WriteCommand that wasn't a Raw"),
        }
    }

    fn unwrap_conditional(
        &self,
    ) -> (
        FilterExpr<'a>,
        Option<WriteCommand<'a>>,
        Option<WriteCommand<'a>>,
    ) {
        match self {
            WriteCommand::Conditional { test, then, r#else } => {
                (*test, then.map(|o| *o), r#else.map(|o| *o))
            }
            _ => panic!("tried to unwrap a WriteCommand that wasn't a Conditional"),
        }
    }

    fn unwrap_indent_mark(&self) -> IndentMark<'a> {
        match self {
            WriteCommand::IndentMark(indent_mark) => *indent_mark,
            _ => panic!("tried to unwrap a WriteCommand that wasn't an IndentMark"),
        }
    }

    fn unwrap_sequence(&self) -> Vec<WriteCommand<'a>> {
        match self {
            WriteCommand::Sequence(write_commands) => *write_commands,
            _ => panic!("tried to unwrap a WriteCommand that wasn't an IndentMark"),
        }
    }
}

impl<'a> Parseable<'a, WriteCommand<'a>> for WriteCommand<'a> {
    fn try_parse(ctx: &'a ParseContext) -> Result<Read<WriteCommand<'a>>, &'static str> {
        try_parse::string(&ctx)
            .map(|s| match s {
                Read {
                    result: Literal::String(content),
                    len,
                } => WriteCommand::Raw(content),
                _ => unreachable!(),
            })
            .or(try_parse::node_reference(&ctx))
            .map(|s| match s {
                Literal::String(content) => Raw(content),
                _ => unreachable!(),
            })
    }
}

#[derive(Debug)]
pub struct Node<'a> {
    pub name: &'a str,
    pub commands: Vec<WriteCommand<'a>>,
}

#[derive(Debug)]
pub struct File<'a> {
    pub nodes: Vec<Node<'a>>,
}

// used to be in try_parse
#[derive(Debug)]
pub struct Read<T> {
    pub result: T,
    pub len: usize,
}

impl<T> Read<T> {
    pub fn new(result: T, len: usize) -> Self {
        Read { result, len }
    }

    pub fn map<U, F: FnOnce(T) -> U>(&self, f: F, added_len: usize) -> Read<U> {
        Read::<U>::new(f(self.result), self.len + added_len)
    }
}

#[macro_use]
pub mod exprs {
    //use super::*;

    // TODO: support slices
    // TODO: support conditional write commands

    /*
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
    */
}

// TODO: rename to like a "parse_NodeSet", parse_file sounds too high-level
fn parse_file<'a>(ctx: &'a ParseContext) -> Result<File<'a>, &'static str> {
    let mut file = File { nodes: Vec::new() };
    while !ctx.at_eof() {
        file.nodes.push(parse_node_decl(ctx)?);
    }
    return Ok(file);
}

fn parse_node_decl<'a>(ctx: &'a ParseContext) -> Result<Node<'a>, &'static str> {
    ctx.skip_whitespace();
    ctx.consume_read_and_space(try_parse_keyword!(ctx, "node")?);
    let name = ctx.consume_read_and_space(try_parse::string(&ctx)?);
    ctx.consume_read_and_space(try_parse_operator!(ctx, "=")?);
    let commands = ctx.consume_read_and_space(try_parse::write_command(&ctx)?);
    // NOTE: maybe only tokens should have a "TokenRead" wrapper which is optionally consumable?
    match name {
        Literal::String(s) => Ok(Node { name: s, commands }),
        _ => unreachable!(),
    };
}

// need interior mutability... why can't I
pub fn parse_text(text: &str) {
    let ctx = ParseContext::new(text);
    if let Ok(ast) = parse_file(&ctx) {
        println!("{:?}", ast);
    }
}
