/**
 * Parser for the sizr-format language
 */
use lazy_static::lazy_static;
use num_derive::FromPrimitive;
use std::boxed::Box;
use std::cell::Cell;
use std::collections::HashMap;
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

    pub fn make_further_test_ctx(&self, advance: usize) -> Self {
        let result = ParseContext {
            src: self.src,
            loc: self.loc.clone(),
        };
        result.inc_loc(advance);
        result
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

    // really this should be done over "Iterator::skip_while" and ParserContext should
    // be an interator, no?
    pub fn skip_whitespace(&self) {
        if let Some(jump) = &self.remaining_src().find(|c: char| !c.is_whitespace()) {
            &self.inc_loc(*jump);
        } else {
            self.loc.set(self.src.len());
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

    #[rustfmt::skip]
    #[allow(dead_code)]
    #[derive(Debug)]
    pub enum BinOp { And, Or, Xor, Gt, Gte, Eq, Neq, Lte, Lt, Add, Sub, Mul, Div, Idiv, Mod, Pow, Dot, }

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

#[rustfmt::skip]
#[allow(dead_code)]
#[derive(Debug)]
pub enum Token<'a> {
    Reference(&'a str),
    Literal(Literal<'a>),
    LBrace, RBrace, LBrack, RBrack, Gt, Lt, Pipe, BSlash, FSlash, Plus, Minus, Asterisk, Ampersand, Dot, Caret, At, Hash, Exclaim, Tilde,
    LtEq, GtEq, EqEq, NotEq, 
    IndentMark(IndentMark<'a>),
    Comment(&'a str),
}

impl<'a> Token<'a> {
    //pub fn tokens(stream: &str) -> impl Iterator<Self> {}

    fn try_lex_indent_mark(src: &str) -> Option<IndentMark> {
        lazy_static! {
            static ref INDENT_MARK_PATTERN: regex::Regex =
                // this is why I didn't want to do regex... maybe I'll rewrite this part later
                regex::Regex::new(r#"^(\|>+)|(<+\|)|(>[1-9][0-9]*)|(>"[^"\\]*(?:\\.[^"\\]*)*)"#)
                    .expect("INDENT_MARK_PATTERN regex failed to compile");
        }
        let capture = INDENT_MARK_PATTERN
            .captures(&src)
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
            .flatten();

        return capture.and_then(|(i, capture)| {
            use std::convert::TryInto;
            let len = capture.range().len();
            let len_u16: u16 = len
                .try_into()
                .expect("expected in/outdent jump of less than 2^16");
            match i {
                1 => Some(IndentMark::Indent(len_u16 - 1)),
                2 => Some(IndentMark::Outdent(len_u16 - 1)),
                3 => {
                    let number = capture.as_str()[1..]
                        .parse::<u16>()
                        .expect("failed to parse a 16-bit unsigned integer in a numeric anchor");
                    Some(IndentMark::NumericAnchor(number))
                    // TODO: double check this rust feature
                }
                4 => {
                    // XXX: might be off by a byte... should write a test
                    let content = &capture.as_str()[2..capture.end() - 1];
                    Some(IndentMark::TokenAnchor(content))
                }
                _ => unreachable!(),
            }
        });
    }

    fn next_token(stream: &'a str) -> Result<Self, &'static str> {
        Self::try_lex_indent_mark(stream)
            .map(|im| Token::IndentMark(im))
            .ok_or("unknown token")
            .or_else(|err| lex::string_literal(stream).map(|s| Token::Literal(Literal::String(s))))
            .or_else(|err|
            // TODO: staircase match is slow or awkward, there ought to be a better way to test for string matches (maybe even a good regex)
            match &stream[0..=2] {
                "<=" => Ok(Token::LtEq),
                ">=" => Ok(Token::GtEq),
                "==" => Ok(Token::EqEq),
                "!=" => Ok(Token::NotEq), 
                _ => match &stream[0..=1] {
                    "{" => Ok(Token::LBrace),
                    "}" => Ok(Token::RBrace),
                    "[" => Ok(Token::LBrack),
                    "]" => Ok(Token::RBrack),
                    ">" => Ok(Token::Gt),
                    "<" => Ok(Token::Lt),
                    "|" => Ok(Token::Pipe),
                    "&" => Ok(Token::Ampersand),
                    "." => Ok(Token::Dot),
                    "^" => Ok(Token::Caret),
                    "@" => Ok(Token::At),
                    "#" => Ok(Token::Hash),
                    "/" => Ok(Token::FSlash),
                    r"\" => Ok(Token::BSlash),
                    "+" => Ok(Token::Plus),
                    "-" => Ok(Token::Minus),
                    "*" => Ok(Token::Asterisk),
                    "!" => Ok(Token::Exclaim),
                    "~" => Ok(Token::Tilde),
                }
            })
    }
}

#[macro_use]
pub mod lex {
    use super::*;

    pub fn static_string<F: FnOnce(&char) -> bool>(
        stream: &str,
        kind: &'static str,
        expect_msg: &'static str,
        test_is_after: F,
    ) -> Result<Read<()>, &'static str> {
        stream
            .starts_with(kind)
            .then(|| ())
            .and(
                stream
                    .chars()
                    .nth(kind.len())
                    .filter(test_is_after)
                    .and(Some(Read::new((), kind.len()))),
            )
            .ok_or(expect_msg)
    }

    #[macro_export]
    macro_rules! lex_keyword {
        ($ctx: expr, $keyword: expr) => {{
            lex::static_string(
                $ctx,
                $keyword,
                concat!("expected keyword '", $keyword, "'"),
                |c: &char| !c.is_ascii_alphabetic(),
            )
        }};
    }

    // NEEDWORK: too simple, i.e. can't tell the difference between = and == tokens
    #[macro_export]
    macro_rules! lex_operator {
        ($ctx: expr, $operator: expr) => {{
            lex::static_string(
                $ctx,
                $operator,
                concat!("expected operator '", $operator, "'"),
                |_| true,
            )
        }};
    }

    macro_rules! lex_escapable_delimited_string {
        // src: &str, delimiter: char, name: expr
        ($src: expr, $delimiter: expr, $literal_name: expr) => {{
            try_read_chars(
                $src,
                |i, c, next| match (i, c, next) {
                    (_, _, None) => Err(concat!("unterminated ", $literal_name, " literal")),
                    (0, $delimiter, _) => Ok(false),
                    (0, _, _) => Err(concat!(
                        $literal_name,
                        " literals must start with the delimiter '",
                        $delimiter,
                        "'"
                    )),
                    (i, c, _) if c == $delimiter && &$src[i - 1..=i - 1] != "\\" => Ok(true),
                    _ => Ok(false),
                },
                |s| Ok(s),
            )
        }};
    }

    pub fn string_literal<'a>(src: &'a str) -> Result<&'a str, &'static str> {
        lex_escapable_delimited_string!(src, '"', "string")
    }

    pub fn regex_literal<'a>(src: &'a str) -> Result<&'a str, &'static str> {
        lex_escapable_delimited_string!(src, '"', "regex")
    }

    pub fn name<'a>(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        fn is_name_start_char(c: char) -> bool {
            c.is_ascii_alphabetic() || c == '_'
        }
        fn is_name_char(c: char) -> bool {
            c.is_ascii_alphanumeric() || c == '_'
        }
        try_read_chars(
            &ctx.remaining_src(),
            |i, c, next| match (i, c, next) {
                (0, c, _) if !is_name_start_char(c) => Err("identifiers must start with /[a-z_]/"),
                (_, c, _) if !is_name_char(c) => Err("identifiers must contain with /[a-z_]/"),
                (_, _, Some(next)) => Ok(!is_name_char(next)),
                (_, _, None) => Ok(true),
            },
            |s| Ok(Read::new(FilterExpr::Name(s), s.len())),
        )
    }

    pub fn node_reference<'a>(ctx: &'a ParseContext) -> Result<Read<&'a str>, &'static str> {
        (ctx.remaining_src().starts_with("$"))
            .then(|| ())
            .ok_or("node references must start with a '$'")
            .and(lex::name(&ctx.make_further_test_ctx(1)))
            .map(|name| Read::new(&ctx.remaining_src()[..name.len + 1], name.len + 1))
    }
}

pub trait Parseable<'a, T> {
    fn try_parse(ctx: &'a ParseContext) -> Result<Read<T>, &'static str>;
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
    fn try_parse(ctx: &'a ParseContext) -> Result<Read<IndentMark<'a>>, &'static str> {
        lazy_static! {
            static ref INDENT_MARK_PATTERN: regex::Regex =
                // this is why I didn't want to do regex... maybe I'll rewrite this part later
                regex::Regex::new(r#"^(\|>+)|(<+\|)|(>[1-9][0-9]*)|(>"[^"\\]*(?:\\.[^"\\]*)*)"#)
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
fn try_read_chars<'a, IsLastChar, Map, Expr>(
    src: &'a str,
    is_last_char: IsLastChar,
    map_to_expr: Map,
) -> Result<Expr, &'static str>
where
    IsLastChar: Fn(usize, char, Option<char>) -> Result<bool, &'static str>,
    Map: FnOnce(&'a str) -> Result<Expr, &'static str>,
{
    src.chars()
        .nth(0)
        .ok_or("trying to start reading from EOF")
        .and(
            src.chars()
                .zip(src.chars().map(Some).skip(1).chain([None]))
                .enumerate()
                .find_map(|(i, (c, next))| {
                    let test = is_last_char(i, c, next);
                    // perhaps there's a better way to do this...
                    match test {
                        Ok(true) => Some(Ok(i)), // done
                        Ok(false) => None,       // not done, continue
                        Err(e) => Some(Err(e)),  // error, we're done
                    }
                })
                .unwrap_or(Err("failed to find")),
        )
        .and_then(|end| {
            let content = &src[..=end];
            map_to_expr(content)
        })
}

impl<'a> Literal<'a> {
    fn try_parse_integer(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            &ctx.remaining_src(),
            |i, c, next| match (i, c, next) {
                (0, c, _) if !matches!(c, '1'..='9') => {
                    Err("integers must start with a non-zero digit /[1-9]/")
                }
                (_, _, None) => Ok(true),
                (_, _, Some(next)) if !(next.is_ascii_digit()) => Ok(true),
                _ => Ok(false),
            },
            |s| {
                s.parse::<i64>()
                    // NEEDSWORK: combine the parse error rather than swallow it?
                    .map_err(|_err| "expected integer literal")
                    .map(|i| Read::new(Literal::Integer(i), s.len()))
            },
        )
    }

    fn try_parse_float(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        try_read_chars(
            &ctx.remaining_src(),
            |i, c, next| match (i, c, next) {
                (0, c, _) if !matches!(c, '1'..='9') => {
                    Err("floats must start with a non-zero digit /[1-9]/")
                }
                (_, _, None) => Ok(true),
                (_, _, Some(next)) if !(next.is_ascii_digit() || next == '.') => Ok(true),
                _ => Ok(false),
            },
            |s| {
                s.parse::<f64>()
                    // NEEDSWORK: combine the parse error rather than swallow it?
                    .map_err(|_err| "expected float literal")
                    .map(|f| Read::new(Literal::Float(f), s.len()))
            },
        )
    }

    fn try_parse_string(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        lex::string_literal(ctx.remaining_src())
            .map(|s| Read::new(Literal::String(&s[1..s.len() - 1]), s.len()))
    }

    fn try_parse_regex(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        // TODO: dedup with lex::string which uses try_read_chars similarly
        try_read_chars(
            &ctx.remaining_src(),
            |i, c, next| match (i, c, next) {
                (_, _, None) => Err("unterminated regex literal"),
                (0, '/', _) => Ok(false),
                (0, _, _) => Err("regex must start with a slash '/'"),
                (i, '/', _) if &ctx.remaining_src()[i - 1..i] == "\\" => Ok(false),
                (_, '/', _) => Ok(true),
                _ => Ok(false),
            },
            |s| {
                regex::Regex::new(s)
                    // NEEDSWORK: should combine with the regex failure message
                    .map_err(|_err| "invalid regex didn't compile")
                    .map(|r| Read::new(Literal::Regex(Regex::new(r)), s.len()))
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
    fn try_parse(ctx: &'a ParseContext) -> Result<Read<Literal<'a>>, &'static str> {
        Self::try_parse_integer(&ctx)
            .or_else(|_| Self::try_parse_float(&ctx)) // TODO: consider combining integer and float parsing
            .or_else(|_| Self::try_parse_string(&ctx))
            .or_else(|_| Self::try_parse_regex(&ctx))
            .or_else(|_| Self::try_parse_boolean(&ctx))
            .map_err(|_| "expected literal") // TODO: consider creating some kind of `Rope` of &str to make compounding errors possible
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

impl<'a> FilterExpr<'a> {
    fn try_parse_rest(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        // TODO: prefer read
        if ctx.remaining_src().starts_with("...") {
            Ok(Read::new(FilterExpr::Rest, "...".len()))
        } else {
            Err("expected rest filter '...'")
        }
    }
    fn try_parse_binop(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        unimplemented!()
    }
    fn try_parse_unaryop(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        unimplemented!()
    }
    fn try_parse_node_reference(
        ctx: &'a ParseContext,
    ) -> Result<Read<FilterExpr<'a>>, &'static str> {
        lex::node_reference(&ctx)
            .map(|r| r.map(|text| FilterExpr::NodeReference { name: &text[1..] }, 0))
    }
    fn try_parse_literal(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        unimplemented!()
    }
    fn try_parse_group(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        unimplemented!()
    }
    fn try_parse_name(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        unimplemented!()
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

impl<'a> Parseable<'a, FilterExpr<'a>> for FilterExpr<'a> {
    #[allow(dead_code)]
    fn try_parse(ctx: &'a ParseContext) -> Result<Read<FilterExpr<'a>>, &'static str> {
        Self::try_parse_rest(&ctx)
            .or_else(|_| Self::try_parse_binop(&ctx))
            .or_else(|_| Self::try_parse_unaryop(&ctx))
            .or_else(|_| Self::try_parse_node_reference(&ctx))
            .or_else(|_| Self::try_parse_literal(&ctx))
            .or_else(|_| Self::try_parse_group(&ctx))
            .or_else(|_| Self::try_parse_name(&ctx))
            .map_err(|_| "expected filter expr")
    }
}

// consider a better name
#[derive(Debug)]
pub enum WriteCommand<'a> {
    Raw(String),
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

fn unescape_newlines(s: &str) -> String {
    s.replace(r"\n", "\n")
}

impl<'a> WriteCommand<'a> {
    pub fn unwrap_raw(self) -> String {
        match self {
            WriteCommand::Raw(content) => content,
            _ => panic!("tried to unwrap a raw"),
        }
    }

    pub fn unwrap_node_reference(self) -> (&'a str, Vec<FilterExpr<'a>>) {
        match self {
            WriteCommand::NodeReference { name, filters } => (name, filters),
            _ => panic!("tried to unwrap a WriteCommand that wasn't a node reference"),
        }
    }

    pub fn unwrap_conditional(
        self,
    ) -> (
        FilterExpr<'a>,
        Option<WriteCommand<'a>>,
        Option<WriteCommand<'a>>,
    ) {
        match self {
            WriteCommand::Conditional { test, then, r#else } => {
                (test, then.map(|o| *o), r#else.map(|o| *o))
            }
            _ => panic!("tried to unwrap a WriteCommand that wasn't a Conditional"),
        }
    }

    pub fn unwrap_indent_mark(self) -> IndentMark<'a> {
        match self {
            WriteCommand::IndentMark(indent_mark) => indent_mark,
            _ => panic!("tried to unwrap a WriteCommand as an indent mark but it wasn't one"),
        }
    }

    pub fn unwrap_sequence(self) -> Vec<WriteCommand<'a>> {
        match self {
            WriteCommand::Sequence(write_commands) => write_commands,
            _ => panic!("tried to unwrap a WriteCommand as a sequence but it wasn't one"),
        }
    }

    fn try_parse_raw(ctx: &'a ParseContext) -> Result<Read<WriteCommand<'a>>, &'static str> {
        lex::string_literal(ctx.remaining_src()).map(|s| {
            Read::new(
                WriteCommand::Raw(unescape_newlines(&s[1..s.len() - 1])),
                s.len(),
            )
        })
    }

    fn try_parse_node_reference(
        ctx: &'a ParseContext,
    ) -> Result<Read<WriteCommand<'a>>, &'static str> {
        lex::node_reference(&ctx).map(|r| {
            r.map(
                |text| WriteCommand::NodeReference {
                    name: &text[1..],
                    filters: Vec::new(),
                },
                0,
            )
        })
    }

    fn try_parse_wrap_point(ctx: &'a ParseContext) -> Result<Read<WriteCommand<'a>>, &'static str> {
        ctx.remaining_src()
            .starts_with("\\")
            .then(|| Read::new(WriteCommand::WrapPoint, 1))
            .ok_or("expected wrap point '\\'")
    }

    fn try_parse_conditional(
        ctx: &'a ParseContext,
    ) -> Result<Read<WriteCommand<'a>>, &'static str> {
        unimplemented!()
    }

    fn try_parse_indent_mark(
        ctx: &'a ParseContext,
    ) -> Result<Read<WriteCommand<'a>>, &'static str> {
        IndentMark::try_parse(ctx)
            .map(|result| result.map(|read| WriteCommand::IndentMark(read), 0))
    }

    fn try_parse_sequence(ctx: &'a ParseContext) -> Result<Read<WriteCommand<'a>>, &'static str> {
        fn try_parse_sequence_start(ctx: &ParseContext) -> Result<Read<()>, &'static str> {
            lex_operator!(ctx.remaining_src(), "{")
        }
        fn try_parse_sequence_end(ctx: &ParseContext) -> Result<Read<()>, &'static str> {
            lex_operator!(ctx.remaining_src(), "}")
        }

        let mut seq = Vec::<WriteCommand<'a>>::new();
        // FIXME: need to get rid of all of my borrowing of already borrowed values...
        ctx.consume_read_and_space(try_parse_sequence_start(ctx)?);
        while try_parse_sequence_end(ctx).is_err() {
            seq.push(ctx.consume_read_and_space(Self::try_parse(ctx)?));
        }
        ctx.consume_read_and_space(try_parse_sequence_end(ctx)?);
        return Ok(Read::new(WriteCommand::Sequence(seq), 0));
    }

    fn try_parse_atom(ctx: &'a ParseContext) -> Result<Read<WriteCommand<'a>>, &'static str> {
        Self::try_parse_raw(ctx)
            .or_else(|_| Self::try_parse_node_reference(ctx))
            .or_else(|_| Self::try_parse_wrap_point(ctx))
            .or_else(|_| Self::try_parse_conditional(ctx))
            .or_else(|_| Self::try_parse_indent_mark(ctx))
            .or_else(|_| Self::try_parse_sequence(ctx))
            .map_err(|_| "expected atomic expression")
    }
}

impl<'a> Parseable<'a, WriteCommand<'a>> for WriteCommand<'a> {
    fn try_parse(ctx: &'a ParseContext) -> Result<Read<WriteCommand<'a>>, &'static str> {
        Self::try_parse_raw(ctx)
            .or_else(|_| Self::try_parse_sequence(ctx))
            .or_else(|_| Self::try_parse_node_reference(ctx))
            .or_else(|_| Self::try_parse_wrap_point(ctx))
            .or_else(|_| Self::try_parse_indent_mark(ctx))
            .or_else(|_| Self::try_parse_conditional(ctx)) // this is placeholder, I'll need some real parsing
            .map_err(|_| "expected write command") // TODO: consider creating some kind of `Rope` of &str to make compounding error strings easy
    }
}

#[derive(Debug)]
pub struct Node<'a> {
    pub name: &'a str,
    pub commands: WriteCommand<'a>,
}

#[derive(Debug)]
pub struct File<'a> {
    pub nodes: HashMap<&'a str, WriteCommand<'a>>,
}

// NEXT: need to add the ability for a Read to be partially consumed so that larger syntax structures that are in process of being made can be vomited back
// used to be in try_parse, probably belongs in some module...
#[derive(Debug)]
pub struct Read<T> {
    result: T,
    pub len: usize,
}

impl<T> Read<T> {
    pub fn new(result: T, len: usize) -> Self {
        Read { result, len }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F, added_len: usize) -> Read<U> {
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

fn parse_file<'a>(ctx: &'a ParseContext) -> Result<File<'a>, &'static str> {
    let mut file = File {
        nodes: HashMap::new(),
    };
    while !ctx.at_eof() {
        let node_decl = parse_node_decl(ctx)?;
        file.nodes.insert(node_decl.name, node_decl.commands);
    }
    return Ok(file);
}

fn parse_node_decl<'a>(ctx: &'a ParseContext) -> Result<Node<'a>, &'static str> {
    ctx.skip_whitespace();
    ctx.consume_read_and_space(lex_keyword!(ctx.remaining_src(), "node")?);
    if cfg!(debug_assertions) {
        println!(
            "after read node keyword remaining: '{}'",
            ctx.remaining_src()
        );
    }
    let name = ctx.consume_read_and_space(
        lex::string_literal(ctx.remaining_src()).map(|s| Read::new(&s[1..s.len() - 1], s.len()))?,
    );
    if cfg!(debug_assertions) {
        println!("name: {:#?}", name);
        println!("after read name remaining: '{}'", ctx.remaining_src());
    }
    ctx.consume_read_and_space(lex_operator!(ctx.remaining_src(), "=")?);
    if cfg!(debug_assertions) {
        println!("after read '=' remaining: '{}'", ctx.remaining_src());
    }
    let commands = ctx.consume_read_and_space(WriteCommand::try_parse(&ctx)?);
    Ok(Node { name, commands })
}

pub(crate) fn parse_text<'a>(ctx: &'a ParseContext) -> Result<File<'a>, &'static str> {
    parse_file(&ctx)
}
