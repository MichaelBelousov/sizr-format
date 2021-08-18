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

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
}

impl ParseError {
    pub fn new(msg: &str) -> Self {
        ParseError {
            msg: String::from(msg),
        }
    }

    pub fn from_string(msg: String) -> Self {
        ParseError { msg }
    }

    pub fn err<T>(msg: &str) -> Result<T, Self> {
        Err(ParseError::new(msg))
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

    // maybe call "pump" or "propagate"... need a better name...
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

    pub fn inc_loc(&self, amount: usize) -> usize {
        &self.loc.set(self.loc.get() + amount);
        self.loc.get()
    }

    pub fn consume_read<T>(&self, read: Read<T>) -> T {
        self.inc_loc(read.len);
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

#[macro_use]
pub mod lex {
    use super::*;

    pub fn static_string<F: FnOnce(&char) -> bool>(
        stream: &str,
        kind: &'static str,
        expect_msg: ParseError,
        test_is_after: F,
    ) -> Result<Read<()>, ParseError> {
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
                ParseError::from_string(format!("expected keyword '{}", $keyword)),
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
                ParseError::from_string(format!("expected operator '{}", $operator)),
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
                    (_, _, None) => Err(ParseError::from_string(format!(
                        "unterminated {} literal",
                        $literal_name
                    ))),
                    (0, $delimiter, _) => Ok(false),
                    (0, _, _) => Err(ParseError::from_string(format!(
                        "{} literals must start with the delimiter '{}'",
                        $literal_name, $delimiter,
                    ))),
                    (i, c, _) if c == $delimiter && &$src[i - 1..=i - 1] != "\\" => Ok(true),
                    _ => Ok(false),
                },
                |s| Ok(s),
            )
        }};
    }

    pub fn string_literal<'a>(src: &'a str) -> Result<&'a str, ParseError> {
        lex_escapable_delimited_string!(src, '"', "string")
    }

    pub fn regex_literal<'a>(src: &'a str) -> Result<&'a str, ParseError> {
        lex_escapable_delimited_string!(src, '"', "regex")
    }

    pub fn name<'a>(src: &'a str) -> Result<Read<FilterExpr<'a>>, ParseError> {
        fn is_name_start_char(c: char) -> bool {
            c.is_ascii_alphabetic() || c == '_'
        }
        fn is_name_char(c: char) -> bool {
            c.is_ascii_alphanumeric() || c == '_'
        }
        try_read_chars(
            src,
            |i, c, next| match (i, c, next) {
                (0, c, _) if !is_name_start_char(c) => {
                    ParseError::err("identifiers must start with /[a-z_]/")
                }
                (_, c, _) if !is_name_char(c) => {
                    ParseError::err("identifiers must contain with /[a-z_]/")
                }
                (_, _, Some(next)) => Ok(!is_name_char(next)),
                (_, _, None) => Ok(true),
            },
            |s| Ok(Read::new(FilterExpr::Name(s), s.len())),
        )
    }

    pub fn node_reference<'a>(src: &'a str) -> Result<Read<&'a str>, ParseError> {
        src.starts_with("$")
            .then(|| ())
            .ok_or(ParseError::new("node references must start with a '$'"))
            .and(lex::name(src))
            .map(|name| Read::new(&src[..name.len + 1], name.len + 1))
    }
}

#[rustfmt::skip]
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    // literals
    Reference(&'a str),
    Literal(Literal<'a>),
    IndentMark(IndentMark<'a>),
    // symbols
    LBrace, RBrace, LBrack, RBrack, Gt, Lt, Eq, Pipe, BSlash, FSlash, Plus, Minus, Asterisk, Ampersand, Dot, Caret, At, Hash, Exclaim, Tilde,
    LtEq, GtEq, EqEq, NotEq, 
    // keywords
    Node,
    // special
    Eof,
    Unknown,
}

struct TokenIter<'a> {
    src: &'a str,
}

impl<'a> ParseContext<'a> {
    fn try_lex_indent_mark(src: &str) -> Option<Read<IndentMark>> {
        lazy_static! {
            static ref INDENT_MARK_PATTERN: regex::Regex =
                // this is why I didn't want to do regex... maybe I'll rewrite this part later
                regex::Regex::new(r#"^(?:(\|>+)|(<+\|)|(>[1-9][0-9]*)|(>"[^"\\]*(?:\\.[^"\\]*)*))"#)
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
            Some(Read::new(
                match i {
                    1 => IndentMark::Indent(len_u16 - 1),
                    2 => IndentMark::Outdent(len_u16 - 1),
                    3 => {
                        let number = capture.as_str()[1..].parse::<u16>().expect(
                            "failed to parse a 16-bit unsigned integer in a numeric anchor",
                        );
                        IndentMark::NumericAnchor(number)
                        // TODO: double check this rust feature
                    }
                    4 => {
                        // XXX: might be off by a byte... should write a test
                        let content = &capture.as_str()[2..capture.end() - 1];
                        IndentMark::TokenAnchor(content)
                    }
                    _ => unreachable!(),
                },
                len,
            ))
        });
    }

    fn skip_whitespace(src: &str) -> (&str, usize) {
        if let Some(jump) = src.find(|c: char| !c.is_whitespace()) {
            (&src[jump..], jump)
        } else {
            ("", src.len())
        }
    }

    fn skip_comments(src: &str) -> (&str, usize) {
        src.starts_with("#")
            .then(|| ())
            .and_then(|_| src.find("\n").map(|dist| (&src[dist..], dist)))
            .unwrap_or((src, 0))
    }

    fn next_token(&mut self) -> Result<Read<Token<'a>>, ParseError> {
        // TODO: fix aliasing
        let stream = self.src;
        let (stream, ws_skip_1) = Self::skip_whitespace(stream);
        let (stream, comment_skip) = Self::skip_comments(stream);
        let (stream, ws_skip_2) = Self::skip_whitespace(stream);
        let skipped = ws_skip_1 + comment_skip + ws_skip_2;
        Self::try_lex_indent_mark(stream)
            .map(|read| read.map(|im| Token::IndentMark(im), 0))
            .ok_or(ParseError::new("next_token: unknown token"))
            .or_else(|_err| {
                Literal::try_lex(stream).map(|read| read.map(|lit| Token::Literal(lit), 0))
            })
            .or_else(|_err| lex_keyword!(stream, "node").map(|_| Read::new(Token::Node, 4)))
            .or_else(|_err|
            // TODO: staircase match is slow or awkward, there ought to be a better way to test for string matches (maybe even regex)
            match &stream[0..=0] {
                "{" => Ok(Read::new(Token::LBrace, 1)),
                "}" => Ok(Read::new(Token::RBrace, 1)),
                "[" => Ok(Read::new(Token::LBrack, 1)),
                "]" => Ok(Read::new(Token::RBrack, 1)),
                ">" => match &stream[1..=1] {
                    "=" => Ok(Read::new(Token::GtEq, 2)),
                    _ => Ok(Read::new(Token::Gt, 1)),
                }
                "<" => match &stream[1..=1] {
                    "=" => Ok(Read::new(Token::LtEq, 2)),
                    _ => Ok(Read::new(Token::Lt, 1)),
                }
                "|" => Ok(Read::new(Token::Pipe, 1)),
                "&" => Ok(Read::new(Token::Ampersand, 1)),
                "." => Ok(Read::new(Token::Dot, 1)),
                "^" => Ok(Read::new(Token::Caret, 1)),
                "@" => Ok(Read::new(Token::At, 1)),
                "#" => Ok(Read::new(Token::Hash, 1)),
                "/" => Ok(Read::new(Token::FSlash, 1)),
                r"\" => Ok(Read::new(Token::BSlash, 1)),
                "+" => Ok(Read::new(Token::Plus, 1)),
                "-" => Ok(Read::new(Token::Minus, 1)),
                "*" => Ok(Read::new(Token::Asterisk, 1)),
                "!" => match &stream[1..=1] {
                    "=" => Ok(Read::new(Token::NotEq, 1)),
                    _ => Ok(Read::new(Token::Exclaim, 1)),
                }
                "=" => match &stream[1..=1] {
                    "=" => Ok(Read::new(Token::EqEq, 2)),
                    _ => Ok(Read::new(Token::Eq, 1)),
                }
                "~" => Ok(Read::new(Token::Tilde, 1)),
                // FIXME: should return Token::Invalid or keep Err?
                _ => ParseError::err("unknown token")
            })
            // add skipped whitespace to the read size
            .map(|read| read.map(|r| r, skipped))
    }

    pub fn tokenize(&mut self) -> &impl Iterator<Item = Result<Read<Token<'a>>, ParseError>> {
        self
    }

    pub fn iter(&mut self) -> &impl Iterator<Item = Result<Read<Token<'a>>, ParseError>> {
        self.tokenize()
    }
}

impl<'a> Iterator for ParseContext<'a> {
    type Item = Result<Read<Token<'a>>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_token = self.next_token();
        match next_token {
            Ok(Read {
                result: Token::Eof, ..
            }) => None,
            Err(e) => Some(Err(e)),
            _ => Some(next_token),
        }
    }
}

#[allow(unused_macros)]
macro_rules! try_read_tokens {
    (@consume $ctx:expr, $p:pat, $result:expr) => {
        {
            let next = $ctx.iter().next();
            let token_read = next.ok_or(ParseError::new("unexpected end of token stream")).and_then(|tok: Result<Read<Token>, ParseError>| match tok {
                Ok(Read{result: $p, len}) => {
                    if cfg!(debug_assertions) {
                        println!("parsed: {:?}", stringify!($p));
                    }
                    Ok(Read::new($result, len))
                },
                // TODO: create a format_parse_error! macro
                other => Err(ParseError::from_string(format!("unexpected token, got '{:?}', but expected '{}; source was {:?}...", other, stringify!($p), &$ctx.remaining_src()[0..=50]))),
            });
            token_read
        }
    };
    (@count $first:expr, $($rest:expr),+) => (
        1 + try_read_tokens!(@count, $(rest)+)
    );
    (@count $first:expr) => (1);
    ($ctx:expr, [ $($toks:pat => $result:expr),* ] ) => {
        {
            for i in 0..=try_read_tokens!(@count $($result),+) {
                let pumped = $ctx.make_further_test_ctx();
            }
            let tuple = (
            $(try_read_tokens!(@consume pumped, $toks, $result)),*
            ,
            );
            return tuple;
        }
    };
}

// FIXME: explore removing the need for the $capture param
macro_rules! expect_tokens {
    (@consume $ctx:expr, $p:pat, $result:expr) => {
        {
            let token_read = $ctx.iter().next().ok_or(ParseError::new("unexpected end of token stream")).and_then(|tok: Result<Read<Token>, ParseError>| match tok {
                Ok(Read{result: $p, len}) => {
                    if cfg!(debug_assertions) {
                        println!("parsed: {:?}", stringify!($p));
                    }
                    Ok(Read::new($result, len))
                },
                // TODO: create a format_parse_error! macro
                other => Err(ParseError::from_string(format!("unexpected token, got '{:?}', but expected '{}; source was {:?}...", other, stringify!($p), &$ctx.remaining_src()[0..=50]))),
            })?;
            $ctx.consume_read(token_read)
        }
    };
    ($ctx:expr, [ $($toks:pat => $result:expr),* ] ) => {
        (
        $(expect_tokens!(@consume $ctx, $toks, $result)),*
        ,
        )
    };
}

pub trait Lexable<'a, T> {
    fn try_lex(src: &'a str) -> Result<Read<T>, ParseError>;
}

pub trait Parseable<'a, T> {
    fn try_parse(ctx: ParseContext) -> (ParseContext, Result<Read<T>, ParseError>);
}

// NOTE: rename to Anchor, or Aligner?
#[derive(Debug, PartialEq)]
pub enum IndentMark<'a> {
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
) -> Result<Expr, ParseError>
where
    IsLastChar: Fn(usize, char, Option<char>) -> Result<bool, ParseError>,
    Map: FnOnce(&'a str) -> Result<Expr, ParseError>,
{
    src.chars()
        .nth(0)
        .ok_or(ParseError::new("trying to start reading from EOF"))
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
                .unwrap_or(ParseError::err("failed to find")),
        )
        .and_then(|end| {
            let content = &src[..=end];
            map_to_expr(content)
        })
}

impl<'a> Literal<'a> {
    fn try_lex_integer(src: &'a str) -> Result<Read<Literal<'a>>, ParseError> {
        try_read_chars(
            src,
            |i, c, next| match (i, c, next) {
                (0, c, _) if !matches!(c, '1'..='9') => {
                    ParseError::err("integers must start with a non-zero digit /[1-9]/")
                }
                (_, _, None) => Ok(true),
                (_, _, Some(next)) if !(next.is_ascii_digit()) => Ok(true),
                _ => Ok(false),
            },
            |s| {
                s.parse::<i64>()
                    // NEEDSWORK: combine the parse error rather than swallow it?
                    .map_err(|_err| ParseError::new("expected integer literal"))
                    .map(|i| Read::new(Literal::Integer(i), s.len()))
            },
        )
    }

    fn try_lex_float(src: &'a str) -> Result<Read<Literal<'a>>, ParseError> {
        try_read_chars(
            src,
            |i, c, next| match (i, c, next) {
                (0, c, _) if !matches!(c, '1'..='9') => {
                    ParseError::err("floats must start with a non-zero digit /[1-9]/")
                }
                (_, _, None) => Ok(true),
                (_, _, Some(next)) if !(next.is_ascii_digit() || next == '.') => Ok(true),
                _ => Ok(false),
            },
            |s| {
                s.parse::<f64>()
                    // NEEDSWORK: combine the parse error rather than swallow it?
                    .map_err(|_err| ParseError::new("expected float literal"))
                    .map(|f| Read::new(Literal::Float(f), s.len()))
            },
        )
    }

    fn try_lex_string(src: &'a str) -> Result<Read<Literal<'a>>, ParseError> {
        lex::string_literal(src).map(|s| Read::new(Literal::String(&s[1..s.len() - 1]), s.len()))
    }

    fn try_lex_regex(src: &'a str) -> Result<Read<Literal<'a>>, ParseError> {
        lex::regex_literal(src).map(|s| Read::new(Literal::String(&s[1..s.len() - 1]), s.len()))
    }

    fn try_lex_boolean(src: &'a str) -> Result<Read<Literal<'a>>, ParseError> {
        if src.starts_with("true") {
            Ok(Read::new(Literal::Boolean(true), "true".len()))
        } else if src.starts_with("false") {
            Ok(Read::new(Literal::Boolean(false), "false".len()))
        } else {
            ParseError::err("expected boolean literal ('true' or 'false')")
        }
    }
}

impl<'a> Lexable<'a, Literal<'a>> for Literal<'a> {
    fn try_lex(src: &'a str) -> Result<Read<Literal<'a>>, ParseError> {
        Self::try_lex_integer(src)
            .or_else(|_| Self::try_lex_float(src)) // TODO: consider combining integer and float parsing
            .or_else(|_| Self::try_lex_string(src))
            .or_else(|_| Self::try_lex_regex(src))
            .or_else(|_| Self::try_lex_boolean(src))
            .map_err(|_| ParseError::new("expected literal")) // compose errors
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
    fn try_parse_rest(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<FilterExpr<'a>>, ParseError>) {
        if ctx.remaining_src().starts_with("...") {
            (ctx, Ok(Read::new(FilterExpr::Rest, "...".len())))
        } else {
            (ctx, ParseError::err("expected rest filter '...'"))
        }
    }
    fn try_parse_binop(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<FilterExpr<'a>>, ParseError>) {
        unimplemented!()
    }
    fn try_parse_unaryop(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<FilterExpr<'a>>, ParseError>) {
        unimplemented!()
    }
    fn try_parse_node_reference(
        mut ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<FilterExpr<'a>>, ParseError>) {
        (
            ctx,
            lex::node_reference(ctx.remaining_src())
                .map(|r| r.map(|text| FilterExpr::NodeReference { name: &text[1..] }, 0)),
        )
    }
    fn try_parse_literal(
        ctx: ParseContext,
    ) -> (ParseContext, Result<Read<FilterExpr<'a>>, ParseError>) {
        unimplemented!()
    }
    fn try_parse_group(
        ctx: ParseContext,
    ) -> (ParseContext, Result<Read<FilterExpr<'a>>, ParseError>) {
        unimplemented!()
    }
    fn try_parse_name(
        ctx: ParseContext,
    ) -> (ParseContext, Result<Read<FilterExpr<'a>>, ParseError>) {
        unimplemented!()
    }
    /*
    pub fn parse<'a>(ctx: ParseContext) -> Ast<'a> {
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
    fn try_parse(ctx: ParseContext) -> (ParseContext, Result<Read<FilterExpr<'a>>, ParseError>) {
        #[rustfmt::skip]
        {
        let (ctx, result) = Self::try_parse_rest(ctx);
        if result.is_ok() { return (ctx, result); }
        let (ctx, result) = Self::try_parse_binop(ctx);
        if result.is_ok() { return (ctx, result); }
        let (ctx, result) = Self::try_parse_unaryop(ctx);
        if result.is_ok() { return (ctx, result); }
        let (ctx, result) = Self::try_parse_node_reference(ctx);
        if result.is_ok() { return (ctx, result); }
        let (ctx, result) = Self::try_parse_literal(ctx);
        if result.is_ok() { return (ctx, result); }
        let (ctx, result) = Self::try_parse_group(ctx);
        if result.is_ok() { return (ctx, result); }
        let (ctx, result) = Self::try_parse_name(ctx);
        if result.is_ok() { return (ctx, result); }
        };
        return (ctx, ParseError::err("expected filter expr"));
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
    fn try_parse_raw(ctx: ParseContext) -> Result<Read<WriteCommand<'a>>, ParseError> {
        lex::string_literal(ctx.remaining_src()).map(|s| {
            Read::new(
                WriteCommand::Raw(unescape_newlines(&s[1..s.len() - 1])),
                s.len(),
            )
        })
    }

    fn try_parse_node_reference(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<WriteCommand<'a>>, ParseError>) {
        (
            ctx,
            lex::node_reference(ctx.remaining_src()).map(|r| {
                r.map(
                    |text| WriteCommand::NodeReference {
                        name: &text[1..],
                        filters: Vec::new(),
                    },
                    0,
                )
            }),
        )
    }

    fn try_parse_wrap_point(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<WriteCommand<'a>>, ParseError>) {
        (
            ctx,
            ctx.remaining_src()
                .starts_with("\\")
                .then(|| Read::new(WriteCommand::WrapPoint, 1))
                .ok_or(ParseError::new("expected wrap point '\\'")),
        )
    }

    fn try_parse_conditional(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<WriteCommand<'a>>, ParseError>) {
        unimplemented!()
    }

    fn try_parse_indent_mark(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<WriteCommand<'a>>, ParseError>) {
        // TODO: base expect_tokens on this?
        (
            ctx,
            ctx.iter()
                .next()
                .ok_or(ParseError::new("expected indent mark, found EOF"))
                .and_then(|tok| match tok {
                    Ok(Read {
                        result: Token::IndentMark(im),
                        len,
                    }) => Ok(Read {
                        result: WriteCommand::IndentMark(im),
                        len,
                    }),
                    _ => ParseError::err("expected indent mark"),
                }),
        )
    }

    fn try_parse_sequence(
        ctx: ParseContext<'a>,
    ) -> (ParseContext<'a>, Result<Read<WriteCommand<'a>>, ParseError>) {
        let (start,) = expect_tokens!(ctx, [Token::LBrace => ()]);
        let mut seq = Vec::<WriteCommand<'a>>::new();
        ctx.consume_read(start);
        while expect_tokens!(ctx, [Token::RBrace => ()]).1.is_err() {
            seq.push(ctx.consume_read(Self::try_parse(ctx)?));
        }
        let (end,) = expect_tokens!(ctx, [Token::RBrace => ()]);
        return Ok(Read::new(WriteCommand::Sequence(seq), 0));
    }

    fn try_parse_atom(ctx: ParseContext) -> Result<Read<WriteCommand<'a>>, ParseError> {
        Self::try_parse_raw(ctx)
            .or_else(|_| Self::try_parse_node_reference(ctx))
            .or_else(|_| Self::try_parse_wrap_point(ctx))
            .or_else(|_| Self::try_parse_conditional(ctx))
            .or_else(|_| Self::try_parse_indent_mark(ctx))
            //.or_else(|_| Self::try_parse_sequence(src))
            .map_err(|_| ParseError::new("expected atomic expression"))
    }
}

impl<'a> Parseable<'a, WriteCommand<'a>> for WriteCommand<'a> {
    fn try_parse(ctx: ParseContext) -> Result<Read<WriteCommand<'a>>, ParseError> {
        Self::try_parse_raw(ctx)
            .or_else(|_| Self::try_parse_sequence(ctx))
            .or_else(|_| Self::try_parse_node_reference(ctx))
            .or_else(|_| Self::try_parse_wrap_point(ctx))
            .or_else(|_| Self::try_parse_indent_mark(ctx))
            .or_else(|_| Self::try_parse_conditional(ctx))
            .map_err(|_| ParseError::new("expected write command")) // TODO: compose errors
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
    pub fn parse_aux<'a>(ctx: ParseContext, min_prec: i32) -> Ast<'a> {
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

    pub fn parse<'a>(ctx: ParseContext) -> Ast<'a> {
        parse_aux(ctx, 0)
    }
    */
}

fn parse_file<'a>(ctx: ParseContext) -> Result<File<'a>, ParseError> {
    let mut file = File {
        nodes: HashMap::new(),
    };
    while !matches!(
        ctx.iter().next(),
        Some(Ok(Read {
            result: Token::Eof,
            ..
        }))
    ) {
        let node_decl = parse_node_decl(ctx)?;
        file.nodes.insert(node_decl.name, node_decl.commands);
    }
    return Ok(file);
}

fn parse_node_decl<'a>(ctx: ParseContext) -> Result<Node<'a>, ParseError> {
    // TODO: make tokenize consume Reads from the parse ctx.
    let (_, name, _) = expect_tokens!(
        ctx,
        [Token::Node => (), Token::Literal(Literal::String(name)) => name, Token::Eq => ()]
    );
    let commands = ctx.consume_read(WriteCommand::try_parse(ctx)?);
    Ok(Node { name, commands })
}

pub(crate) fn parse_text<'a>(ctx: ParseContext) -> Result<File<'a>, ParseError> {
    parse_file(ctx)
}
