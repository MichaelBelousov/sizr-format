/**
 * Parser for the sizr-format language
 */

extern crate regex;
use regex::Regex;

extern crate lazy_static;
use std::collections::BTreeMap;

use std::vec::Vec;

pub mod parser {

    #[derive(Debug)]
    pub enum Ast<'a> {
        Indent,
        Outdent,
        Align(Option<regex::Regex>),
        Add(Box<Ast<'a>>, Box<Ast<'a>>),
        Quote(&'a str),
        Number(f64),
        Variable{ name: &'a str },
        Group(Box<Ast<'a>>),
        Cond{
            cond: Box<Ast<'a>>,
            then: Box<Ast<'a>>,
            else_: Box<Ast<'a>>
        },
        WrapPoint,
        NodeFormat(Vec<Box<Ast<'a>>>),
        File(Vec<Format>),
    }

    impl<'a> Ast<'a> {
        pub fn append(&self, next: Ast<'a>) {
            match self {
                Indent => ,
                Outdent =>
            }
        }
        /*
        pub fn finish(&self) {
            match self {
                Group(_) => 
            }
        }
        */
    }

    #[derive(Debug)]
    struct ParseContext<'a> {
        pub src: &'a str,
        pub loc: usize,
        pub ast: Ast<'a>,
    }

    impl<'a> ParseContext<'a> {
        fn remainingSrc(&self) -> &'a str {
            &self.src[self.loc..]
        }
    }

    pub mod atoms {
        use ParseContext;

        pub fn parseQuoteSyntax(ctx: &mut ParseContext, delim: char) {
            // TODO: in debug mode check explicitly for delimiter match
            ctx.loc += 1; //skip delimiter
            let start = ctx.loc;
            let delimLen = 1;
            loop {
                match &ctx.remainingSrc().find(|c: char| c == '\\' || c == delim) {
                    Some(jump) => {
                        match &ctx.remainingSrc()[jump] {
                            '\\'  => { ctx.loc += jump + delimLen + 1; },
                            delim => { ctx.loc += jump + 1; break; },
                            _ => panic!("unreachable")
                        }
                    },
                    None => break
                }
            }
        }

        pub fn parseNumber(ctx: &mut ParseContext) {
            let end = ctx.src[ctx.loc](|c| c.whitespace);
            let atom_src = ctx.src[ctx.loc..ctx.src];
        }

        pub fn parseQuote(ctx: &mut ParseContext) {
            parseQuoteSyntax(ctx, '"');
        }

        pub fn parseRegex(ctx: &mut ParseContext) {
            parseQuoteSyntax(ctx, '/');
        }

        pub fn parseParenGroup(ctx: &mut ParseContext) {
            ctx.loc += 1; //skip opener
            // TODO: in debug mode check explicitly for delimiter match
            let expr = parseExpression();
            ctx.loc += 1; //skip closer
            // TODO: in debug mode check explicitly for delimiter match
            ctx.ast.append(loc);
        }

        pub fn parseLambda(ctx: &mut ParseContext) {
        }

        pub fn parseWrap(ctx: &mut ParseContext) {
        }
    }

    pub fn parseAtom(ctx: &mut ParseContext) {
    }

    pub fn parseCommand(ctx: &mut ParseContext) {
        match &ctx.remainingSrc().chars().nth(0) {
            Some(c) => match c {
                '"'  => { atoms::parseQuote(ctx) },
                '\\' => { atoms::parseWrap(ctx) },
                '?'  => { atoms::parseCond(ctx) },
                _ => panic!("Unknown token, expected write command")
            },
            None => panic!("Unknown token, expected write command")
        }
    }

    fn skipWhitespace(ctx: &mut ParseContext) {
        match &ctx.remainingSrc().find(|c: char| !c.is_whitespace()) {
            Some(jump) => ctx.loc += jump,
            None => panic!("reached EOI")
        }
    }

    pub fn parseFile(ctx: &mut ParseContext) {
        while ctx.loc < ctx.src.len() {
            skipWhitespace(ctx);
            parseFormatDef(ctx);
        }
    }

    fn skipToDelim(ctx: &mut ParseContext) {
        match &ctx.remainingSrc().find(|c: char| c == '\'') {
            Some(jump) => ctx.loc += jump,
            None => panic!("EOI")
        }
    }

    pub fn parseIdentifier(ctx: &mut ParseContext) {
        let first = ctx.remainingSrc().chars().nth(0);
        if first.is_numeric() { panic!("invalid identifier")
        let after = ctx.remainingSrc().find(|c: char| c.is_whitespace() && c != '_' && c.is_numeric() )
    }

    pub fn parseFormatDef(ctx: &mut ParseContext) {
        skipWhitespace(ctx);
        parseIdentifier(ctx);
        skipToDelim(ctx);
        let idxAfterDelim = &ctx.remainingSrc().find(|c: char| c != '\'');
        if let Some(idxAfterDelim) {
            let delim = &ctx.remainingSrc()[..idxAfterDelim];
            while &ctx.remainingSrc()[..idxAfterDelim] != delim {
                skipWhitespace(ctx);
                parseCommand(ctx);
            }
        } else {
            panic!("bad format definition syntax");
        }
    }

    pub mod ops {
        use ParseContext;

        fn parseSlice(ctx: &mut ParseContext) {
        }

        pub enum Precedence {
            Logic = 0,
            Comp, Add, Mult, Exp, Dot,
        }

        lazy_static! {
          pub static ref binOpPrecedenceMap: BTreeMap<&'static str, Precedence> = {
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
          }
        }
    }

    // TODO: use precedence climbing for bin ops
    pub fn parseBinOp(ctx: &mut ParseContext) {
      skipWhitespace(ctx);
      parseAtom(ctx);
      parseBinOp(ctx);
      parseAtom(ctx);
    }

    pub fn parseUnaryOp(ctx: &mut ParseContext) {
    }

    pub fn parseIndentCtxDecl(ctx: &mut ParseContext) {
        match &ctx.src[ctx.loc..ctx.loc+2] {
            "|>" => { ctx.ast.append(Ast::Indent); },
            ">/" => { ctx.loc += 1; atoms::parseRegex(ctx); },
            "<|" => { ctx.ast.append(Ast::Outdent) },
            _ => panic!("Unknown token, expected indentation context")
        }
    }
}
