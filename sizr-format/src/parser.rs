/**
 * Parser for the sizr-format language
 */

extern crate regex;

extern crate lazy_static;
use std::collections::BTreeMap;

use std::vec::Vec;
use std::boxed::Box;


pub mod parser {

    #[derive(Debug)]
    pub enum Ast<'a> {
        Identifier(&'a str),
        Indent,
        Outdent,
        Align(Option<regex::Regex>),
        Add(Box<Ast<'a>>, Box<Ast<'a>>),
        Quote(&'a str),
        Regex(regex::Regex),
        Number(f64),
        Variable{ name: &'a str },
        Lambda{ equals: Option<Box<Ast<'a>>> },
        Underscore,
        Group(Box<Ast<'a>>),
        Cond{
            cond: Box<Ast<'a>>,
            then: Option<Box<Ast<'a>>>,
            else_: Option<Box<Ast<'a>>>
        },
        WrapPoint,
        NodeFormat(Vec<Box<Ast<'a>>>),
        File(Vec<NodeFormat>),
    }

    impl<'a> Ast<'a> {
        pub fn append(&self, next: Ast<'a>) {
            match self {
                NodeFormat(v) => v.push(Box::new(next)),
                File(v) => match next {
                    NodeFormat => v.push(next),
                    _ => panic!("wrong ast node to append to file"),
                },
                _ => panic!("cannot append to other ast nodes")
            }
        }
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

    pub mod matcher {
        fn identifier(ctx: &ParseContext) -> bool {
            if let Some(c) = ctx.remainingSrc().chars().nth(0) {
                c.is_alpha()
            } else false
        }

        fn number(ctx: &ParseContext) -> bool {
            if let Some(c) = ctx.remainingSrc().chars().nth(0) {
                c.is_numeric()
            } else false
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
    }

    pub mod atoms {
        use ParseContext;

        // NOTE: maybe take immutable context ref?
        pub fn readIdentifier(ctx: &mut ParseContext) -> str {
            // TODO: verify first char is not numeric in debug mode
            if let Some(after) = ctx.remainingSrc().find(
                |c: char| !c.is_alphanumeric() && c != '_'
            ) {
                // TODO: convert escape sequences i.e. \n, \\, etc
                let content = &ctx.remainingSrc()[..after];
                ctx.ast.append(Ast::Identifier(content));
                ctx.loc += after;
                content
            } else {
                panic!("debug");
            }
        }


        fn readQuoted(ctx: &mut ParseContext, delim: char) -> &str {
            // TODO: in debug mode check explicitly for delimiter match
            ctx.loc += 1; //skip delimiter
            let start = ctx.loc;
            loop {
                match &ctx.remainingSrc().find(|c: char| c == '\\' || c == delim) {
                    Some(jump) => {
                        match &ctx.remainingSrc()[jump] {
                            '\\'  => { ctx.loc += jump + 2; },
                            delim => { ctx.loc += jump + 1; break; },
                            _ => panic!("unreachable")
                        }
                    },
                    None => break
                }
            }
            &ctx.src[start..ctx.loc-1]
        }

        pub fn parseNumber(ctx: &mut ParseContext) -> Ast::Number {
            // TODO: support scientific notation
            if let Some(end) =
                ctx.remainingSrc().find(|c: char| !c.is_numeric() && c != '.'
            ) {
                let src = ctx.remainingSrc()[..end];
                let parsed = src.parse::<f64>().unwrap();
                Ast::Number(parsed)
            } else {
                panic!("failed to parse number");
            }
        }

        pub fn parseQuote(ctx: &mut ParseContext) -> Ast::Quote {
            Ast::Quote(readQuoted(ctx, '"'));
        }

        pub fn parseRegex(ctx: &mut ParseContext) -> Ast::Regex {
            Ast::Regex(readQuoted(ctx, '/'));
        }

        pub fn parseParenGroup(ctx: &mut ParseContext) {
            ctx.loc += 1; //skip opener
            // TODO: in debug mode check explicitly for delimiter match
            let expr = parseExpression(ctx);
            ctx.loc += 1; //skip closer
            // TODO: in debug mode check explicitly for delimiter match
            ctx.ast = Ast::Group(0);
        }

        pub fn parseLambda(ctx: &mut ParseContext) {
            // TODO: in debug mode explicitly check for "." start
            ctx.loc += 1;
            let name = readIdentifier();
            match ctx.remainingSrc().chars().nth(0)  {
                Some(c @ '=') => {
                    let expr = parseExpression(ctx);
                    ast.append(Ast::Lambda{equals: Some(Box::new(expr))});
                },
                Some(_) => {
                    ast.append(Ast::Lambda{equals: None});
                },
                _ => panic!("unexpected during lambda parsing")
            }
        }

        pub fn parseWrapPoint(ctx: &mut ParseContext) {
            ctx.loc += 1;
            ctx.ast.append(Ast::WrapPoint);
        }
    }

    //pub mod writes
    pub mod exprs {
        use ParseContext;

        pub fn parseCond(ctx: &mut ParseContext) {
        }
    }

    pub fn parseCommand(ctx: &mut ParseContext) {
        match &ctx.remainingSrc().chars().nth(0) {
            Some(c) => match c {
                '"'  => atoms::parseQuote(ctx),
                '\\' => atoms::parseWrapPoint(ctx),
                '?'  => exprs::parseCond(ctx),
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

    // TODO: group with parse format def specific mod
    fn skipToDelim(ctx: &mut ParseContext) {
        match &ctx.remainingSrc().find(|c: char| c == '\'') {
            Some(jump) => ctx.loc += jump,
            None => panic!("EOI")
        }
    }

    pub fn parseFormatDef(ctx: &mut ParseContext) {
        skipWhitespace(ctx);
        let name = atoms::readIdentifier(ctx);
        skipToDelim(ctx);
        let maybeIdx = &ctx.remainingSrc().find(|c: char| c != '\'');
        if let Some(idxAfterDelim) = maybeIdx {
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
      //parseAtom(ctx);
      //parseBinOp(ctx);
      //parseAtom(ctx);
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
