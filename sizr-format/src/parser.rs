/**
 * Parser for the sizr-format language
 */

extern crate regex;
use regex::Regex;

extern crate lazy_static;
use std::collections::BTreeMap;

pub mod parser {

    #[derive(Debug)]
    pub enum Ast<'a> {
        Indent,
        Outdent,
        Align(Option<Regex<'a>>),
        Add(Box<AstNode<'a>>, Box<AstNode<'a>>),
        Quote(&'a str),
        Number(f64),
        Expr(Box<AstNode<'a>>),
    }

    impl<'a> Ast<'a> {
        pub fn append(&self) {
        }
        pub fn finish(&self) {
        }
    }

    #[derive(Debug)]
    struct ParseContext<'a> {
        pub src: &'a str,
        pub loc: usize,
        pub ast: AstNode<'a>, // Vec<TopLevelDef>
    }

    impl<'a> ParseContext<'a> {
        fn remainingSrc(&self) -> &'a str {
            &self.src[self.loc..]
        }
    }

    pub mod atoms {
        use ParseContext;

        pub fn parse_quote_syntax(ctx: &mut ParseContext, delim: char) {
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

        pub fn parse_number(ctx: &mut ParseContext) {
            let end = ctx.src[ctx.loc](|c| c.whitespace);
            let atom_src = ctx.src[ctx.loc..ctx.src];
        }

        pub fn parse_quote(ctx: &mut ParseContext) {
            parse_quote_syntax(ctx, '"');
        }

        pub fn parse_regex(ctx: &mut ParseContext) {
            parse_quote_syntax(ctx, '/');
        }

        pub fn parse_paren_group(ctx: &mut ParseContext) {
            ctx.loc += 1; //skip opener
            // TODO: in debug mode check explicitly for delimiter match
            let expr = parse_expression();
            ctx.loc += 1; //skip closer
            // TODO: in debug mode check explicitly for delimiter match
            ctx.ast.append(loc);
        }

        pub fn parse_lambda(ctx: &mut ParseContext) {
        }

        pub fn parse_wrap(ctx: &mut ParseContext) {
        }
    }

    pub fn parse_atom(ctx: &mut ParseContext) {
    }

    pub fn parseCommand(ctx: &mut ParseContext) {
        match &ctx.remainingSrc().chars().nth(0) {
            Some(c) => match c {
                '"'  => { atoms::parse_quote(ctx) },
                '\\' => { atoms::parse_wrap(ctx) },
                '?'  => { atoms::parse_cond(ctx) },
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

    pub fn parseFormatDef(ctx: &mut ParseContext) {
        skipWhitespace(ctx);
        parseIdentifier(ctx);
        skipToDelim(ctx);
        //parseDelim?
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
        fn parse_slice(ctx: &mut ParseContext) {
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
    pub fn parse_bin_op(ctx: &mut ParseContext) {
      skipWhitespace(ctx);
      parse_atom(ctx);
      parse_bin_op(ctx);
      parse_atom(ctx);
    }

    pub fn parse_unary_op(ctx: &mut ParseContext) {
    }

    pub fn parse_indent_ctx_decl(ctx: &mut ParseContext) {
        match &ctx.src[ctx.loc..ctx.loc+2] {
            "|>" => { ctx.ast.append(Ast::Indent); },
            ">/" => { ctx.loc += 1; atoms::parse_regex(ctx); },
            "<|" => { ctx.ast.append(Ast::Outdent) },
            _ => panic!("Unknown token, expected indentation context")
        }
    }
}
