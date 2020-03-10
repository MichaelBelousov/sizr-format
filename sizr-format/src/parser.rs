/**
 * Parser for the sizr-format language
 */

#[derive(Debug)]
struct ParseContext<'a> {
    src: &'a str;
    loc: mut u32;
    fn remainingSrc() -> &'a str { src[loc..] }
    astSoFar: mut AstNode; // Vec<TopLevelDef>
    unmatched: mut Stack<AstNode>;
}

pub mod atoms {
    fn parse_integer(ctx: &mut ParseContext) {
        let end = ctx.src[ctx.loc](|c| c.whitespace);
        let atom_src = ctx.src[ctx.loc..ctx.src];
    }

    fn parse_quote(ctx: &mut ParseContext) {
        ctx.loc += 1; //skip delimiter
    }

    fn parse_regex(ctx: &mut ParseContext) {
        ctx.loc += 1; //skip delimiter
    }

    fn parse_paren_group(ctx: &mut ParseContext) {
        ctx.loc += 1; //skip opener
    }

    fn parse_lambda(ctx: &mut ParseContext) {
    }

    fn parse_wrap(ctx: &mut ParseContext) {
    }
}

fn parseAtom(ctx: &mut ParseContext) {
}

fn parseCommand(ctx: &mut ParseContext) {
    match ctx.remainingSrc()[0] {
        '"'  => { parse_quote(&ctx) },
        '\\' => { parse_wrap(&ctx) },
        '?'  => { parse_cond(&ctx) },
        _ => panic!("Unknown token, expected write command")
    }
}

fn skipWhitespace(ctx: &mut ParseContext) {
    ctx.loc = ctx.remainingSrc().find(|c| !c.is_space());
}

fn parseFile(ctx: &mut ParseContext) {
    while ctx.loc < ctx.src.length {
        skipWhitespace(&ctx);
        parseFormatDef(&ctx);
    }
}

fn skipToDelim(ctx: &mut ParseContext) {
    ctx.loc = ctx.remainingSrc().findIndex(|c| c == '\'');
}

fn parseFormatDef(ctx: &mut ParseContext) {
    skipWhitespace(&ctx);
    parseIdentifier(&ctx);
    skipToDelim(&ctx);
    //parseDelim?
    let idxAfterDelim = ctx.remainingSrc().findIndex(|c| c != '\'');
    let delim = ctx.remainingSrc()[..idxAfterDelim];
    while ctx.remainingSrc()[..idxAfterDelim.len()] == delim {
        skipWhitespace(&ctx);
        parseCommand(&ctx);
    }
}

pub mod ops {
    static let AND = "&";
    static let OR = "|";
    static let XOR = "^";
    static let GT = ">";
    static let GTE = ">=";
    static let EQ = "=";
    static let NEQ = "!=";
    static let LTE = "<=";
    static let LT = "<";
    static let PLUS = "+";
    static let MINUS = "-";
    static let MULT = "*";
    static let DIV = "/";
    static let INTDIV = "//";
    static let MOD = "%";
    static let POW = "**";
    static let DOT = ".";
    static let EXCLAIM = "!";
    static let TILDE = "~";
    fn parse_slice(ctx: &mut ParseContext) {
    }
}

fn parse_bin_op(ctx: &mut ParseContext) {
}

fn parse_mono_op(ctx: &mut ParseContext) {
}

fn parse_write(ctx: &mut ParseContext) {
}

pub enum AstNode<'a> {
    Indent,
    Outdent,
    Align(Regex),
    Add(Box<AstNode>, Box<AstNode>),
    Quote(&'a str)
}

fn parse_indent_ctx_decl(ctx: &mut ParseContext) {
    match ctx.src[ctx.loc..ctx.loc+2] {
        "|>" => { ctx.ast.add(Indent()); },
        ">/" => { ctx.loc+=1; parse_regex(&ctx); },
        "<|" => { ctx.ast.add(Outdent()) },
        _ => panic!("Unknown token, expected indentation context")
    }
}
