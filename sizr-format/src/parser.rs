
#[derive(Debug, PartialEq)]
pub struct lambda {
}

#[derive(Debug, PartialEq)]
pub struct lambda {
}

struct ParseContext<'a> {
    source: &'a str;
    loc: mut u32;
    astSoFar: mut AstNode;
    known: mut Stack<AstNode>;
}

pub mod atoms {
    fn parse_integer(ctx: &mut ParseContext) {
    }

    fn parse_quote(ctx: &mut ParseContext) {
    }

    fn parse_regex(ctx: &mut ParseContext) {
        ctx.loc += 1; //skip delimiter
    }

    fn parse_paren_group(ctx: &mut ParseContext) {
        ctx.loc += 1; //skip opener
    }

    fn parse_lambda(ctx: &mut ParseContext) {
    }

    fn parse_wrap_pt(ctx: &mut ParseContext) {
    }
}

fn parse_atom(ctx: &mut ParseContext) {
}

pub mod ops {
    fn parse_
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
    match ctx.source[ctx.loc..ctx.loc+2] {
        "|>" => { ctx.ast.add(Indent()); },
        ">/" => { ctx.loc+=1; parse_regex(&ctx); },
        "<|" => { ctx.ast.add(Outdent()) },
        _ => panic!("Unknown [indent] token")
    }
}
