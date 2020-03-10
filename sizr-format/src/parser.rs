
#[derive(Debug, PartialEq)]
pub struct lambda {
}

#[derive(Debug, PartialEq)]
pub struct lambda {
}

struct ParseContext<'a> {
    source: &'a str;
    loc: mut u32;
}

pub mod atoms {
    fn parse_integer(ctx: &mut ParseContext) {
    }

    fn parse_quote(ctx: &mut ParseContext) {
    }

    fn parse_regex(ctx: &mut ParseContext) {
    }

    fn parse_paren_group(ctx: &mut ParseContext) {
    }

    fn parse_lambda(ctx: &mut ParseContext) {
    }

    fn parse_wrap_pt(ctx: &mut ParseContext) {
    }
}

fn parse_atom(ctx: &mut ParseContext) {
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
    fn slice(ctx: &mut ParseContext) {
    }

    fn slice(ctx: &mut ParseContext) {
    }
}

fn parse_bin_op(ctx: &mut ParseContext) {
}

fn parse_mono_op(ctx: &mut ParseContext) {
}

fn parse_write(ctx: &mut ParseContext) {
}

fn parse_indent_decl(ctx: &mut ParseContext) {
    match ctx.source[ctx.loc..ctx.loc+2] {
        "
    } else if {
    }
}
