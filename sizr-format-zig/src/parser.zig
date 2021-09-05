// parser for sizr-format

const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

const Prec = enum {
    or_,
    and_,
    eq,
    cmp,
    add,
    mult,
    exp,
    dot,
};

fn ParseCtx() type {
    return struct {
        src: []const u8,
        loc: usize,
    };
}

const UnaryOp = enum {
    negate,
    bitwise_complement,
    logical_complement,
};

const Assoc = enum { left, right };

const BinOp = enum {
    and_,
    or_,
    xor,
    gt,
    gte,
    eq,
    neq,
    lte,
    lt,
    add,
    sub,
    mul,
    div,
    idiv,
    mod,
    pow,
    dot,
    fn prec(self: BinOp) Prec {
        switch (self) {
            BinOp.or_, BinOp.xor => Prec.or_,
            BinOp.and_ => Prec.and_,
            BinOp.gt, BinOp.gte, BinOp.lte, BinOp.lt => Prec.cmp,
            BinOp.eq, BinOp.neq => Prec.eq,
            BinOp.add, BinOp.sub => Prec.add,
            BinOp.mul, BinOp.div, BinOp.idiv, BinOp.mod => Prec.add,
            BinOp.pow => Prec.exp,
            BinOp.dot => Prec.dot,
        }
    }
    fn assoc(self: BinOp) Prec {
        switch (self) {
            BinOp.pow => Assoc.Right,
            else => Assoc.Left,
        }
    }
};

const Literal = union(enum) {
    boolean: bool,
    string: []const u8,
    regex: []const u8,
    integer: i64,
    float: f64,
};

const IndentMark = union(enum) {
    indent: u16,
    outdent: u16,
    token_anchor: []const u8,
    numeric_anchor: u16,
};

const Token = union(enum) {
    reference: []const u8,
    literal: Literal,
    indent_mark: IndentMark,
    // symbols
    lbrace,
    rbrace,
    lbrack,
    rbrack,
    gt,
    lt,
    eq,
    pipe,
    bslash,
    fslash,
    plus,
    minus,
    asterisk,
    ampersand,
    dot,
    caret,
    at,
    hash,
    exclaim,
    tilde,
    lteq,
    gteq,
    eqeq,
    noteq,
    // keywords
    kw_node,
    // special
    eof,
};

const LexError = error{
    UnexpectedEof,
    Unknown,
};

pub fn next_token(src: []const u8) !Token {
    if (mem.startsWith(u8, src, "{")) return Token{ .lbrace = {} };
    if (mem.startsWith(u8, src, "}")) return Token.rbrace;
    if (mem.startsWith(u8, src, "[")) return Token.lbrack;
    if (mem.startsWith(u8, src, "]")) return Token.rbrack;
    // TODO: handle indentation and anchors here
    //if (mem.startsWith(u8, src, "<|")) return Token.indent_mark;
    if (mem.startsWith(u8, src, ">=")) return Token.gteq;
    if (mem.startsWith(u8, src, ">")) return Token.gt;
    if (mem.startsWith(u8, src, "<=")) return Token.lteq;
    if (mem.startsWith(u8, src, "<")) return Token.lt;
    if (mem.startsWith(u8, src, "|")) return Token.pipe;
    if (mem.startsWith(u8, src, "&")) return Token.ampersand;
    if (mem.startsWith(u8, src, ".")) return Token.dot;
    if (mem.startsWith(u8, src, "^")) return Token.caret;
    if (mem.startsWith(u8, src, "@")) return Token.at;
    if (mem.startsWith(u8, src, "#")) return Token.hash;
    // NOTE: need to figure out how to disambiguate this from regex literals
    if (mem.startsWith(u8, src, "/")) return Token.fslash;
    if (mem.startsWith(u8, src, "\\")) return Token.bslash;
    if (mem.startsWith(u8, src, "+")) return Token.plus;
    if (mem.startsWith(u8, src, "-")) return Token.minus;
    if (mem.startsWith(u8, src, "*")) return Token.asterisk;
    if (mem.startsWith(u8, src, "!=")) return Token.noteq;
    if (mem.startsWith(u8, src, "!")) return Token.noteq;
    if (mem.startsWith(u8, src, "==")) return Token.eqeq;
    if (mem.startsWith(u8, src, "=")) return Token.eq;
    if (mem.startsWith(u8, src, "~")) return Token.tilde;
    if (mem.startsWith(u8, src, "\"")) {
        return Token{ .literal = Literal{ .string = try readCharDelimitedContent(src, '"') } };
    }
    if (mem.startsWith(u8, src, "$")) {
        const ident = readIdent(src);
        return Token{ .reference = ident };
    }
    if (isIdentStart(src[0])) {
        const ident = readIdent(src);
        if (mem.eql(u8, ident, "node")) return Token.kw_node;
        return LexError.Unknown;
    }
    if (ascii.isDigit(src[0])) {
        return readNumber(src);
    }
    return LexError.Unknown;
}

const FilterExpr = union(enum) {
    rest,
    binop: struct {
        op: BinOp,
        left: *FilterExpr,
        right: *FilterExpr,
    },
    unaryop: struct {
        op: BinOp,
        expr: *FilterExpr,
    },
    noderef: []const u8,
    literal: Literal,
    group: *FilterExpr,
    name: []const u8,
};

const expect = @import("std").testing.expect;
test "lexer" {
    try expect(next_token("node_example") == error.unknown);
}

fn isIdent(c: u8) bool {
    return ascii.isAlNum(c) or c == '_';
}

fn isIdentStart(c: u8) bool {
    return ascii.isAlpha(c) or c == '_';
}

// has a precondition that src starts with an identifier
fn readIdent(src: []const u8) []const u8 {
    for (src) |c, i| {
        if (!isIdent(c)) {
            return src[0..i];
        }
    }
    return src;
}

// has a precondition that src starts with a digit
fn readNumber(src: []const u8) !Token {
    // TODO: roll my own parser to not reparse here
    var hadPoint = false;
    var tok_end: usize = 0;
    for (src) |c, i| {
        if (!(ascii.isDigit(c) or c == '.')) {
            hadPoint = true;
            tok_end = i;
            break;
        }
    }
    if (hadPoint) {
        const val = try std.fmt.parseFloat(f64, src[0..tok_end]);
        return Token{ .literal = Literal{ .float = val } };
    } else {
        const val = try std.fmt.parseInt(i64, src[0..tok_end], 0);
        return Token{ .literal = Literal{ .integer = val } };
    }
}

// TODO: figure out specific error sets
// has precondition that src starts with the delimiter
fn readCharDelimitedContent(src: []const u8, comptime delimiter: u8) LexError![]const u8 {
    const escaper = '\\';
    for (src[1..]) |c, i| {
        if (c == delimiter and src[i - 1] != escaper)
            return src[i..i];
    }
    return LexError.UnexpectedEof;
}
