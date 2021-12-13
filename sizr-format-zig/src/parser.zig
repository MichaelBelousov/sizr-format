// parser for sizr-format

const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

const expect = @import("std").testing.expect;
const expectError = @import("std").testing.expectError;

const util = @import("./util.zig");

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

// TODO: rename size to bytesRead
pub fn next_token(inSrc: []const u8) !struct { tok: Token, size: usize } {
    const ReturnType = @typeInfo(@typeInfo(@TypeOf(next_token)).Fn.return_type.?).ErrorUnion.payload;
    // first eat all white space
    const firstNonSpace = util.indexOfNotAny(u8, inSrc, &[_]u8{ ' ', '\t', '\n' }) orelse return ReturnType{ .tok = Token.eof, .size = 0 };
    const src = inSrc[firstNonSpace..];

    if (mem.startsWith(u8, src, "{")) return ReturnType{ .tok = Token.lbrace, .size = 1 };
    if (mem.startsWith(u8, src, "}")) return ReturnType{ .tok = Token.rbrace, .size = 1 };
    if (mem.startsWith(u8, src, "[")) return ReturnType{ .tok = Token.lbrack, .size = 1 };
    if (mem.startsWith(u8, src, "]")) return ReturnType{ .tok = Token.rbrack, .size = 1 };
    // TODO: handle indentation and anchors here
    //if (mem.startsWith(u8, src, "<|")) return Token.indent_mark;
    if (mem.startsWith(u8, src, ">=")) return ReturnType{ .tok = Token.gteq, .size = 1 };
    if (mem.startsWith(u8, src, ">")) return ReturnType{ .tok = Token.gt, .size = 1 };
    if (mem.startsWith(u8, src, "<=")) return ReturnType{ .tok = Token.lteq, .size = 1 };
    if (mem.startsWith(u8, src, "<")) return ReturnType{ .tok = Token.lt, .size = 1 };
    if (mem.startsWith(u8, src, "|")) return ReturnType{ .tok = Token.pipe, .size = 1 };
    if (mem.startsWith(u8, src, "&")) return ReturnType{ .tok = Token.ampersand, .size = 1 };
    if (mem.startsWith(u8, src, ".")) return ReturnType{ .tok = Token.dot, .size = 1 };
    if (mem.startsWith(u8, src, "^")) return ReturnType{ .tok = Token.caret, .size = 1 };
    if (mem.startsWith(u8, src, "@")) return ReturnType{ .tok = Token.at, .size = 1 };
    if (mem.startsWith(u8, src, "#")) return ReturnType{ .tok = Token.hash, .size = 1 };
    // NOTE: need to figure out how to disambiguate this from regex literals
    if (mem.startsWith(u8, src, "/")) return ReturnType{ .tok = Token.fslash, .size = 1 };
    if (mem.startsWith(u8, src, "\\")) return ReturnType{ .tok = Token.bslash, .size = 1 };
    if (mem.startsWith(u8, src, "+")) return ReturnType{ .tok = Token.plus, .size = 1 };
    if (mem.startsWith(u8, src, "-")) return ReturnType{ .tok = Token.minus, .size = 1 };
    if (mem.startsWith(u8, src, "*")) return ReturnType{ .tok = Token.asterisk, .size = 1 };
    if (mem.startsWith(u8, src, "!=")) return ReturnType{ .tok = Token.noteq, .size = 2 };
    if (mem.startsWith(u8, src, "!")) return ReturnType{ .tok = Token.exclaim, .size = 1 };
    if (mem.startsWith(u8, src, "==")) return ReturnType{ .tok = Token.eqeq, .size = 2 };
    if (mem.startsWith(u8, src, "=")) return ReturnType{ .tok = Token.eq, .size = 1 };
    if (mem.startsWith(u8, src, "~")) return ReturnType{ .tok = Token.tilde, .size = 1 };
    if (mem.startsWith(u8, src, "\"")) {
        const content = try readCharDelimitedContent(src, '"');
        return ReturnType{ .tok = Token{ .literal = Literal{ .string = content } }, .size = content.len };
    }
    if (mem.startsWith(u8, src, "$")) {
        const ident = readIdent(src[1..]);
        return ReturnType{ .tok = Token{ .reference = ident }, .size = ident.len };
    }
    if (isIdentStart(src[0])) {
        const ident = readIdent(src);
        if (mem.eql(u8, ident, "node")) return ReturnType{ .tok = Token{ .kw_node = {} }, .size = ident.len };
        return LexError.Unknown;
    }
    if (ascii.isDigit(src[0])) {
        const number = try readNumber(src);
        return ReturnType{ .tok = number.tok, .size = number.size };
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

test "lexer" {
    try expectError(LexError.Unknown, next_token("node_example"));
    try expectError(LexError.UnexpectedEof, next_token("\"unterminated string"));
    try expect(Token.eof == (next_token("") catch unreachable).tok);
    try expect(Token.eof == (next_token("  \t  \n ") catch unreachable).tok);
    try expect((try next_token("    4.56 ")).tok.literal.float == 4.56);
    // FIXME: is this idiomatic equality checking?
    try expect(std.mem.eql(u8, (try next_token("\"escape containing\\\" string \" ")).tok.literal.string, "escape containing\\\" string "));
    try expect((try next_token("4.56 ")).tok.literal.float == 4.56);
    try expect((try next_token("1005 ")).tok.literal.integer == 1005);
    try expect((try next_token("0x5_6 ")).tok.literal.integer == 0x5_6);
    try expect((try next_token("0b1001_0000_1111 ")).tok.literal.integer == 0b1001_0000_1111);
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
fn readNumber(src: []const u8) !struct { tok: Token, size: usize } {
    const ReturnType = @typeInfo(@typeInfo(@TypeOf(readNumber)).Fn.return_type.?).ErrorUnion.payload;
    // TODO: roll my own parser to not have redundant logic
    const hasPrefixChar = ascii.isAlpha(src[1]) and ascii.isDigit(src[2]);
    var hadPoint = false;
    var tok_end: usize = 0;
    for (src) |c, i| {
        if (c == '.') {
            hadPoint = true;
            continue;
        }
        if (c == '_') continue;
        const isPrefixChar = i == 1 and hasPrefixChar;
        if (!ascii.isDigit(c) and !isPrefixChar) {
            tok_end = i;
            break;
        }
    }
    if (hadPoint) {
        const val = try std.fmt.parseFloat(f64, src[0..tok_end]);
        return ReturnType{ .tok = Token{ .literal = Literal{ .float = val } }, .size = tok_end };
    } else {
        const val = try std.fmt.parseInt(i64, src[0..tok_end], 0);
        return ReturnType{ .tok = Token{ .literal = Literal{ .integer = val } }, .size = tok_end };
    }
}

// FIXME: this might need to allocate to remove the escapes, or we can make it a special
// string type which is interpreted with those escapes.
// @precondition that src starts with the delimiter
fn readCharDelimitedContent(src: []const u8, comptime delimiter: u8) LexError![]const u8 {
    const escaper = '\\';
    for (src[1..]) |c, i| {
        // we're iterating over src[1..]: src[1..][i] = src[i + 1]
        if (c == delimiter and src[i] != escaper) {
            return src[1 .. i + 1];
        }
    }
    return LexError.UnexpectedEof;
}

const ParseError = error{
    CloserWithoutCorrespondingOpener,
    Unknown,
};

fn token_stream(inSrc: []const u8) !std.ArrayList(Token) {
    var cur = inSrc;
    var tok: ?Token = null;
    var result = std.ArrayList(Token).init(std.heap.c_allocator);
    while (true) {
        const tok_result = try next_token(cur);
        const definitely_a_token = tok_result.tok;
        tok = definitely_a_token;
        cur = cur[tok_result.size..];
        try result.append(definitely_a_token);
        if (definitely_a_token == Token.eof) break;
    }
    return result;
}

test "token_stream" {
    const toks = [_]Token{ Token{ .literal = Literal{ .float = 5.2 } }, Token{ .literal = Literal{ .integer = 100 } }, Token.eof };
    try expect(std.mem.eql(Token, &toks, (try token_stream(" 5.2 100 "))[0..]));

    try expectError(LexError.UnexpectedEof, next_token("\"unterminated string"));
    try expect(Token.eof == next_token("") catch unreachable);
    try expect(Token.eof == next_token("  \t  \n ") catch unreachable);
    try expect((try next_token("    4.56 ")).literal.float == 4.56);
    // FIXME: is this idiomatic equality checking?
    try expect(std.mem.eql(u8, (try next_token("\"escape containing\\\" string \" ")).literal.string, "escape containing\\\" string "));
    try expect((try next_token("4.56 ")).literal.float == 4.56);
    try expect((try next_token("1005 ")).literal.integer == 1005);
    //std.debug.print("found '{s}'\n", .{try next_token("0x56 ")});
    try expect((try next_token("0x5_6 ")).literal.integer == 0x5_6);
    try expect((try next_token("0b1001_0000_1111 ")).literal.integer == 0b1001_0000_1111);
}

const WriteCommand = union(enum) {
    raw: []const u8,
    referenceExpr: struct {
        name: []const u8,
        name: []const u8,
        filters: std.ArrayList(FilterExpr), // comma-separated
    },
    wrapPoint,
    conditional: struct {
        test_: FilterExpr,
        then: ?*WriteCommand,
        else_: ?*WriteCommand,
    },
    indentMark: IndentMark,
    sequence: std.ArrayList(WriteCommand),
};

fn parse(src: []const u8) !std.ArrayList(WriteCommand) {
    // FIXME: don't use the c_allocator until we integrate with treesitter
    var result = std.ArrayList(WriteCommand).init(std.heap.c_allocator);
    return result;
}
