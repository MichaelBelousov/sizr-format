// parser for sizr-format

const mem = @import("std").mem;

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
        src: []u8,
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

const Literal = union(enum) {};

const IndentMark = union(enum) {};

const Token = union(enum) {
    reference: []u8,
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
    node,
    // special
    eof,
    unknown,
};

fn next_token(src: []u8) Token {
    // comptime compiled regex is probably better than this hand-written prefix tree
    return switch (src[0]) {
        '{' => Token.lbrace,
        '}' => Token.rbrace,
        '[' => Token.lbrack,
        ']' => Token.rbrack,
        '>' => switch (src[1]) {
            '=' => Token.gteq,
            else => Token.gt,
        },
        '<' => switch (src[1]) {
            '=' => Token.lteq,
            else => Token.lt,
        },
        '|' => Token.pipe,
        '&' => Token.ampersand,
        '.' => Token.dot,
        '^' => Token.caret,
        '@' => Token.at,
        '#' => Token.hash,
        '/' => Token.fslash,
        '\\' => Token.bslash,
        '+' => Token.plus,
        '-' => Token.minus,
        '*' => Token.asterisk,
        '!' => switch (src[1]) {
            '=' => Token.noteq,
            else => Token.exclaim,
        },
        '=' => switch (src[1]) {
            '=' => Token.eqeq,
            else => Token.eq,
        },
        '~' => Token.tilde,
        else => Token.unknown,
    };
}
