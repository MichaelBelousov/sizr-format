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
};
const Assoc = enum { left, right };
