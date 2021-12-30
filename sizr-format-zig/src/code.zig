const Expr = union(enum) {
    a: u32,
    b,
};

const Value = union(enum) {
    c: u32,
    d,
};

// works if I make it return not optional
fn f(expr: Expr) ?Value {
    var val: u32 = 0;
    return switch(expr) {
        .a => |a|
            if (a == 0)
                Value{ .c = 0}
            else if (val == 0)
                Value{ .d = {} }
            else
                Value{ .d = {} },
        else => Value{.d = {}}
    };
}

test "" {
    _ = f(Expr{.a = 0});
}
