const std = @import("std");

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
    var prng = std.rand.DefaultPrng.init(0);

    return switch(expr) {
        .a => |a|
            if (a == 32)
                Value{ .c = 43}
            else if (prng.random().int(u32) == 0)
                Value{ .d = {} }
            else
                Value{ .d = {} },
        else => Value{.d = {}}
    };
}

test "write" {
    _ = f(Expr{.a = 51});
}
