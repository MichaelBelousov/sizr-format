const std = @import("std");

const Expr = union(enum) {
    name: []const u8,
    other,
};

const Value = union(enum) {
    string: []const u8,
    node,
};

// works if I make it return not optional
fn f(expr: Expr) ?Value {
    var prng = std.rand.DefaultPrng.init(0);

    return switch(expr) {
        .name => |name|
            if (std.mem.eql(u8, name, "type"))
                Value{ .string = "test" }
            else if (prng.random().int(u32) == 0)
                Value{ .node = {} }
            else
                Value{ .node = {} },
        else => null
    };
}

test "write" {
    _ = f(Expr{.name = "test"});
}
