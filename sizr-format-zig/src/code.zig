const std = @import("std");

const Expr = union(enum) {
    name: []const u8,
    other,
};

const Value = union(enum) {
    string: []const u8,
    node,
};

const Resolver = struct {
    const Self = @This();
    resolveFn: fn (self: Self, expr: Expr) ?Value,
    pub fn resolve(self: Self, expr: Expr) ?Value {
        return self.resolveFn(self, expr);
    }
};

const LangResolver = struct {
    const Self = @This();

    resolver: Resolver,

    fn resolveFn(resolver: Resolver, expr: Expr) ?Value {
        const self = @fieldParentPtr(Self, "resolver", &resolver);
        _ = self;

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

    pub fn init() Self {
        return Self{
            .resolver = Resolver{
                .resolveFn = Self.resolveFn
            },
        };
    }
};

test "write" {
    const langr = LangResolver.init();
    _ = langr;
}
