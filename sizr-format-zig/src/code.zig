const std = @import("std");
const expect = @import("std").testing.expect;

const ts = struct {
    const Node = struct {
        _c: struct {
            context: [4]u32,
            id: ?*void,
            tree: ?*void,
        },
    };
};


const BinOp = enum {
    dot,
};

const Expr = union(enum) {
    rest,
    binop: struct {
        op: BinOp,
        left: *Expr,
        right: *Expr,
    },
    name: []const u8,

    pub fn parse(alloc: std.mem.Allocator, source: []const u8) !*Expr {
        // super bare-bones impl only supporting member-of operator for now
        var remaining = source;
        var expr: *Expr = try alloc.create(Expr);
        var loop_first_iter = true;
        while (std.mem.indexOf(u8, remaining, ".")) |index| {
            if (loop_first_iter) {
                expr.* = Expr{.name = remaining[0..index]};
                loop_first_iter = false;
            } else {
                const name_expr = try alloc.create(Expr);
                name_expr.* = Expr{.name = remaining[0..index]};
                const prev_expr = expr;
                expr = try alloc.create(Expr);
                expr.* = Expr{.binop = .{
                    .op = .dot,
                    .left = prev_expr,
                    .right = name_expr
                }};
            }
            remaining = remaining[index+1..];
        }
        return expr;
    }

    /// recursively free the expression tree
    /// alloc must be the same allocator that was used when creating this Expr
    pub fn free(self: @This(), alloc: std.mem.Allocator) void {
        switch (self) {
            .binop => |val| { alloc.destroy(val.left); alloc.destroy(val.right); },
            else => {}
        }
    }
};

const WriteCommand = union(enum) {
    referenceExpr: struct {
        name: Expr,
        // FIXME: this should actually be a more specific type than List(Value), since only certain value types are allowed
        filters: []const Value, // comma-separated
    },
};

/// TODO: replace with Expr (or otherwise merge them, maybe the "Value" name is better)
const Value = union(enum) {
    string: []const u8,
    node: ts.Node,
};

// TODO: use idiomatic zig polymorphism
fn Resolver(
    comptime Ctx: type,
) type {
    return struct {
        const Self = @This();
        resolveFn: fn (self: Self, ctx: Ctx, expr: Expr) ?Value,
        pub fn resolve(self: Self, ctx: Ctx, expr: Expr) ?Value {
            return self.resolveFn(self, ctx, expr);
        }
    };
}

const null_node = ts.Node{ ._c = .{.context=.{0,0,0,0}, .id=null, .tree=null } };

const LangResolver = struct {
    const Self = @This();

    // temp
    source: []const u8,

    resolver: Resolver(ts.Node),

    fn resolveFn(resolver: Resolver(ts.Node), node: ts.Node, expr: Expr) ?Value {
        const self = @fieldParentPtr(Self, "resolver", &resolver);
        _ = self;

        return switch(expr) {
            .name => |name|
                if (std.meta.eql(name, "type"))
                    Value{ .string = "test" }
                else if (std.fmt.parseInt(u32, name, 10)) |index| (
                    // the parser will reject negative indices
                    if (index < 0) unreachable
                    else Value{ .node = null_node }
                ) else |_| Value{ .node = null_node },
            .binop => |op| switch (op.op) {
                .dot => {
                    const left = resolveFn(self.resolver, node, op.left.*);
                    if (left == null or left.? != .node) @panic("only nodes are supported on the left side of a `.` expr right now");
                    if (op.right.* != .name) unreachable; // "right hand side of a `.` expr must be a name"
                    return resolveFn(self.resolver, left.?.node, op.right.*);
                },
            },
            else => null
        };
    }

    pub fn init(source: []const u8) Self {
        return Self{ .resolver = Resolver(ts.Node){ .resolveFn = Self.resolveFn }, .source = source };
    }
};

test "write" {
    const langr = LangResolver.init("void test(){}");
    _ = langr;
    const expr = try Expr.parse(std.testing.allocator, "0.0");
    defer expr.free(std.testing.allocator);
}
