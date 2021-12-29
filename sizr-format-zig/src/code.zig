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

    /// an optionally allocated blob that should be freed, but might that might be a noop
    const SerializedBlob = struct {
        alloc: ?std.mem.Allocator, // could make this a class constant to save on space
        //needs_free: bool,
        buf: []const u8,
        pub fn free(self: @This()) void {
            if (self.alloc) |alloc| {
                alloc.free(self.buf);
            }
        }
    };

    pub fn serialize(self: @This(), evalCtx: EvalCtx) []const u8 {
        _ = evalCtx;
        return switch (self) {
            .string => |val| val, // ignoring oom for now
            .node => "NULL_NODE",
        };
    }
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

const EvalCtx = struct {
    source: []const u8,
    indentLevel: u32,
    // could layer resolvers, e.g. getting linesep vars from an outer osEnv
    varResolver: LangResolver,

    const Self = @This();

    pub fn eval(self: Self, expr: Expr) ?Value {
        const node = null_node;
        return self.varResolver.resolver.resolve(node, expr);
    }

    pub fn @"test"(self: Self, expr: Expr) bool {
        // TODO: need to also return true if it's a node or etc
        return if (self.eval(expr)) |expr_val| switch (expr_val) {
            .boolean => |val| val,
            .string => |val| !std.mem.eql(u8, "", val),
            .regex => |val| !std.mem.eql(u8, "", val),
            .float => |val| val != 0.0,
            .integer => |val| val != 0,
            .node => |val| val.@"null"(),
        } else false;
    }

    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
            .indentLevel = 0,
            .varResolver = LangResolver.init(source),
        };
    }
};

pub fn write(
    // TODO: should probably be a writer that handles wrapping
    evalCtx: EvalCtx,
    cmd: WriteCommand,
    /// expected to be an std.io.Writer
    writer: anytype,
) @TypeOf(writer).Error!void {
    switch (cmd) {
        .referenceExpr => |val| {
            const maybe_eval_result = evalCtx.eval(val.name);
            if (maybe_eval_result) |eval_result| {
                const serialized = eval_result.serialize(evalCtx);
                _ = try writer.write(serialized);
            }
        },
    }
}

test "write" {
    var local = struct {
        ctx: EvalCtx,
        buf: [1024]u8,
        fn writeEqlString(self: *@This(), wcmd: WriteCommand, output: []const u8) bool {
            const bufWriter = std.io.fixedBufferStream(&self.buf).writer();
            write(self.ctx, wcmd, bufWriter) catch unreachable;
            bufWriter.writeByte(0) catch unreachable;
            const len = 1 + (std.mem.indexOf(u8, self.buf[0..], "\x00") orelse self.buf.len);
            std.debug.print("buf content: {s}\n", .{self.buf[0..len]});
            return std.mem.eql(u8, self.buf[0..len], output);
        }
    }{
        .ctx = EvalCtx.init("void test(){}"),
        .buf = undefined,
    };
    const expr = try Expr.parse(std.heap.c_allocator, "0.0");
    defer expr.free(std.heap.c_allocator);
    try expect(local.writeEqlString(
        WriteCommand{ .referenceExpr = .{ .name = expr.*, .filters = &.{}  }},
        "void test(){}\x00"
    ));
}
