const std = @import("std");
const expect = @import("std").testing.expect;

const ts = @import("./tree_sitter.zig");

const BinOp = enum {
    dot,
};

const Expr = union(enum) {
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

    pub fn serialize(self: @This(), alloc: std.mem.Allocator, evalCtx: EvalCtx) SerializedBlob {
        return switch (self) {
            .string => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "\"{s}\"", .{val}) catch unreachable, .alloc = alloc }, // ignoring oom for now
            .node => |val| SerializedBlob{.buf = _: {
                if (val.@"null"()) break: _ "<NULL_NODE>"; // FIXME: might be better to spit out an empty string
                const start = ts._c.ts_node_start_byte(val._c);
                const end = ts._c.ts_node_end_byte(val._c);
                break :_  evalCtx.source[start..end];
            }, .alloc = null },
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

const LangResolver = struct {
    const Self = @This();

    // temp
    source: []const u8,

    resolver: Resolver(ts.Node),

    fn resolveFn(resolver: Resolver(ts.Node), node: ts.Node, expr: Expr) ?Value {
        const self = @fieldParentPtr(Self, "resolver", &resolver);
        _ = self;

        const debug_str = node.string();
        std.debug.print("{s}\n", .{debug_str.ptr});
        defer debug_str.free();

        return switch(expr) {
            .name => |name|
                if (std.meta.eql(name, "type")) blk: {
                    const cstr = ts._c.ts_node_type(node._c);
                    const len = std.mem.len(cstr);
                    break :blk Value{ .string = cstr[0..len] };
                } else if (std.fmt.parseInt(u32, name, 10)) |index| (
                    // the parser will reject negative indices
                    if (index < 0) unreachable else _: {
                        const maybe_field_name = node.field_name_for_child(index);
                        if (maybe_field_name) |field_name| {
                            std.debug.print("field {} is named '{s}'\n", .{index, field_name});
                        } else {
                            std.debug.print("field {} had null name\n", .{index});
                        }
                        break :_ Value{ .node = ts.Node{ ._c = ts._c.ts_node_child(node._c, index) } };
                    }
                ) else |_| Value{ .node = node.child_by_field_name(name) },
            .binop => |op| switch (op.op) {
                .dot => {
                    const left = resolveFn(self.resolver, node, op.left.*);
                    if (left == null or left.? != .node) @panic("only nodes are supported on the left side of a `.` expr right now");
                    if (op.right.* != .name) unreachable; // "right hand side of a `.` expr must be a name"
                    return resolveFn(self.resolver, left.?.node, op.right.*);
                },
            },
        };
    }

    pub fn init(source: []const u8) Self {
        return Self{ .resolver = Resolver(ts.Node){ .resolveFn = Self.resolveFn }, .source = source };
    }
};

const EvalCtx = struct {
    source: []const u8,
    tree: ts.Tree,
    parser: ts.Parser,
    nodeCursor: ts._c.TSTreeCursor,
    indentLevel: u32,
    // could layer resolvers, e.g. getting linesep vars from an outer osEnv
    varResolver: LangResolver,

    const Self = @This();

    pub fn eval(self: Self, expr: Expr) ?Value {
        //const node = self.nodeCursor
        const node = ts.Node{ ._c = ts._c.ts_tree_cursor_current_node(&self.nodeCursor) };
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
        const parser = ts.Parser.new();
        if (!parser.set_language(ts.cpp()))
            @panic("couldn't set cpp lang");
        const tree = parser.parse_string(null, source);
        const root = ts._c.ts_tree_root_node(tree._c);
        const cursor = ts._c.ts_tree_cursor_new(root);
        return Self{
            .source = source,
            .parser = parser,
            .tree = tree,
            .nodeCursor = cursor,
            .indentLevel = 0,
            .varResolver = LangResolver.init(source),
        };
    }

    // FIXME: learn exact idiomatic naming of dealloc
    /// free memory associated with this
    pub fn deinit(self: Self) void {
        self.parser.free();
        ts._c.ts_tree_delete(self.tree._c);
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
                const serialized = eval_result.serialize(std.heap.c_allocator, evalCtx);
                defer serialized.free();
                _ = try writer.write(serialized.buf);
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
