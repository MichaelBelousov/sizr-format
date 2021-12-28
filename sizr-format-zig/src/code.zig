/// ~~byte~~code for sizr-format
const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

const Writer = std.io.Writer;
const StringArrayHashMap = std.StringArrayHashMap;

const expect = @import("std").testing.expect;
const expectError = @import("std").testing.expectError;

const util = @import("./util.zig");
const test_util = @import("./test_util.zig");
const ts = @import("./tree_sitter.zig");

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

const BinOp = enum {
    add,
    sub,
};

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

const WriteCommand = union(enum) {
    raw: []const u8,
    referenceExpr: struct {
        name: []const u8,
        // FIXME: this should actually be a more specific type than List(Value), since only certain value types are allowed
        filters: []const Value, // comma-separated
    },
    wrapPoint,
    conditional: struct {
        @"test": Expr, // should be Value
        then: ?*WriteCommand,
        @"else": ?*WriteCommand,
    },
    indentMark: IndentMark,
    sequence: []const WriteCommand,
};

/// TODO: replace with FilterExpr
const Value = union(enum) {
    boolean: bool,
    string: []const u8,
    regex: []const u8,
    integer: i64,
    float: f64,
    node: struct {
        node: ts.Node,
        // acts as a partial evalCtx
        src: []const u8,
    },

    pub fn serialize(self: @This(), alloc: std.mem.Allocator) []const u8 {
        return switch (self) {
            .boolean => |val| if (val) "true" else "false",
            .string => |val| std.fmt.allocPrint(alloc, "\"{s}\"", .{val}) catch unreachable, // FIXME: ignoring oom for now
            .regex => |val| std.fmt.allocPrint(alloc, "/{s}/", .{val}) catch unreachable,
            .integer => |val| std.fmt.allocPrint(alloc, "{}", .{val}) catch unreachable,
            .float => |val| std.fmt.allocPrint(alloc, "{}", .{val}) catch unreachable,
            .node => |val| _: {
                const start = ts._c.ts_node_start_byte(val.node._c);
                const end = ts._c.ts_node_end_byte(val.node._c);
                break :_ val.src[start..end];
            },
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

// should be able to get this from tree-sitter's language objects
const nodeTypes = StringArrayHashMap(u16).init(std.heap.c_allocator);

const Expr = []const u8;

const LangResolver = struct {
    const Self = @This();

    // temp
    source: []const u8,

    resolver: Resolver(ts.Node),

    fn resolveFn(resolver: Resolver(ts.Node), node: ts.Node, expr: Expr) ?Value {
        const self = @fieldParentPtr(Self, "resolver", &resolver);
        _ = self;
        return if (std.meta.eql(expr, "type")) blk: {
            const cstr = ts._c.ts_node_type(node._c);
            const len = std.mem.len(cstr);
            break :blk Value{ .string = cstr[0..len] };
        } else if (std.fmt.parseInt(u32, expr, 10)) |parsed| (if (parsed >= 0)
            Value{ .node = .{ .node = ts.Node{ ._c = ts._c.ts_node_child(node._c, parsed) }, .src = self.source } }
        else
            unreachable // it is expected that the lexer of the expression will reject negative indices
        ) else |_| Value{ .node = .{ .node = ts.Node{ ._c = ts._c.ts_node_child_by_field_name(node._c, expr.ptr, @truncate(u32, expr.len)) }, .src = self.source } };
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
    aligners: StringArrayHashMap(u32),
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
            .node => true, // NOTE: these can in fact probably be invalid, need to check that here
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
            .aligners = StringArrayHashMap(u32).init(std.heap.c_allocator),
            .varResolver = LangResolver.init(source),
        };
    }

    // FIXME: learn exact idiomatic naming of dealloc
    /// free memory associated with this
    pub fn deinit(self: Self) void {
        self.parser.free();
        ts._c.ts_tree_delete(self.tree._c);
    }

    pub fn tryWrap(self: Self) []const u8 {
        _ = self;
        @panic("unimplemented");
    }

    pub fn indent(self: Self, indentVal: IndentMark) []const u8 {
        _ = self;
        _ = indentVal;
        @panic("unimplemented");
    }
};

test "EvalCtx" {
    const ctx = EvalCtx.init(test_util.simpleTestSource);
    _ = ctx;
}

pub fn write(
    // TODO: should probably be a writer that handles wrapping
    evalCtx: EvalCtx,
    cmd: WriteCommand,
    /// expected to be an std.io.Writer
    writer: anytype,
) @TypeOf(writer).Error!void {
    switch (cmd) {
        .raw => |val| {
            _ = try writer.write(val);
        },
        .referenceExpr => |val| {
            const maybe_eval_result = evalCtx.eval(val.name);
            if (maybe_eval_result) |eval_result| {
                const serialized = eval_result.serialize(std.heap.c_allocator);
                defer std.heap.c_allocator.free(serialized);
                _ = try writer.write(serialized);
            }
        },
        .wrapPoint => {
            _ = try writer.write(evalCtx.tryWrap());
        },
        .conditional => |val| {
            if (evalCtx.@"test"(val.@"test") and val.then != null) {
                _ = try write(evalCtx, val.then.?.*, writer);
            } else if (val.@"else" != null) {
                _ = try write(evalCtx, val.@"else".?.*, writer);
            }
        },
        .indentMark => |val| {
            _ = evalCtx.indent(val);
        },
        .sequence => |cmds| {
            for (cmds) |sub_cmd| {
                _ = try write(evalCtx, sub_cmd, writer);
            }
        },
    }
}

test "write" {
    var local = struct {
        ctx: EvalCtx,
        buf: [1024]u8,
        fn writeAndCmp(self: *@This(), wcmd: WriteCommand, output: []const u8) !void {
            const bufWriter = std.io.fixedBufferStream(&self.buf).writer();
            write(self.ctx, wcmd, bufWriter) catch unreachable;
            bufWriter.writeByte(0) catch unreachable;
            try expect(std.mem.eql(u8, self.buf[0..std.mem.len(self.buf)], output));
        }
    }{
        .ctx = EvalCtx.init(test_util.simpleTestSource),
        .buf = undefined,
    };

    try local.writeAndCmp(
        WriteCommand{ .raw = "test" },
        "test\x00"
    );
    try local.writeAndCmp(
        WriteCommand{ .referenceExpr = .{ .name = "test", .filters = &.{}  }},
        "blah\x00"
    );
    try local.writeAndCmp(
        WriteCommand{ .sequence = &.{ WriteCommand{.raw = "test"}, WriteCommand{.raw = "("}, WriteCommand{.raw=" )"} } },
        "test( )\x00"
    );
    // still need to be tested:
    // - referenceExpr
    // - wrapPoint,
    // - conditional
    // - indentMark: IndentMark,
}
