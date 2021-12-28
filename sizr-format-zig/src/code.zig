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
        name: Expr,
        // FIXME: this should actually be a more specific type than List(Value), since only certain value types are allowed
        filters: []const Value, // comma-separated
    },
    wrapPoint,
    conditional: struct {
        @"test": Expr,
        then: ?*WriteCommand,
        @"else": ?*WriteCommand,
    },
    indentMark: IndentMark,
    sequence: []const WriteCommand,
};

/// TODO: replace with FilterExpr (or otherwise merge them, maybe the "Value" name is better)
const Value = union(enum) {
    boolean: bool,
    string: []const u8,
    regex: []const u8,
    integer: i64,
    float: f64,
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
            .boolean => |val| SerializedBlob{.buf = if (val) "true" else "false", .alloc = null },
            .string => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "\"{s}\"", .{val}) catch unreachable, .alloc = alloc }, // ignoring oom for now
            .regex => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "/{s}/", .{val}) catch unreachable, .alloc = alloc },
            .integer => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "{}", .{val}) catch unreachable, .alloc = alloc },
            .float => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "{}", .{val}) catch unreachable, .alloc = alloc },
            .node => |val| SerializedBlob{.buf = _: {
                if (val.@"null"()) break: _ "<NULL_NODE>";
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

        const debug_str = node.string();
        std.debug.print("{s}\n", .{debug_str.ptr});
        defer debug_str.free();

        return if (std.meta.eql(expr, "type")) blk: {
            const cstr = ts._c.ts_node_type(node._c);
            const len = std.mem.len(cstr);
            break :blk Value{ .string = cstr[0..len] };
        } else if (std.fmt.parseInt(u32, expr, 10)) |index| (if (index >= 0)
            _: {
            const maybe_field_name = node.field_name_for_child(index);
            if (maybe_field_name) |field_name| {
                std.debug.print("field {} is named '{s}'\n", .{index, field_name});
            } else {
                std.debug.print("field {} had null name\n", .{index});
            }
            break :_ Value{ .node = ts.Node{ ._c = ts._c.ts_node_child(node._c, index) } };
            }
        else
            unreachable // it is expected that the parser of the expression will reject negative indices
        ) else |_| Value{ .node = node.child_by_field_name(expr) };
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
                const serialized = eval_result.serialize(std.heap.c_allocator, evalCtx);
                defer serialized.free();
                _ = try writer.write(serialized.buf);
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
        fn writeEqlString(self: *@This(), wcmd: WriteCommand, output: []const u8) bool {
            const bufWriter = std.io.fixedBufferStream(&self.buf).writer();
            write(self.ctx, wcmd, bufWriter) catch unreachable;
            bufWriter.writeByte(0) catch unreachable;
            const len = 1 + (std.mem.indexOf(u8, self.buf[0..], "\x00") orelse self.buf.len);
            std.debug.print("buf content: {s}\n", .{self.buf[0..len]});
            return std.mem.eql(u8, self.buf[0..len], output);
        }
    }{
        .ctx = EvalCtx.init(test_util.simpleTestSource),
        .buf = undefined,
    };

    // try expect(local.writeEqlString(
    //     WriteCommand{ .raw = "test" },
    //     "test\x00"
    // ));
    // try expect(local.writeEqlString(
    //     WriteCommand{ .referenceExpr = .{ .name = "0", .filters = &.{}  }},
    //     "void test(){}\x00"
    // ));
    try expect(local.writeEqlString(
        WriteCommand{ .referenceExpr = .{ .name = "0", .filters = &.{}  }},
        "void test(){}\x00"
    ));
    try expect(local.writeEqlString(
        WriteCommand{ .sequence = &.{ WriteCommand{.raw = "test"}, WriteCommand{.raw = "("}, WriteCommand{.raw=" )"} } },
        "test( )\x00"
    ));
    // still need to be tested:
    // - referenceExpr
    // - wrapPoint,
    // - conditional
    // - indentMark: IndentMark,
}
