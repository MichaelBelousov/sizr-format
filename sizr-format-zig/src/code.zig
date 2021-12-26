/// ~~byte~~code for sizr-format
const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

const Writer = std.io.Writer;
const StringArrayHashMap = std.StringArrayHashMap;

const expect = @import("std").testing.expect;
const expectError = @import("std").testing.expectError;

const util = @import("./util.zig");
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
        name: []const u8,
        filters: std.ArrayList(FilterExpr), // comma-separated
    },
    wrapPoint,
    conditional: struct {
        @"test": FilterExpr,
        then: ?*WriteCommand,
        @"else": ?*WriteCommand,
    },
    indentMark: IndentMark,
    sequence: std.ArrayList(WriteCommand),
};

const Value = union(enum) {
    boolean: bool,
    string: []const u8,
    regex: []const u8,
    integer: i64,
    float: f64,
    node: ts.Node,
};

// TODO: use idiomatic zig polymorphism
fn Resolver(
    comptime Ctx: type,
) type {
    return struct {
        const Self = @This();
        resolveFn: fn (self: Self, ctx: Ctx, @"expr": []const u8) Value,
        pub fn resolve(self: Self, ctx: Ctx, @"expr": []const u8) Value {
            self.resolveFn(ctx, @"expr");
        }
    };
}

// should be able to get this from tree-sitter's language objects
const nodeTypes = StringArrayHashMap(u16).init(std.heap.c_allocator);

const LangResolver = struct {
    const Self = @This();

    resolver: Resolver(ts.Node),

    fn resolveFn(resolver: Resolver(ts.Node), node: ts.Node, @"expr": []const u8) Value {
        const self = @fieldParentPtr(Self, "resolver", &resolver);
        _ = self;
        return if (std.meta.eql(@"expr", "type")) blk: {
            const cstr = ts._c.ts_node_type(node._c.*);
            const len = std.mem.len(cstr);
            break :blk Value{ .string = cstr[0..len] };
        } else if (std.fmt.parseInt(u32, @"expr", 10)) |parsed| (if (parsed >= 0)
            Value{ .node = ts.Node{ ._c = &ts._c.ts_node_child(node._c.*, parsed) } }
        else
            unreachable // it is expected that the lexer of the expression will reject negative indices
        ) else |_| Value{ .node = ts.Node{ ._c = &ts._c.ts_node_child_by_field_name(node._c.*, @"expr".ptr, @truncate(u32, @"expr".len)) } };
    }

    pub fn init() Self {
        return Self{ .resolver = Resolver(ts.Node){ .resolveFn = Self.resolveFn } };
    }
};

const EvalCtx = struct {
    indentLevel: u32,
    aligners: StringArrayHashMap(u32),
    // could layer resolvers, e.g. getting linesep vars from osEnv
    varResolver: LangResolver,

    const Self = @This();

    pub fn eval(self: Self, ctx: ts.Node) Value {
        self.varResolver.resolve(ctx);
    }

    pub fn @"test"(self: Self, ctx: ts.Node) Value {
        self.eval(ctx) == Value{ .bool = true };
    }

    pub fn init() Self {
        return Self{
            .indentLevel = 0,
            .aligners = StringArrayHashMap(u32).init(std.heap.c_allocator),
            .varResolver = LangResolver.init(),
        };
    }
};

test "EvalCtx" {
    const ctx = EvalCtx.init();
    _ = ctx;
}

pub fn write(evalCtx: EvalCtx, cmd: WriteCommand, writer: Writer) void {
    switch (cmd) {
        .raw => |val| writer.write(val),
        .referenceExpr => |val| writer.write(evalCtx.eval(val)),
        .wrapPoint => writer.write(evalCtx.tryWrap()),
        .conditional => |val| {
            write(evalCtx, if (evalCtx.eval(evalCtx.@"test"(val.@"test"))) val.then else val.@"else", writer);
        },
        .indentMark => |val| evalCtx.indent(val),
        .sequence => |cmds| for (cmds) |c| {
            write(evalCtx, c, writer);
        },
    }
}
