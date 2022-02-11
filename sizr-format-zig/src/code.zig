//! vm for sizr-format

const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

const Writer = std.io.Writer;
const StringArrayHashMap = std.StringArrayHashMap;

const expect = @import("std").testing.expect;
const expectEqualStrings = @import("std").testing.expectEqualStrings;
const expectError = @import("std").testing.expectError;

const util = @import("./util.zig");
const test_util = @import("./test_util.zig");
const ts = @import("./tree_sitter.zig");
const cpp = @import("./cpp.zig");

pub const Literal = union(enum) {
    boolean: bool,
    string: []const u8,
    regex: []const u8,
    integer: i64,
    float: f64,
};

pub const IndentMark = union(enum) {
    indent: u16,
    outdent: u16,
    token_anchor: []const u8,
    numeric_anchor: u16,
};

pub const BinOp = enum {
    add,
    sub,
    dot,
};

pub const Expr = union(enum) {
    rest,
    binop: struct {
        op: BinOp,
        left: *const Expr,
        right: *const Expr,
    },
    unaryop: struct {
        op: BinOp,
        expr: *const Expr,
    },
    noderef: []const u8,
    literal: Literal,
    group: *const Expr,
    name: []const u8,
    all,

    /// create a dot expression from a chain of names
    // FIXME: eww need a better form of compiletime allocation, no?
    // probably use some kind of comptime var iterator of type sizes to get
    // to that end, I maybe can use a comptime var to calculate the necessary allocation size and build a slice to hold it?
    pub fn staticDot(comptime names: anytype) [@typeInfo(@TypeOf(names)).Array.len + 1]Expr {
        const ReturnType = [@typeInfo(@TypeOf(names)).Array.len + 1]Expr;
        var result: ReturnType = undefined;
        const lastIndex = names.len;
        for (names) |name, i| {
            const isFirst = i == 0;
            result[i+1] = Expr{.name = name};
            result[0] = if (isFirst) result[i+1] else Expr{.binop = .{
                .op = .dot,
                .left = &result[lastIndex],
                .right = &result[i],
            }};
        }
        return result;
    }

    /// parse a string into an expression
    pub fn parse(alloc: std.mem.Allocator, source: []const u8) !*Expr {
        // super bare-bones impl only supporting member-of operator for now
        var remaining = source;
        var cur_expr: ?*Expr = null;
        while (true) {
            const index = std.mem.indexOf(u8, remaining, ".") orelse remaining.len;
            const name_expr = Expr{.name = remaining[0..index]};
            if (cur_expr) |cur_expr_val| {
                const prev_expr = cur_expr_val;
                const name_expr_slot = try alloc.create(Expr);
                name_expr_slot.* = name_expr;
                cur_expr = try alloc.create(Expr);
                cur_expr.?.* = Expr{.binop = .{
                    .op = .dot,
                    .left = prev_expr,
                    .right = name_expr_slot,
                }};
            } else {
                cur_expr = try alloc.create(Expr);
                cur_expr.?.* = name_expr;
            }
            if (index == remaining.len) break;
            remaining = remaining[index+1..];
        }
        return cur_expr.?;
    }

    /// recursively free the expression tree, and then the self pointer itself
    /// alloc must be the same allocator that was used when creating this Expr
    pub fn free(self: *const @This(), alloc: std.mem.Allocator) void {
        switch (self.*) {
            .binop => |val| { val.left.free(alloc); val.right.free(alloc); },
            .unaryop => |val| val.expr.free(alloc),
            .group => |val| val.free(alloc),
            else => {}
        }
        alloc.destroy(self);
    }

    pub fn print(self: @This()) void {
        const local = struct {
            fn p(s: @This(), comptime str: []const u8) void { _ = s; std.debug.print(str, .{}); }
        }{};
        switch (self) {
            .binop => |v| {local.p(".binop={{"); v.left.print(); v.right.print(); local.p("}}");},
            .unaryop => |v| {local.p(".unaryop={{"); v.expr.print(); local.p("}}"); },
            .name => |v| std.debug.print(".name={s}", .{v}),
            .all => local.p("all"),
            else => @panic("not supported yet"),
        }
    }

    pub fn eql(self: *const @This(), other: *const @This()) bool {
        return switch (self.*) {
            .binop => |l| (switch (other.*) {
                .binop => |r| l.op == r.op and l.left.eql(r.left) and l.right.eql(r.right) ,
                else => false,
            }),
            .unaryop => |l| switch (other.*) {
                .unaryop => |r| l.op == r.op and l.expr.eql(r.expr),
                else => false,
            },
            .name => |l| switch (other.*) {
                .name => |r| std.mem.eql(u8, l, r),
                else => false,
            },
            .all => (other.* == .all),
            else => @panic("not supported yet"),
        };
    }
};

test "Expr.parse" {
    // TODO: figure out how to specify literals as mutable
    var @"0" = Expr{.name="0"};
    var expr = Expr{.binop = .{ .op = .dot, .left = &Expr{.name="0"}, .right = &Expr{.name="2"}}};

    const parsed1 = try Expr.parse(std.testing.allocator, "0");
    defer parsed1.free(std.testing.allocator);

    try expect(std.meta.eql(parsed1.*, @"0"));

    const parsed = try Expr.parse(std.testing.allocator, "0.2");
    defer parsed.free(std.testing.allocator);

    try expect(parsed.eql(&expr));
}

pub const WriteCommand = union(enum) {
    raw: []const u8,
    ref: struct {
        name: Expr,
        // FIXME: this should actually be a more specific type than List(Value), since only certain value types are allowed
        //filters: []const Value, // comma-separated
    },
    wrapPoint,
    conditional: struct {
        @"test": Expr,
        then: ?*WriteCommand,
        @"else": ?*WriteCommand,
    },
    indentMark: IndentMark,
    sequence: []const WriteCommand,
    // maybe rename to "passthrough" or "opaque" or something?
    /// just print out all children in turn
    trivial
};

/// TODO: dedup with Expr and Literal
const Value = union(enum) {
    boolean: bool,
    string: []const u8,
    regex: []const u8,
    integer: i64,
    float: f64,
    node: ts.Node,

    /// an optionally allocated blob that should be freed, but might that might be a noop
    const SerializedBlob = struct {
        needs_free: bool,
        buf: []const u8,
        pub fn free(self: @This(), alloc: std.mem.Allocator) void {
            if (self.needs_free) alloc.free(self.buf);
        }
    };

    pub fn serialize(self: @This(), alloc: std.mem.Allocator, evalCtx: anytype) SerializedBlob {
        return switch (self) {
            .boolean => |val| SerializedBlob{.buf = if (val) "true" else "false", .needs_free = false },
            .string => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "\"{s}\"", .{val}) catch unreachable, .needs_free = true }, // ignoring oom for now
            .regex => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "/{s}/", .{val}) catch unreachable, .needs_free = true },
            .integer => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "{}", .{val}) catch unreachable, .needs_free = true },
            .float => |val| SerializedBlob{.buf = std.fmt.allocPrint(alloc, "{}", .{val}) catch unreachable, .needs_free = true },
            .node => |val| SerializedBlob{.buf = _: {
                if (val.@"null"()) break: _ "<NULL_NODE>"; // FIXME: might be better to spit out an empty string
                const start = ts._c.ts_node_start_byte(val._c);
                const end = ts._c.ts_node_end_byte(val._c);
                break :_  evalCtx.source[start..end];
            }, .needs_free = false },
        };
    }
};

// stolen from discord
// fat pointer cast I think
pub fn castTo(comptime T: type, ptr: *anyopaque) *T {
    return @ptrCast(*T, @alignCast(@alignOf(T), ptr));
}

// TODO: use idiomatic zig polymorphism (I did actually... just not since allocgate)
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

fn EvalCtx(comptime WriterType: type) type {
    return struct {
        const Self = @This();

        const LangResolver = struct {
            resolver: Resolver(ts.Node),

            fn resolveFn(resolver: Resolver(ts.Node), node: ts.Node, expr: Expr) ?Value {
                const self = @fieldParentPtr(@This(), "resolver", &resolver);
                //const evalCtx = @fieldParentPtr(EvalCtx, "varResolver", &self);

                // TODO: this not taking an allocator is inconsistent... I know it's an underlying libc allocation but maybe a dummy arg is still a good idea?
                const debug_str = node.string();
                dbglogv("{s}\n", .{debug_str.ptr});
                defer debug_str.free();

                // Workaround used here for a bug https://github.com/ziglang/zig/issues/10601
                return switch(expr) {
                    .name => |name|
                        if (std.meta.eql(name, "type")) blk: {
                            const cstr = ts._c.ts_node_type(node._c);
                            const len = std.mem.len(cstr);
                            break :blk @as(?Value, Value{ .string = cstr[0..len] });
                        } else if (std.fmt.parseInt(u32, name, 10)) |index| (
                            // the parser will reject negative indices
                            if (index < 0) unreachable else _: {
                                const maybe_field_name = node.field_name_for_child(index);
                                if (maybe_field_name) |field_name| {
                                    dbglogv("field {} is named '{s}'\n", .{index, field_name});
                                } else {
                                    dbglogv("field {} had null name\n", .{index});
                                }
                                break :_ @as(?Value, Value{ .node = ts.Node{ ._c = ts._c.ts_node_child(node._c, index) } });
                            }
                        ) else |_| @as(?Value, Value{ .node = node.child_by_field_name(name) }),
                    .binop => |op| switch (op.op) {
                        .dot => {
                            const left = resolveFn(self.resolver, node, op.left.*);
                            if (left == null or left.? != .node) @panic("only nodes are supported on the left side of a `.` expr right now");
                            if (op.right.* != .name) unreachable; // "right hand side of a `.` expr must be a name"
                            return @as(?Value, resolveFn(self.resolver, left.?.node, op.right.*));
                        },
                        else => @panic("only dot is implemented!")
                    },
                    .all => @as(?Value, Value{.node = node}),
                    else => @as(?Value, null),
                };
            }

            pub fn init() @This() {
                return @This(){ .resolver = .{ .resolveFn = @This().resolveFn } };
            }
        };

        source: []const u8,
        tree: ts.Tree,
        parser: ts.Parser,
        nodeCursor: ts._c.TSTreeCursor,
        indentLevel: u32,
        aligners: StringArrayHashMap(u32),
        // could layer resolvers, e.g. getting linesep vars from an outer osEnv
        varResolver: LangResolver,
        languageFormat: LanguageFormat,
        desiredLineSize: usize,
        writer: WriterType,

        lineBuffer: []u8,
        lineBufCursor: usize,

        pub fn eval(self: Self, expr: Expr) ?Value {
            //const node = self.nodeCursor
            const node = ts.Node{ ._c = ts._c.ts_tree_cursor_current_node(&self.nodeCursor) };
            return self.varResolver.resolver.resolve(node, expr);
        }

        pub fn evalAndWrite(self: *Self, expr: Expr) !void {
            const maybe_eval_result = self.eval(expr);
            if (maybe_eval_result) |eval_result| {
                // FIXME: take an allocator instance argument so we can track leaks in tests
                const serialized = eval_result.serialize(std.heap.c_allocator, self);
                defer serialized.free(std.heap.c_allocator);
                _ = try self._write(serialized.buf);
            }
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

        pub fn init(
            args: struct {
                source: []const u8,
                writer: WriterType, //std.io.Writer
                allocator: std.mem.Allocator,
                desiredLineSize: u32,
                languageFormat: LanguageFormat,
            }
        ) !Self {
            const parser = ts.Parser.new();
            if (!parser.set_language(ts.cpp()))
                @panic("couldn't set cpp lang");
            const tree = parser.parse_string(null, args.source);
            const root = ts._c.ts_tree_root_node(tree._c);
            const cursor = ts._c.ts_tree_cursor_new(root);
            var result = Self{
                .source = args.source,
                .parser = parser,
                .tree = tree,
                .nodeCursor = cursor,
                .indentLevel = 0,
                .aligners = StringArrayHashMap(u32).init(args.allocator),
                .varResolver = LangResolver.init(),
                .writer = args.writer,
                .lineBuffer = try args.allocator.alloc(u8, args.desiredLineSize),
                .desiredLineSize = args.desiredLineSize,
                .lineBufCursor = 0,
                .languageFormat = args.languageFormat
            };
            return result;
        }

        /// `alloc` MUST be the same allocator that was passed when `init`ing this
        /// TODO: probably should store the allocator ourselves since we need it for _write...
        pub fn free(self: *Self, alloc: std.mem.Allocator) void {
            alloc.free(self.lineBuffer);
            self.parser.free();
            ts._c.ts_tree_delete(self.tree._c);
        }

        pub fn tryWrap(self: *Self) !void {
            if (self.lineBufCursor > self.desiredLineSize) {
                _ = try self.writer.write("\n");
                self.lineBufCursor = 0;
            }
        }

        pub fn indent(self: Self, indentVal: IndentMark) []const u8 {
            _ = self;
            _ = indentVal;
            @panic("unimplemented");
        }

        fn _write(self: *Self, slice: []const u8) WriterType.Error!void {
            self.lineBufCursor += slice.len;
            _ = try self.writer.write(slice);
        }

        /// get the write command registered for the root syntactic construct
        /// and write the source in this EvalCtx
        pub fn writeSrc(self: *Self) WriterType.Error!void {
            try self.writeCmd(self.languageFormat.nodeFormats(self.languageFormat.rootNodeKey));
        }

        // FIXME: need to separate this further from writeSrc
        // TODO: Self should probably be itself a (proxy) writer that handles wrapping
        // could be basically the same as std.io.BufferedWriter
        pub fn writeCmd(self: *Self, cmd: WriteCommand) WriterType.Error!void {
            switch (cmd) {
                .raw => |val| try self._write(val),
                .ref => |val| try self.evalAndWrite(val.name),
                .wrapPoint => try self.tryWrap(),
                .conditional => |val| {
                    if (self.@"test"(val.@"test") and val.then != null) {
                        _ = try self.writeCmd(val.then.?.*);
                    } else if (val.@"else" != null) {
                        _ = try self.writeCmd(val.@"else".?.*);
                    }
                },
                .indentMark => |val| _ = self.indent(val),
                .sequence => |cmds| {
                    for (cmds) |sub_cmd| {
                        _ = try self.writeCmd(sub_cmd);
                    }
                },
                .trivial => try self.evalAndWrite(.all),
            }
        }
    };
}

test "EvalCtx" {
    var ctx = EvalCtx(@TypeOf(std.io.null_writer)).init(.{
        .source = test_util.simpleTestSource,
        .writer = std.io.null_writer,
        .allocator = std.testing.allocator,
        .desiredLineSize = 80,
        .languageFormat = cpp.languageFormat
    }) catch unreachable;
    defer ctx.free(std.testing.allocator);
    _ = ctx;
}

// why don't I just ship the zig compiler itself for plugins rather than make them dynamically loadable,
// dynamically compile them? Brutish approach but might not be that bad...
/// language specific data of how to format a language's AST
pub const LanguageFormat = struct {
    nodeFormats: fn([]const u8) WriteCommand,
    rootNodeKey: []const u8,
};

test "write" {
    // TODO: move to test_util.zig
    var local = struct {
        buf: [1024]u8,

        fn expectWrittenString(self: *@This(), src: []const u8, comptime wcmd: WriteCommand, expected: []const u8) !void {
            dbglog("\n");
            const bufWriter = std.io.fixedBufferStream(&self.buf).writer();
            const TestFormatLanguage = struct { fn nodeFormats(_: []const u8) WriteCommand { return wcmd; } };
            var ctx = EvalCtx(@TypeOf(bufWriter)).init(.{
                .source = src,
                .writer = bufWriter,
                .allocator = std.testing.allocator,
                .desiredLineSize = 60,
                .languageFormat = LanguageFormat{
                    .nodeFormats =  TestFormatLanguage.nodeFormats,
                    .rootNodeKey = "",
                }
            }) catch unreachable;
            defer ctx.free(std.testing.allocator);
            ctx.writeSrc() catch unreachable;
            bufWriter.writeByte(0) catch unreachable;
            const len = 1 + (std.mem.indexOf(u8, self.buf[0..], "\x00") orelse self.buf.len);
            dbglogv("buf content: '{s}'\n", .{self.buf[0..len]});
            return expectEqualStrings(expected, self.buf[0..len], );
        }
    }{
        .buf = undefined,
    };

    try local.expectWrittenString(
        "void test(){}",
        WriteCommand{ .raw = "test" },
        "test\x00"
    );
    try local.expectWrittenString(
        "void test(){}",
        WriteCommand{ .ref = .{ .name = Expr{.name = "0"}  }},
        "void test(){}\x00"
    );
    try local.expectWrittenString(
        "void test(){}",
        WriteCommand{ .ref = .{ .name = Expr{.name = "0"}  }},
        "void test(){}\x00"
    );

    try local.expectWrittenString(
        "void test(){}",
        WriteCommand{ .ref = .{ .name = Expr{.binop = .{.op = .dot, .left = &Expr{.name="0"}, .right = &Expr{.name="0"}}}  }},
        "void\x00"
    );

    const expr2 = try Expr.parse(std.testing.allocator, "0.2");
    defer expr2.free(std.testing.allocator);

    try local.expectWrittenString(
        "void test(){}",
        WriteCommand{ .ref = .{ .name = Expr{.binop = .{.op = .dot, .left = &Expr{.name="0"}, .right = &Expr{.name="1"}}}  }},
        "test()\x00"
    );

    try local.expectWrittenString(
        "void test(){}",
        WriteCommand{ .sequence = &.{ WriteCommand{.raw = "test"}, WriteCommand{.raw = "("}, WriteCommand{.raw=" )"} } },
        "test( )\x00"
    );

    const funcname = Expr{.binop = .{.op = .dot, .left = &Expr{.binop = .{.op = .dot, .left = &Expr{.name="0"}, .right = &Expr{.name="declarator"}}}, .right = &Expr{.name="declarator"}}};
    const params = Expr{.binop = .{.op = .dot, .left = &Expr{.binop = .{.op = .dot, .left = &Expr{.name="0"}, .right = &Expr{.name="declarator"}}}, .right = &Expr{.name="parameters"}}};
    const body = Expr{.binop = .{.op = .dot, .left = &Expr{.name = "0"}, .right = &Expr{.name="body"}}};

    try local.expectWrittenString(
        "void someRidiculouslyLongFunctionNameLikeForRealWhatsUpWhyShouldItBeThisLong ()     {     }  ",
        // must use an explicit slice instead of tuple literal to avoid a compiler bug
        WriteCommand{ .sequence = &[_]WriteCommand{
            WriteCommand{.ref = .{.name=funcname}},
            WriteCommand.wrapPoint,
            WriteCommand{.ref = .{.name=params}},
            WriteCommand.wrapPoint,
            WriteCommand{.ref = .{.name=body}}
        } },
        "someRidiculouslyLongFunctionNameLikeForRealWhatsUpWhyShouldItBeThisLong\n(){     }\x00"
    );

    // still need to be tested:
    // - wrapPoint,
    // - conditional
    // - indentMark: IndentMark,
}

fn dbglog(comptime str: []const u8) void {
    if (std.os.getenv("DEBUG") != null)
        std.debug.print(str, .{});
}

fn dbglogv(comptime str: []const u8, args: anytype) void {
    if (std.os.getenv("DEBUG") != null)
        std.debug.print(str, args);
}
