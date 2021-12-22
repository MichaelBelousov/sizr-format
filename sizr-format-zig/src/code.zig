/// ~~byte~~code for sizr-format

const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

comst Writer = std.io.Writer;

const expect = @import("std").testing.expect;
const expectError = @import("std").testing.expectError;

const util = @import("./util.zig");

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
        test_: FilterExpr,
        then: ?*WriteCommand,
        else_: ?*WriteCommand,
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
};

fn Resolver(
  comptime Ctx: typename,
  comptime State: typename,
  comptime resolveFn: fn(state: State, ctx: Ctx) Value
) type {
  return struct {
    pub fn resolve(state: State, ctx: Ctx) {
      resolveFn(state, ctx);
    }
  };
};

const nodeTypes = HashMap([]const u8, u16).init();

// TODO: use treesitter here
struct Node {
  type: u16,
  namedChildren: HashMap([]const u8, *Node)
};

struct LangResolver {
  fn resolveFn(self: @This(), node: Node) Value {

  }
  pub const Resolver = Resolver(@This(), NodeType, resolveFn);
};

struct EvalCtx {
  indentLevel: u32;
  aligners: HashMap([]const u8, u32),
  // could layer resolvers, e.g. getting linesep vars from osEnv
  varResolver: LangVarResolver(Cpp),

  fn eval(self: Self, ctx: Cpp.Node) {
    self.varResolver.resolve(ctx);
  }

  fn test_(self: Self, ctx: Cpp.Node) {
    self.eval(ctx) == Val.true;
  }

};

pub fn write(evalCtx: EvalCtx, cmd: WriteCommand, writer: Writer) {
    switch (cmd) {
            WriteCommand.raw => |val| writer.write(val),
            WriteCommand.referenceExpr => |val| writer.write(evalCtx.eval(val)),
            WriteCommand.wrapPoint => writer.write(evalCtx.tryWrap()),
            WriteCommand.conditional => |val| write(evalCtx, evalCtx.eval(evalCtxrtest_(val.test_)) ? val.then : val.else_, writer),
            WriteCommand.indentMark => |val| evalCtx.indent(val),
            WriteCommand.sequence => |cmds| for (cmds) |cmd| { write(ctx, cmd, writer) },
        
}
