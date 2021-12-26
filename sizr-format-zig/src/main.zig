const std = @import("std");
const sizr_parser = @import("./parser.zig");
const code = @import("./code.zig");
const ts = @import("./tree_sitter.zig");

test "" {
    _ = code;
    _ = ts;
}

test "parse" {
    const src =
        \\#include "myfile/blah.h"
        \\
        \\long test(int x) {
        \\  const long i = 5;
        \\  return i + x;
        \\}
    ;
    const parser = ts.Parser.new();
    defer parser.free();
    if (!parser.set_language(ts.cpp()))
        @panic("couldn't set cpp lang");
    const tree = parser.parse_string(null, src);
    defer ts._c.ts_tree_delete(tree._c);
    const root = ts._c.ts_tree_root_node(tree._c);
    const syntax_tree = ts._c.ts_node_string(root);
    defer std.c.free(syntax_tree);
    std.debug.print("syntax_tree: '{s}'\n", .{syntax_tree});
}

pub fn main() !void {
}