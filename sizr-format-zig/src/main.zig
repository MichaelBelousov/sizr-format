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
    var tree = parser.parse_string(null, src);
    std.debug.print("result: '{}'\n", .{tree});
}

pub fn main() !void {
}