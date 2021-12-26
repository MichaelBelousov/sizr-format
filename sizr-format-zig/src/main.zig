const std = @import("std");
const sizr_parser = @import("./parser.zig");
const code = @import("./code.zig");
const ts = @import("./tree_sitter.zig");

test "" {
    _ = code;
    _ = ts;
}

pub fn main() !void {
    const parser = ts.Parser.new();
    _ = parser;
}
