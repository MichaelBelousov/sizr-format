const std = @import("std");
const ts = @import("./src/tree_sitter.zig");

pub fn main() !void {
  if (std.os.argv.len != 3) {
    std.log.info("Usage: query <QUERY> <SOURCE_PATH>", .{});
    return error.BadArgs;
  }

  const query = std.os.argv[1];
  std.debug.print("query: {s}", .{query});

  const path = std.os.argv[2];
  std.debug.print("path: {s}", .{path});

  const file = try std.fs.cwd().openFileZ(path, .{});
  defer file.close();

  var src: [8192]u8 = undefined;
  const bytes_read = try file.read(&src);

  if (bytes_read == src.len) {
    std.log.err("File was too long", .{});
    return error.FileTooLong;
  }

  src[src.len - 1] = '\x00';

  const parser = ts.Parser.new();
  defer parser.free();
  if (!parser.set_language(ts.cpp()))
    @panic("couldn't set cpp lang");
  const tree = parser.parse_string(null, &src);
  defer ts._c.ts_tree_delete(tree._c);
  const root = ts._c.ts_tree_root_node(tree._c);
  const syntax_tree = ts._c.ts_node_string(root);
  defer std.c.free(syntax_tree);
  std.debug.print("syntax_tree: '{s}'\n", .{syntax_tree});
}

