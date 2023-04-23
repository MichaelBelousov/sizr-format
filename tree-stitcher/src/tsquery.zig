const std = @import("std");
const ts = @import("tree-sitter");

pub fn main() !void {
    if (std.os.argv.len != 3) {
        std.log.info("Usage: query <QUERY> <SOURCE_PATH>", .{});
        for (std.os.argv) |arg| {
            std.debug.print("arg: {s}\n", .{arg});
        }
        return error.BadArgs;
    }

    const query = std.os.argv[1];
    const path = std.os.argv[2];

    const file = try std.fs.cwd().openFileZ(path, .{});
    defer file.close();

    //var src: [8192]u8 = undefined;
    //const bytes_read = try file.readAll(&src);
    var src = std.c.mmap(path);

    // if (bytes_read >= src.len) {
    //     std.log.err("File was too long", .{});
    //     return error.FileTooLong;
    // }
    //src[src.len - 1] = '\x00';

    const parser = ts.Parser.new();
    defer parser.free();
    if (!parser.set_language(ts.cpp()))
        @panic("couldn't set cpp lang");

    const tree = parser.parse_string(null, &src);
    defer tree.delete();
    const root = tree.root_node();
    const syntax_tree_str = root.string();
    defer syntax_tree_str.free();
    std.debug.print("syntax_tree: '{s}'\n", .{syntax_tree_str.ptr});

    var query_len = std.mem.len(query);
    var query_match_iter = try root.exec_query(query[0..query_len]);

    while (query_match_iter.next()) |match| {
        std.debug.print("match: {any}\n", .{match});
        var i: usize = 0;
        while (i < match._c.capture_count) : (i += 1) {
            const capture_node = ts.Node{._c = match._c.captures[i].node};
            const capture_str = capture_node.string();
            defer capture_str.free();
            std.debug.print("capture: {s}\n", .{capture_str.ptr});
            std.debug.print("capture source: {s}\n", .{capture_node.in_source(&src)});
        }
    } else {
        std.debug.print("no more matches\n", .{});
    }
}

