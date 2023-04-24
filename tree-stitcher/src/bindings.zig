const std = @import("std");
const ts = @import("tree-sitter");

export fn my_test(x: i32) [*]const u8 {
    _ = x;
    return "hello!";
}

export fn exec_query(
    query: [*:0]const u8,
    query_len: usize,
    srcs: [*c][*:0]const u8,
    srcs_count: usize
) ?*const ts.QueryMatch {
    // FIXME: replace these catches
    const file = std.fs.cwd().openFileZ(srcs[0], .{}) catch {
        std.debug.print("openFileZ failed", .{});
        return null;
    };
    _ = srcs_count;
    defer file.close();

    var src: [8192]u8 = undefined;
    const bytes_read = file.readAll(&src) catch {
        std.debug.print("openFileZ failed", .{});
        return null;
    };
    // TODO: mmap
    // var src = std.c.mmap(path);

    if (bytes_read >= src.len) {
        std.log.err("File was too long", .{});
        return null;
    }
    src[src.len - 1] = '\x00';

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

    var query_match_iter = root.exec_query(query[0..query_len]) catch {
        std.debug.print("openFileZ failed", .{});
        return null;
    };

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
        return &match;
    } else {
        std.debug.print("no more matches\n", .{});
    }

    return null;
}

