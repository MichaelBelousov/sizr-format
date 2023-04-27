const std = @import("std");
const ts = @import("tree-sitter");
const mman = @cImport({
    @cInclude("sys/mman.h");
});

const ExecQueryResult = struct {
    parse_tree: ts.Tree,
    query_match_iter: ts.QueryMatchesIterator,
    matches: [*:null]?*const ts._c.TSQueryMatch,
    buff: [8192]u8,
};

/// free a malloc'ed ExecQueryResult
export fn free_ExecQueryResult(r: *ExecQueryResult) void {
    r.parse_tree.delete();
    r.query_match_iter.free();
    const match_count = std.mem.len(r.matches);
    for (r.matches[0..match_count]) |maybe_match| {
        if (maybe_match) |match| std.heap.c_allocator.destroy(match);
    }
    // NOTE: maybe should use actual std.c.free (and malloc)?
    std.heap.c_allocator.destroy(r);
}

export fn matches_ExecQueryResult(r: *ExecQueryResult) [*:null]?*const ts._c.TSQueryMatch {
    return r.matches;
}

/// Caller is responsible for std.c.free'ing the result
export fn node_source(_node: ts._c.TSNode, ctx: *const ExecQueryResult) [*c]const u8 {
    const node = ts.Node { ._c = _node };
    const source = node.in_source(&ctx.buff);
    const result = std.heap.c_allocator.allocSentinel(u8, source.len, 0) catch |err| {
        std.debug.print("node_source allocSentinel err {any}", .{err});
        return null;
    };
    std.mem.copy(u8, result[0..source.len], source);
    // confusing...
    return @as([*:0]const u8, result);
}


/// Caller must use libc free to free each object pointed to by the returned list,
/// as well as the returned list itself
export fn exec_query(
    query: [*:0]const u8,
    srcs: [*c][*:0]const u8
) ?*ExecQueryResult {
    // FIXME: replace these catches
    const file = std.fs.cwd().openFileZ(srcs[0], .{}) catch {
        std.debug.print("openFileZ failed", .{});
        return null;
    };
    // compiler error
    //_ = std.mem.len(srcs);
    defer file.close();

    var file_len = (file.stat() catch {
        std.debug.print("stat file failed", .{});
        return null;
    }).size;
    _ = file_len;

    var result = std.heap.c_allocator.create(ExecQueryResult) catch |err| {
        std.debug.print("couldn't alloc ExecQueryResult because {any}", .{err});
        return null;
    };

    _ = file.readAll(&result.buff) catch |err| {
        std.debug.print("readAll fail {any}", .{err});
        return null;
    };

    const src = &result.buff;

    // var src_ptr = @alignCast(
    //     std.mem.page_size,
    //     std.c.mmap(null, file_len, mman.PROT_READ, mman.MAP_FILE, file.handle, 0)
    // );
    //var src = @ptrCast([*]const u8, src_ptr)[0..file_len];
    // defer {
    //     var result = std.c.getErrno(std.c.munmap(src_ptr, file_len));
    //     if (result != .SUCCESS)
    //         std.debug.print("munmap errno: {any}", .{ result });
    // }

    const parser = ts.Parser.new();
    defer parser.free();
    if (!parser.set_language(ts.cpp()))
        @panic("couldn't set cpp lang");

    result.parse_tree = parser.parse_string(null, src);
    const root = result.parse_tree.root_node();
    const syntax_tree_str = root.string();
    defer syntax_tree_str.free();

    const query_len = std.mem.len(query);
    result.query_match_iter = root.exec_query(query[0..query_len]) catch {
        std.debug.print("openFileZ failed", .{});
        return null;
    };

    var list = std.SegmentedList(*ts.QueryMatch, 16){};
    defer list.deinit(std.heap.c_allocator);

    while (result.query_match_iter.next()) |match| {
        const match_slot = std.heap.c_allocator.create(ts.QueryMatch) catch |err| {
            std.debug.print("c_allocator create err: {any}", .{err});
            return null;
        };
        match_slot.* = match;

        // copy onto heap
        list.append(std.heap.c_allocator, match_slot) catch |err| {
            std.debug.print("add to list err: {any}", .{err});
            return null;
        };

        var i: usize = 0;
        while (i < match._c.capture_count) : (i += 1) {
            const capture_node = ts.Node{._c = match._c.captures[i].node};
            const capture_str = capture_node.string();
            defer capture_str.free();
        }
    }

    result.matches = std.heap.c_allocator.allocSentinel(?*ts._c.TSQueryMatch, list.len, null) catch |err| {
        std.debug.print("allocSentinel err {any}", .{err});
        return null;
    };
    var list_iter = list.iterator(0);

    var i: usize = 0;
    while (list_iter.next()) |val| {
        // FIXME: remove usage of the wrapping ts.QueryMatch
        result.matches[i] = &val.*._c;
        i += 1;
    }

    return result;
}

