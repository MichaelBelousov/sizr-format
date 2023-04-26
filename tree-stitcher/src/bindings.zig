const std = @import("std");
const ts = @import("tree-sitter");
const mman = @cImport({
    @cInclude("sys/mman.h");
});

export fn my_test(x: i32) [*]const u8 {
    _ = x;
    return "hello!";
}

export fn free_query_match(match: *ts.QueryMatch) void {
    // FIXME: not freed yet because owning container needs to be freed
    _ = match;
    //std.heap.c_allocator.destroy(match);
}

export fn exec_query(
    query: [*:0]const u8,
    srcs: [*c][*:0]const u8
) ?[*:null]?*const ts.QueryMatch {
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

    var src_ptr = @alignCast(
        std.mem.page_size,
        std.c.mmap(null, file_len, mman.PROT_READ, mman.MAP_FILE, file.handle, 0)
    );
    var src = @ptrCast([*]const u8, src_ptr)[0..file_len];
    defer {
        var result = std.c.getErrno(std.c.munmap(src_ptr, file_len));
        if (result != .SUCCESS)
            std.debug.print("munmap errno: {any}", .{ result });
    }

    const parser = ts.Parser.new();
    defer parser.free();
    if (!parser.set_language(ts.cpp()))
        @panic("couldn't set cpp lang");

    const tree = parser.parse_string(null, src);
    defer tree.delete();
    const root = tree.root_node();
    const syntax_tree_str = root.string();
    defer syntax_tree_str.free();
    std.debug.print("syntax_tree: '{s}'\n", .{syntax_tree_str.ptr});

    const query_len = std.mem.len(query);
    var query_match_iter = root.exec_query(query[0..query_len]) catch {
        std.debug.print("openFileZ failed", .{});
        return null;
    };

    var list = std.SegmentedList(ts.QueryMatch, 16){};
    defer list.deinit(std.heap.c_allocator); // TODO: use arena allocator

    while (query_match_iter.next()) |match| {
        const match_slot = std.heap.c_allocator.create(ts.QueryMatch) catch |err| {
            std.debug.print("c_allocator create err: {any}", .{err});
            return null;
        };
        match_slot.* = match;

        // copy onto heap
        list.append(std.heap.c_allocator, match) catch |err| {
            std.debug.print("add to list err: {any}", .{err});
            return null;
        };

        std.debug.print("match: {any}\n", .{match});
        var i: usize = 0;
        while (i < match._c.capture_count) : (i += 1) {
            const capture_node = ts.Node{._c = match._c.captures[i].node};
            const capture_str = capture_node.string();
            defer capture_str.free();
            std.debug.print("capture: {s}\n", .{capture_str.ptr});
            std.debug.print("capture source: {s}\n", .{capture_node.in_source(src)});
        }
    } else {
        std.debug.print("no more matches\n", .{});
    }

    // FIXME: leaks container
    const array = std.heap.c_allocator.allocSentinel(?*ts.QueryMatch, list.len, null) catch |err| {
        std.debug.print("allocSentinel err {any}", .{err});
        return null;
    };
    var list_iter = list.iterator(0);

    var i: usize = 0;
    while (list_iter.next()) |val| {
        array[i] = val;
        i += 1;
    }

    return array;
}

