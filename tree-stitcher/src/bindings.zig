const std = @import("std");
const ts = @import("tree-sitter");
const mman = @cImport({ @cInclude("sys/mman.h"); });

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
    const file = std.fs.cwd().openFileZ(srcs[0], .{}) catch unreachable;
    // compiler error
    //_ = std.mem.len(srcs);
    defer file.close();

    var file_len = (file.stat() catch unreachable).size;
    _ = file_len;

    var result = std.heap.c_allocator.create(ExecQueryResult) catch unreachable;

    _ = file.readAll(&result.buff) catch unreachable;

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
    result.query_match_iter = root.exec_query(query[0..query_len]) catch unreachable;

    var list = std.SegmentedList(*ts._c.TSQueryMatch, 16){};
    defer list.deinit(std.heap.c_allocator);

    while (result.query_match_iter.next()) |match| {
        const match_slot = std.heap.c_allocator.create(ts._c.TSQueryMatch) catch unreachable;
        match_slot.* = match._c;

        // LEAK? working around that tree-sitter seems to reuse the capture pointer of returned matches when you
        // call next again... but will tree-sitter free it correctly later?
        const newCaptures = std.heap.c_allocator.alloc(ts._c.TSQueryCapture, match._c.capture_count) catch unreachable;
        std.mem.copy(
            ts._c.TSQueryCapture,
            newCaptures[0..match._c.capture_count],
            match._c.captures[0..match._c.capture_count]
        );
        match_slot.captures = @as([*c]ts._c.TSQueryCapture, &newCaptures[0]);

        list.append(std.heap.c_allocator, match_slot) catch unreachable;
    }

    result.matches = std.heap.c_allocator.allocSentinel(?*ts._c.TSQueryMatch, list.len, null) catch |err| {
        std.debug.print("allocSentinel err {any}", .{err});
        return null;
    };
    var list_iter = list.iterator(0);

    var i: usize = 0;
    while (list_iter.next()) |val| {
        result.matches[i] = val.*;
        i += 1;
    }

    return result;
}

// file issue on zig about all this stuff not working
//const chibi = @cImport({ @cInclude("chibi/eval.h"); });
const chibi = @cImport({ @cInclude("chibi_macros.h"); });

export fn transform_ExecQueryResult(r: *ExecQueryResult, transform: chibi.sexp, ctx: chibi.sexp) [*c]const u8 {
    var result = std.heap.c_allocator.allocSentinel(u8, 8192, 0) catch unreachable;
    var writer = std.io.fixedBufferStream(result);
    chibi._sexp_debug(ctx, "transform arg:", transform);
    std.debug.print("length: {any}\n", .{ chibi._sexp_length_unboxed(transform) });

    const match_count = std.mem.len(r.matches);
    var i: usize = 0;
    for (r.matches[0..match_count]) |maybe_match| {
        if (maybe_match) |match| {
            const outer_capture = match.captures[match.capture_count - 1];
            _ = outer_capture.node;
            const start = ts._c.ts_node_start_byte(outer_capture.node);
            const end = ts._c.ts_node_end_byte(outer_capture.node);
            std.debug.print("chunk: {d}:{d}\n", .{i, start});
            _ = writer.write(r.buff[i..start]) catch unreachable;
            i = end;

            chibi._sexp_debug(ctx, "transform arg:", chibi._sexp_car(transform));
            //std.debug.print("symbol1: {any}\n", .{ _sexp_symbol_to_string(null, _sexp_car(null, transform)) });
            //std.debug.print("symbol2: {s}\n", .{ chibi._sexp_string_data(chibi._sexp_symbol_to_string(ctx, transform)) });
            _ = writer.write(r.buff[i..start]) catch unreachable;
        }
    }
    _ = writer.write(r.buff[i..]) catch unreachable;
    
    return &result[0];
}

