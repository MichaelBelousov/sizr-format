//! web driver of sizr-lisp, a (completey chibi-scheme-based) scheme interpreter
//! with the sizr primitives built in

// TODO: rename owning directory to webdriver, doesn't support non-web use case yet,
// and probably want a different directory for that

const std = @import("std");
const builtin = @import("builtin");
// FIXME: create a package of chibi bindings
const chibi = @cImport({
    if (builtin.os.tag == .wasi) {
        @cDefine("SEXP_USE_DL", "0");
    }
    @cInclude("../chibi_macros.h");
});

// FIXME: horrible, errno is different between wasi and emscripten,
// so we have to declare this to satisfy linker
// this issue https://github.com/WebAssembly/wasi-libc/issues/411
extern "C" var errno: c_long;

export var chibi_ctx: chibi.sexp = null;

// TODO: consider using a different allocator
var allocator = std.heap.c_allocator;

var preopens: std.fs.wasi.PreopenList = undefined;

var target_buf: []u8 = undefined;

export fn init() u16 {
    // const args = std.process.argsAlloc(allocator) catch |e| {
    //     std.debug.print("proc arg alloc err: {}\n", .{e});
    //     return @errorToInt(e);
    // };
    // defer std.process.argsFree(allocator, args);
    // for (args) |arg| {
    //     std.debug.print("arg: {s}\n", .{arg});
    // }

    // preopens = std.fs.wasi.PreopenList.init(allocator);
    // // populate causes integer overflow somehow,
    // no backtraces so haven't looked into it yet
    // preopens.populate("/") catch return 1;
    // preopens.populate("/") catch |e| {
    //     std.debug.print("preopen populate err: {}\n", .{e});
    //     return @errorToInt(e);
    // };
    // for (preopens.asSlice()) |preopen, i| {
    //     std.debug.print("preopen {}: {}\n", .{i, preopen});
    // }
    // std.os.initPreopensWasi(allocator, "/") catch |e| {
    //     std.debug.print("initPreopen err: {}\n", .{e});
    //     return @errorToInt(e);
    // };

    const target_file = std.fs.cwd().openFile("/target.txt", .{}) catch |e| {
        std.debug.print("open /target.txt err: {}\n", .{e});
        return @errorToInt(e);
    };
    defer target_file.close();
    const file_len = @intCast(usize, (target_file.stat() catch |e| {
        std.debug.print("target.txt stat err: {}\n", .{e});
        return @errorToInt(e);
    }).size);

    target_buf = allocator.alloc(u8, file_len) catch |e| {
        std.debug.print("target_buf alloc err: {}\n", .{e});
        return @errorToInt(e);
    };

    _ = target_file.readAll(target_buf) catch |e| {
        std.debug.print("err: {}\n", .{e});
        return @errorToInt(e);
    };

    chibi.sexp_scheme_init();

    chibi_ctx = chibi.sexp_make_eval_context(null, null, null, 0, 0);
    _ = chibi.sexp_load_standard_env(chibi_ctx, null, chibi.SEXP_SEVEN);
    _ = chibi.sexp_load_standard_ports(chibi_ctx, null, chibi.stdin, chibi.stdout, chibi.stdout, 1);

    return 0;
}

export fn deinit() void {
    _ = chibi.sexp_destroy_context(chibi_ctx);
    preopens.deinit();
    allocator.free(target_buf);
}

// this blog demonstrates how I should implement usage of this function in the browser with wasmer
// https://mnt.io/2018/08/22/from-rust-to-beyond-the-webassembly-galaxy/
export fn eval_str(buf_ptr: [*]const u8, _buf_len: i32) void {
    const result = chibi.sexp_eval_string(chibi_ctx, buf_ptr, @intCast(c_int, _buf_len), null);
    chibi._sexp_debug(chibi_ctx, "", result);
}

export fn eval_stdin() u16 {
    var line_buff: [1024]u8 = undefined;
    const bytes_read = std.io.getStdIn().read(&line_buff) catch |e| return @errorToInt(e);
    const result = chibi.sexp_eval_string(chibi_ctx, &line_buff, @intCast(c_int, bytes_read), null);
    chibi._sexp_debug(chibi_ctx, "", result);
    return 0;
}

pub fn main() !void {
    const init_result = init();
    if (init_result != 0) {
        const init_err = @intToError(init_result);
        std.debug.print("caught init error: {}\n", .{init_err});
        return init_err;
    }

    defer deinit();

    //while (true) {
    {
        var line_buff: [1024]u8 = undefined;
        // TODO: use readline lib and also wait for parens to match
        _ = try std.io.getStdOut().write("> ");
        const bytes_read = try std.io.getStdIn().read(&line_buff);
        // if (std.mem.eql(u8, "exit", line_buff[0..bytes_read]))
        //     break;
        eval_str(&line_buff, @intCast(i32, bytes_read));
    }
}
