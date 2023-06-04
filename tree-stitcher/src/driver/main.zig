//! super simple driver of sizr-lisp, a (completey chibi-scheme-based) scheme interpreter
//! with the sizr primitives built in

const std = @import("std");
const builtin = @import("builtin");
// FIXME: create a package of chibi bindings
const chibi = @cImport({
    if (builtin.os.tag == .emscripten) {
        @cDefine("sexp_platform", "emscripten");
        @cDefine("SEXP_USE_DL", "0");
    }
    @cInclude("../chibi_macros.h");
});

// FIXME: horrible, errno is different between wasi and emscripten!
// this issue https://github.com/WebAssembly/wasi-libc/issues/411
extern "C" var errno: c_long;

export var chibi_ctx: chibi.sexp = null;

export fn init() u16 {
    const args = std.process.argsAlloc(std.heap.c_allocator) catch |e| return @errorToInt(e);
    defer std.process.argsFree(std.heap.c_allocator, args);
    for (args) |arg| {
        std.debug.print("arg: {s}\n", .{arg});
    }

    chibi.sexp_scheme_init();

    chibi_ctx = chibi.sexp_make_eval_context(null, null, null, 0, 0);
    _ = chibi.sexp_load_standard_env(chibi_ctx, null, chibi.SEXP_SEVEN);
    _ = chibi.sexp_load_standard_ports(chibi_ctx, null, chibi.stdin, chibi.stdout, chibi.stderr, 1);

    return 0;
}

export fn deinit() void {
    defer _ = chibi.sexp_destroy_context(chibi_ctx);
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
    chibi.sexp_scheme_init();

    //try @as(anyerror!void, @intToError(init()));
    _ = init();
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
