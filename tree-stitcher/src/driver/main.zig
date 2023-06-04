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
extern "C" var errno: c_long;

export fn _initDriver() u32 {
    const args = std.process.argsAlloc(std.heap.c_allocator) catch |err| return @errorToInt(err);
    defer std.process.argsFree(std.heap.c_allocator, args);
    for (args) |arg| {
        std.debug.print("arg: {s}\n", .{arg});
    }

    chibi.sexp_scheme_init();

    var ctx: chibi.sexp = null;
    ctx = chibi.sexp_make_eval_context(null, null, null, 0, 0);
    defer _ = chibi.sexp_destroy_context(ctx);
    _ = chibi.sexp_load_standard_env(ctx, null, chibi.SEXP_SEVEN);
    _ = chibi.sexp_load_standard_ports(ctx, null, chibi.stdin, chibi.stdout, chibi.stderr, 1);

    while (true) {
        var line_buff: [8192]u8 = undefined;
        // TODO: use readline lib and also wait for parens to match
        _ = std.io.getStdOut().write("> ") catch |err| return @errorToInt(err);
        const bytes_read = std.io.getStdIn().read(&line_buff) catch |err| return @errorToInt(err);
        const result = chibi.sexp_eval_string(ctx, &line_buff, @intCast(c_long, bytes_read), null);
        chibi._sexp_debug(ctx, "", result);
        if (std.mem.eql(u8, "exit", line_buff[0..4]))
            break;
    }

    return 0;
}

