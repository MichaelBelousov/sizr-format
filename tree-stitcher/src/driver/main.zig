//! super simple driver of sizr-lisp, a (completey chibi-scheme-based) scheme interpreter
//! with the sizr primitives built in

const std = @import("std");
// FIXME: create a package of chibi bindings
const chibi = @cImport({ @cInclude("../chibi_macros.h"); });

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.c_allocator);
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
        _ = try std.io.getStdOut().write("> ");
        const bytes_read = try std.io.getStdIn().read(&line_buff);
        const result = chibi.sexp_eval_string(ctx, &line_buff, @intCast(c_long, bytes_read), null);
        chibi._sexp_debug(ctx, "", result);
        if (std.mem.eql(u8, "exit", line_buff[0..4]))
            break;
    }
}

