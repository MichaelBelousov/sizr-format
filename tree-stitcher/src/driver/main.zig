//! super simple driver of sizr-lisp, a (completey chibi-scheme-based) scheme interpreter
//! with the sizr primitives built in

const std = @import("std");
// FIXME: create a package of chibi bindings
const chibi = @cImport({ @cInclude("../chibi_macros.h"); });
const c_stdio = @cImport({ @cInclude("stdio.h"); });
const File = @cImport({ @cInclude("bits/types/FILE.h"); });

pub fn main() !void {
    var ctx: chibi.sexp = undefined;
    chibi.sexp_scheme_init();
    ctx = chibi.sexp_make_eval_context(null, null, null, 0, 0);
    _ = chibi.sexp_load_standard_env(ctx, null, chibi.SEXP_SEVEN);
    _ = chibi.sexp_load_standard_ports(ctx, null, @ptrCast(*File.FILE, c_stdio.stdin), c_stdio.stdout, c_stdio.stderr, 1);
    _ = chibi.sexp_destroy_context(ctx);

    const args = try std.process.argsAlloc(std.heap.c_allocator);
    std.process.argsFree(std.heap.c_allocator, args);

    for (args) |arg| {
        std.log.info("arg: {s}", .{arg});
    }
}

