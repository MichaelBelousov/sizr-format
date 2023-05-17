const std = @import("std");
const bindings = @import("./bindings.zig");
const ts = @import("tree-sitter");
const chibi = @cImport({ @cInclude("./chibi_macros.h"); });

export fn node_to_ast(ctx: chibi.sexp, in_node: ts._c.TSNode) chibi.sexp {
    var ast = chibi.SEXP_NULL;
    const node = ts.Node{._c = in_node};
    const cursor = ts.TreeCursor.new(node);
    defer cursor.free();
    // make it backwards and then run reverse because lisp!

    while (true) {
        cursor.current_node();
        ast = chibi._sexp_cons();
    }

    chibi.sexp_nreverse(ctx, ast);

    return ast;
}

const MatchTransformer = struct {
    r: *bindings.ExecQueryResult,
    ctx: chibi.sexp,
    transform: chibi.sexp,

    /// make a copy of the given transform sexp with the following changes:
    /// - replace all tree-sitter query capture syntax `@symbol` with the tree-sitter s-exp for that capture
    /// - replace all naked tree-sitter query field syntax `field:` with tree-sitter s-exp for that field
    /// - collapse all tree-sitter query field syntax `field: (expr)` into operations on that node
    pub fn transform_match(self: @This(), match: ts._c.TSQueryMatch) chibi.sexp {
        var index: usize = 0;
        return self.transform_match_impl(match, self.transform, &index);
    }

    fn transform_match_impl(self: @This(), match: ts._c.TSQueryMatch, expr: chibi.sexp, capture_index: *usize) chibi.sexp {
        const env = chibi._sexp_context_env(self.ctx);

        const none: chibi.sexp = null;
        // TODO: only needed in an error situation
        const sexp_self = chibi.sexp_env_ref(self.ctx, env, chibi.sexp_intern(self.ctx, "transform_ExecQueryResult", -1), none);
        if (sexp_self == none)
            @panic("could not find owning function bindings in environment");

        if (chibi._sexp_pairp(expr) == 0) {
            if (chibi._sexp_symbolp(expr) != 0) {
                // TODO: add util func for this
                const symbol_str = chibi._sexp_string_data(chibi._sexp_symbol_to_string(self.ctx, expr));
                const symbol_slice = symbol_str[0..std.mem.len(symbol_str)];
                if (std.mem.startsWith(u8, symbol_slice, "@")) {
                    if (capture_index.* >= match.capture_count)
                        std.debug.panic("capture list overflowed (capture_index={d})", .{capture_index.*});
                    capture_index.* += 1;
                    const capture = match.captures[capture_index.*];
                    const capture_node = ts.Node { ._c = capture.node };
                    const capture_sexp_string = capture_node.string();
                    defer capture_sexp_string.free();
                    const string_to_expr = chibi.sexp_env_ref(self.ctx, env, chibi.sexp_intern(self.ctx, "string->expr", -1), none);
                    if (sexp_self == none) @panic("could not find 'string->expr' in environment");
                    const capture_sexp_string_sexp = chibi.sexp_c_string(self.ctx, capture_sexp_string.ptr, -1);
                    const capture_sexp = chibi.sexp_apply(self.ctx, string_to_expr, capture_sexp_string_sexp);
                    return capture_sexp;
                } if (std.mem.endsWith(u8, symbol_slice, ":")) {
                    // to do this, need to know the current level of the ast we're in...
                    const capture = match.captures[capture_index.*];
                    const capture_node = ts.Node { ._c = capture.node };
                    const capture_sexp_string = capture_node.string();
                    defer capture_sexp_string.free();
                    const string_to_expr = chibi.sexp_env_ref(self.ctx, env, chibi.sexp_intern(self.ctx, "string->expr", -1), none);
                    if (sexp_self == none) @panic("could not find 'string->expr' in environment");
                    const capture_sexp_string_sexp = chibi.sexp_c_string(self.ctx, capture_sexp_string.ptr, -1);
                    const capture_sexp = chibi.sexp_apply(self.ctx, string_to_expr, capture_sexp_string_sexp);
                    return capture_sexp;
                } else {
                    // just an ast function name
                    // NEXT

                }
                return expr;
            } else {
                return expr;
            }
        } else {
            const car = chibi._sexp_car(expr);
            const cdr = chibi._sexp_cdr(expr);
            const new_car = self.transform_match_impl(match, car, capture_index);
            const new_cdr =
                if (chibi._sexp_nullp(cdr) == 0)
                    self.transform_match_impl(match, cdr, capture_index)
                else null;
            return chibi._sexp_cons(self.ctx, new_car, new_cdr);
        }

        //const symbol = chibi.sexp_env_define(
            //self.ctx, env, chibi.sexp_intern(ctx, t.name, -1),
        //);

        //chibi.sexp_define_foreign(self.ctx, env, t.name, 1, sexp_proc);
        //const symbol = chibi.sexp_env_define(
            //self.ctx, env, chibi.sexp_intern(ctx, t.name, -1),
        //);
    }
};

export fn transform_ExecQueryResult(r: *bindings.ExecQueryResult, transform: chibi.sexp, ctx: chibi.sexp) [*c]const u8 {
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

            chibi._sexp_debug(ctx, "transform arg1:", chibi._sexp_car(transform));
            // evaluate the functionized transform body into a full tree-sitter node tree
            // then serialize that back into the source language
            const match_transformer = MatchTransformer { .r = r, .ctx = ctx, .transform = transform, };
            const functionized_transform = match_transformer.transform_match(match.*);
            const transform_result = chibi._sexp_eval(ctx, functionized_transform, null);
            chibi._sexp_debug(ctx, "transform result:", transform_result);
        }
    }
    _ = writer.write(r.buff[i..]) catch unreachable;
    
    return &result[0];
}

