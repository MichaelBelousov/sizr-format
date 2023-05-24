const std = @import("std");
const bindings = @import("./bindings.zig");
const ts = @import("tree-sitter");
const chibi = @cImport({ @cInclude("./chibi_macros.h"); });

fn _sexp_prepend(ctx: chibi.sexp, list: *chibi.sexp, exp: chibi.sexp) void {
    // ZIGBUG: possible zig translate-c bug when using just sexp_push
    _ = chibi.sexp_push_op(ctx, list, chibi.SEXP_VOID);
    chibi._set_sexp_car(list.*, exp);
}

const NodeToAstImpl = struct {
    fn node_to_ast_impl(
        ctx: chibi.sexp,
        cursor: *ts.TreeCursor,
        parse_ctx: *const bindings.ExecQueryResult,
    ) chibi.sexp {
        var sexp_stack = std.SegmentedList(chibi.sexp, 64){};
        defer sexp_stack.deinit(std.heap.c_allocator);
        var top = sexp_stack.addOne(std.heap.c_allocator) catch unreachable;
        top.* = chibi.SEXP_NULL;

        // FIXME?: handle anonymous root node

        // 1. if N.is_named, add it name as a symbol to the list at the top of the stack
        // 2. try to go down
        //   # maybe shouldn't add an empty list if new node is named?
        //   2a. if we can (there are children), do, push an empty list to the stack, and restart
        //   2b. if the node has no children, append the source string to the list at the top of the stack, and proceed (to 3)
        // 3. try to go up until we can go right or hit the root
        //   3a. each time we go up, pop
        //   3b. if we hit the root node, we are finished
        //   3c. if we can move right
        //     - do so
        //     - if the node was named
        //       - pop the stack,
        //     - if the post-move new node is named:
        //       - push an empty list to the stack
        //     - restart
        outer: while (true) {
            const str1 = cursor.current_node().string();
            defer str1.free();
            std.debug.print("CURR: {s}\n", .{str1.ptr});

            const debug = struct {
                _cursor: ts.TreeCursor,
                pub fn print_moved(self: @This(), moved_to_label: []const u8) void {
                    const maybe_node_type = self._cursor.current_node().@"type"();
                    std.debug.print("moved to {s} '{s}'\n", .{
                        moved_to_label,
                        if (maybe_node_type) |node_type| node_type else "UNKNOWN"
                    });
                }
            }{ ._cursor = cursor.* };

            var stack_iter = sexp_stack.constIterator(0);
            var i: u32 = 0;
            while (stack_iter.next()) |val| {
                std.debug.print("stack {d}:\n", .{i});
                chibi._sexp_debug(ctx, " ", val.*);
                i += 1;
            }

            if (cursor.current_node().is_null())
                @panic("current node was null, not possible with tree cursor");

            // TODO:
            // if (cursor.current_node().is_missing())

            if (cursor.current_node().is_named()) {
                // ZIGBUG?: why isn't this an implicit conversion?
                const sym = chibi.sexp_intern(ctx, cursor.current_node().@"type"().?.ptr, -1);
                _sexp_prepend(ctx, top, sym);
            }

            if (cursor.goto_first_child()) {
                debug.print_moved("first child");
                if (cursor.current_node().is_named()) {
                    var next = sexp_stack.addOne(std.heap.c_allocator) catch unreachable;
                    next.* = chibi.SEXP_NULL;
                    top = next;
                }
            } else {
                std.debug.print("no children\n", .{});
                const slice = cursor.current_node().in_source(&parse_ctx.buff);
                const str = chibi.sexp_c_string(ctx, slice.ptr, @intCast(c_long, slice.len));
                _sexp_prepend(ctx, top, str);
            }

            const wasNamed = cursor.current_node().is_named();

            if (wasNamed) {
                var old_top = sexp_stack.pop();
                // FIXME: expensive check of end! (maybe double pop instead?)
                top = sexp_stack.uncheckedAt(sexp_stack.count() - 1);
                old_top = chibi._sexp_nreverse(ctx, old_top.?);
                _sexp_prepend(ctx, top, old_top.?);
            }

            while (true) {
                if (cursor.goto_next_sibling()) {
                    debug.print_moved("next sibling");
                    break;
                }
                const foundRoot = !cursor.goto_parent();
                if (foundRoot) {
                    std.debug.print("found root\n", .{});
                    break :outer;
                }
                debug.print_moved("parent");
            }

            const nowNamed = cursor.current_node().is_named();

            if (nowNamed) {
                var next = sexp_stack.addOne(std.heap.c_allocator) catch unreachable;
                next.* = chibi.SEXP_NULL;
                top = next;
            }
        }

        var ast = sexp_stack.pop().?;
        ast = chibi._sexp_nreverse(ctx, ast);
        chibi._sexp_debug(ctx, "RESULT AST:", ast);
        return ast;
    }

    // NOTE: not sure I need this thunk anymore since there is no longer recursion in the impl
    // ZIGBUG even with pub on the struct, this doesn't make it into the bundled library when marked `export`
    fn node_to_ast(ctx: chibi.sexp, in_node: ts._c.TSNode, parse_ctx: *const bindings.ExecQueryResult) chibi.sexp {
        const node = ts.Node{._c = in_node};
        var cursor = ts.TreeCursor.new(node);
        defer cursor.free();
        const ast = node_to_ast_impl(ctx, &cursor, parse_ctx);
        return ast;
    }
};

export fn node_to_ast(ctx: chibi.sexp, in_node: ts._c.TSNode, parse_ctx: *const bindings.ExecQueryResult) chibi.sexp {
    return NodeToAstImpl.node_to_ast(ctx, in_node, parse_ctx);
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
        const result = self.transform_match_impl(match, self.transform, &index);
        return result;
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
                    const capture = match.captures[capture_index.*];
                    capture_index.* += 1;
                    const ast = node_to_ast(self.ctx, capture.node, self.r);
                    const quote_sym = chibi.sexp_intern(self.ctx, "quote", -1);
                    const ast_list = chibi.sexp_list2(self.ctx, quote_sym, ast);
                    return ast_list;
                } if (std.mem.endsWith(u8, symbol_slice, ":")) {
                    // to do this, need to know the current level of the ast we're in...
                    const capture = match.captures[capture_index.*];
                    const capture_node = ts.Node { ._c = capture.node };
                    const capture_sexp_string = capture_node.string();
                    defer capture_sexp_string.free();
                    const string_to_expr = chibi.sexp_env_ref(self.ctx, env, chibi.sexp_intern(self.ctx, "string->expr", -1), none);
                    if (string_to_expr == none) @panic("could not find 'string->expr' in environment");
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
                else chibi.SEXP_NULL;
            return chibi._sexp_cons(self.ctx, new_car, new_cdr);
        }
    }
};

export fn transform_ExecQueryResult(r: *bindings.ExecQueryResult, transform: chibi.sexp, ctx: chibi.sexp) [*c]const u8 {
    var result = std.heap.c_allocator.allocSentinel(u8, 8192, 0) catch unreachable;
    var writer = std.io.fixedBufferStream(result);

    const match_count = std.mem.len(r.matches);
    var i: usize = 0;
    for (r.matches[0..match_count]) |maybe_match| {
        if (maybe_match) |match| {
            const outer_capture = match.captures[match.capture_count - 1];
            _ = outer_capture.node;
            const start = ts._c.ts_node_start_byte(outer_capture.node);
            const end = ts._c.ts_node_end_byte(outer_capture.node);
            _ = writer.write(r.buff[i..start]) catch unreachable;
            i = end;

            // OLD:
            // evaluate the functionized transform body into a full tree-sitter node tree
            // then serialize that back into the source language

            const match_transformer = MatchTransformer { .r = r, .ctx = ctx, .transform = transform, };
            const transformed_ast = match_transformer.transform_match(match.*);
            const transform_result = chibi._sexp_eval(ctx, transformed_ast, null);
            // TODO: implicit ast->string?
            const transform_as_str = chibi._sexp_string_data(transform_result);
            const transform_as_str_len = chibi._sexp_string_size(transform_result);
            _ = writer.write(transform_as_str[0..transform_as_str_len]) catch unreachable;
        }
    }
    _ = writer.write(r.buff[i..]) catch unreachable;
    
    return &result[0];
}

