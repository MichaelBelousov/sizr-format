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

        const state = struct {
            cursor: *ts.TreeCursor,
            top: **chibi.sexp,
            sexp_stack: *std.SegmentedList(chibi.sexp, 64),
            ctx: chibi.sexp,

            pub fn print_moved(self: @This(), moved_to_label: []const u8) void {
                const maybe_node_type = self._cursor.current_node().@"type"();
                std.debug.print("moved to {s} '{s}'\n", .{
                    moved_to_label,
                    if (maybe_node_type) |node_type| node_type else "UNKNOWN"
                });
            }

            pub fn popMergeUp(self: @This()) void {
                // FIXME: do I need var, can I reverse without setting?
                var old_top = self.sexp_stack.pop();
                // FIXME: expensive check of end! (maybe double pop and repush instead?)
                // (or use a different stack data structure)
                // FIXME ugly double pointers... just initialize it in the state
                self.top.* = self.sexp_stack.uncheckedAt(self.sexp_stack.count() - 1);
                old_top = chibi._sexp_nreverse(self.ctx, old_top.?);
                _sexp_prepend(self.ctx, self.top.*, old_top.?);
            }

            pub fn pushEmpty(self: @This()) void {
                var next = self.sexp_stack.addOne(std.heap.c_allocator) catch unreachable;
                next.* = chibi.SEXP_NULL;
                self.top.* = next;
            }

            const CursorGotoResult = struct {
                did_move: bool,
                was_named: bool,
                is_named: bool,
            };

            pub fn goto_parent(self: @This()) ?CursorGotoResult {
                const was_named = self.cursor.current_node().is_named();
                const did_move = self.cursor.goto_parent();
                const is_named = if (did_move) self.cursor.current_node().is_named() else was_named;
                return if (!did_move) null
                    else .{ .was_named = was_named, .did_move = did_move, .is_named = is_named };
            }

            pub fn goto_next_sibling(self: @This()) ?CursorGotoResult {
                const was_named = self.cursor.current_node().is_named();
                const did_move = self.cursor.goto_next_sibling();
                const is_named = if (did_move) self.cursor.current_node().is_named() else was_named;
                return if (!did_move) null
                    else .{ .was_named = was_named, .did_move = did_move, .is_named = is_named };
            }

            pub fn goto_first_child(self: @This()) ?CursorGotoResult {
                const was_named = self.cursor.current_node().is_named();
                const did_move = self.cursor.goto_first_child();
                const is_named = if (did_move) self.cursor.current_node().is_named() else was_named;
                return if (!did_move) null
                    else .{ .was_named = was_named, .did_move = did_move, .is_named = is_named };
            }
        }{
            .cursor = cursor,
            .top = &top,
            .sexp_stack = &sexp_stack,
            .ctx = ctx,
        };

        // FIXME?: handle anonymous root node

        outer: while (true) {
            const str1 = cursor.current_node().string();
            defer str1.free();
            std.debug.print("CURR: {s}\n", .{str1.ptr});

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

            if (state.goto_first_child()) |goto_result| {
                if (goto_result.is_named) state.pushEmpty();
                continue;
            } else {
                const slice = cursor.current_node().in_source(&parse_ctx.buff);
                const str = chibi.sexp_c_string(ctx, slice.ptr, @intCast(c_long, slice.len));
                _sexp_prepend(ctx, top, str);
            }

            while (true) {
                if (state.goto_next_sibling()) |goto_result| {
                    if (goto_result.was_named) state.popMergeUp();
                    if (goto_result.is_named) state.pushEmpty();
                    break;
                }
                if (state.goto_parent()) |goto_result| {
                    if (goto_result.was_named) state.popMergeUp();
                } else break :outer;
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

