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
        field_replacements: std.StringArrayHashMap(chibi.sexp),
    ) chibi.sexp {
        var sexp_stack = std.SegmentedList(chibi.sexp, 64){};
        defer sexp_stack.deinit(std.heap.c_allocator);
        var top = sexp_stack.addOne(std.heap.c_allocator) catch unreachable;
        top.* = chibi.SEXP_NULL;

        // TODO: move to internal state of NodeToAstImpl?
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
            if (cursor.current_node().is_null())
                @panic("current node was null, not possible with tree cursor");

            // TODO:
            // if (cursor.current_node().is_missing())

            // FIXME: instead of a string map, use the language to store in the field map the TSSymbol
            // for faster lookups
            const curr_field_name = cursor.current_field_name();
            const maybe_replacement = if (curr_field_name) |field| field_replacements.get(field) else null;
            if (maybe_replacement) |replacement| {
                _sexp_prepend(ctx, top, replacement);

            } else {
                if (cursor.current_node().is_named()) {
                    // ZIGBUG?: why isn't this an implicit conversion?
                    const sym = chibi.sexp_intern(ctx, cursor.current_node().@"type"().?.ptr, -1);
                    _sexp_prepend(ctx, top, sym);
                }

                if (state.goto_first_child()) |goto_result| {
                    if (goto_result.is_named) state.pushEmpty();
                    continue;
                } else {
                    const slice = cursor.current_node().in_source(parse_ctx.buff);
                    const str = chibi.sexp_c_string(ctx, slice.ptr, @intCast(c_long, slice.len));
                    _sexp_prepend(ctx, top, str);
                }
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
        return ast;
    }

    // NOTE: not sure I need this thunk anymore since there is no longer recursion in the impl
    // ZIGBUG even with pub on the struct, this doesn't make it into the bundled library when marked `export`
    fn node_to_ast(
        ctx: chibi.sexp,
        in_node: ts._c.TSNode,
        parse_ctx: *const bindings.ExecQueryResult,
        field_replacements: std.StringArrayHashMap(chibi.sexp),
    ) chibi.sexp {
        const node = ts.Node{._c = in_node};
        var cursor = ts.TreeCursor.new(node);
        defer cursor.free();
        const ast = node_to_ast_impl(ctx, &cursor, parse_ctx, field_replacements);
        return ast;
    }
};

// ZIGBUG: crash if exported?
fn node_to_ast(
    ctx: chibi.sexp,
    in_node: ts._c.TSNode,
    parse_ctx: *const bindings.ExecQueryResult,
    field_replacements: std.StringArrayHashMap(chibi.sexp),
) chibi.sexp {
    return NodeToAstImpl.node_to_ast(ctx, in_node, parse_ctx, field_replacements);
}

const none: chibi.sexp = null;

const MatchTransformer = struct {
    query_ctx: *bindings.ExecQueryResult,
    ctx: chibi.sexp,
    transform: chibi.sexp,

    env: chibi.sexp,
    sexp_self: chibi.sexp, // for exceptions

    fn new(query_ctx: *bindings.ExecQueryResult, ctx: chibi.sexp, transform: chibi.sexp) @This() {
        const env = chibi._sexp_context_env(ctx);
        const sexp_self = chibi.sexp_env_ref(
            ctx, env,
            chibi.sexp_intern(ctx, "transform_ExecQueryResult", -1), none
        );
        if (sexp_self == none) @panic("could not find owning function bindings in environment");

        return @This(){
            .query_ctx = query_ctx,
            .ctx = ctx,
            .transform = transform,
            .env = env,
            .sexp_self = sexp_self,
        };
    }

    pub fn transform_match(
        query_ctx: *bindings.ExecQueryResult,
        chibi_ctx: chibi.sexp,
        match: ts._c.TSQueryMatch,
        transform: chibi.sexp,
    ) chibi.sexp {
        // TODO: document better that we always add a root capture to the end of the query
        const root_node = ts.Node{._c = match.captures[match.capture_count - 1].node};
        const result = MatchTransformer
            .new(query_ctx, chibi_ctx, transform)
            .transform_match_impl(match, transform, root_node, false);
        return result;
    }

    /// Given "match", a query matching context, a "transform" s-exp, and a root node, "node"
    /// - traverse the transform and each time we find an s-exp list starting with a `@capture`,
    ///   skip its subtree replace it with:
    ///   - the extended tree-sitter s-exp representation (defined by node_to_ast above)
    ///     of that capture's node in the match, except
    ///   - read through the "arguments" of the caller (list elements following that first one)
    ///     - if they are a `field:` identifier, replace that field's representation in the caller's expansion
    ///       with transform_match_impl(match, sibling_after_field:_in_transform, field_node)
    ///       // FIXME: field order could matter, should be illegal to have a non-field before a field
    ///     - otherwise append to the caller's expansion:
    ///       transform_match_impl(match_ctx, the_non_field_transform_subexpr, node)
    fn transform_match_impl(
        self: @This(),
        match: ts._c.TSQueryMatch,
        transform_expr: chibi.sexp,
        node: ts.Node,
        in_ast_expansion: bool,
    ) chibi.sexp {
        if (chibi._sexp_pairp(transform_expr) == 0)
            return transform_expr;

        const car = chibi._sexp_car(transform_expr);
        const list_starts_with_symbol = chibi._sexp_symbolp(car) != 0; 

        if (list_starts_with_symbol) {
            // TODO: add util func for this
            const symbol_str = chibi._sexp_string_data(chibi._sexp_symbol_to_string(self.ctx, car));
            const symbol_slice = symbol_str[0..std.mem.len(symbol_str)];
            // TODO: check if in map instead of assume and fail
            const list_starts_with_capture_ref = std.mem.startsWith(u8, symbol_slice, "@");

            if (list_starts_with_capture_ref) {
                // FIXME: the capture order is in captured node source order, so this naive analysis is probably wrong
                const capture_index = match.capture_count - 1 - (
                    self.query_ctx.capture_name_to_index.get(symbol_slice)
                    orelse std.debug.panic("couldn't find capture {s}", .{symbol_slice})
                  );

                const capture = match.captures[capture_index];

                // FIXME: shouldn't the query context also use a StringArrayHashMap?
                var fields = std.StringArrayHashMap(chibi.sexp).init(std.heap.c_allocator);
                defer fields.deinit();
                // go through children, determining fields
                var child_list = chibi._sexp_cdr(transform_expr);
                var done_with_fields = false;
                var to_append_list = std.SegmentedList(chibi.sexp, 16){};
                defer to_append_list.deinit(std.heap.c_allocator);
                while (chibi._sexp_nullp(child_list) == 0) : (child_list = chibi._sexp_cdr(child_list)) {
                    const child = chibi._sexp_car(child_list);
                    const is_symbol = chibi._sexp_symbolp(child) != 0;

                    if (is_symbol) {
                        const child_symbol_str = chibi._sexp_string_data(chibi._sexp_symbol_to_string(self.ctx, child));
                        const child_symbol_slice = child_symbol_str[0..std.mem.len(child_symbol_str)];
                        const is_field_ref = std.mem.endsWith(u8, child_symbol_slice, ":");
                        if (is_field_ref) {
                            if (done_with_fields) @panic("fields are illegal after non-fields");
                            child_list = chibi._sexp_cdr(child_list);
                            // FIXME: error handling
                            if (chibi._sexp_nullp(child_list) != 0) @panic("field without replacement");
                            const field_name = child_symbol_slice[0..child_symbol_slice.len - 1];
                            const field_replacement = chibi._sexp_car(child_list);
                            // FIXME: error handling
                            const field_node = node.child_by_field_name(field_name)
                                orelse std.debug.panic("bad field name {s}\n", .{field_name});
                            const transformed_field_replacement =
                                self.transform_match_impl(match, field_replacement, field_node, true);
                            fields.put(field_name, transformed_field_replacement) catch @panic("put field failed");
                            continue;
                        }
                    } else {
                        done_with_fields = true;
                        const to_append = to_append_list.addOne(std.heap.c_allocator) catch unreachable;
                        // FIXME: returning a symbol won't work here...
                        to_append.* = self.transform_match_impl(match, child, node, true);
                    }
                }

                var ast = node_to_ast(self.ctx, capture.node, self.query_ctx, fields);

                var to_append_iter = to_append_list.constIterator(0);
                while (to_append_iter.next()) |to_append| {
                    ast = chibi._sexp_append2(
                        self.ctx, ast,
                        chibi._sexp_cons(self.ctx, to_append.*, chibi.SEXP_NULL)
                    );
                }

                // TODO: remove nested quoting!
                // FIXME: maybe I can just replace all quoting with implementing the tree-sitter
                // nodes as forms in the lang namespace
                if (!in_ast_expansion) {
                    const quasiquote_sym = chibi.sexp_intern(self.ctx, "quasiquote", -1);
                    ast = chibi.sexp_list2(self.ctx, quasiquote_sym, ast);
                }

                return ast;
            }
        }

        // not a list starting with a capture
        const new_car = self.transform_match_impl(match, car, node, in_ast_expansion);
        const cdr = chibi._sexp_cdr(transform_expr);
        const cdr_not_null = chibi._sexp_nullp(cdr) == 0;
        const new_cdr =
            if (cdr_not_null) self.transform_match_impl(match, cdr, node, in_ast_expansion)
            else chibi.SEXP_NULL;
        return chibi._sexp_cons(self.ctx, new_car, new_cdr);
    }
};

export fn transform_ExecQueryResult(query_ctx: *bindings.ExecQueryResult, transform: chibi.sexp, ctx: chibi.sexp) [*c]const u8 {
    var result = std.heap.c_allocator.allocSentinel(u8, 8192, 0) catch unreachable;
    var writer = std.io.fixedBufferStream(result);

    const match_count = std.mem.len(query_ctx.matches);
    var i: usize = 0;
    for (query_ctx.matches[0..match_count]) |maybe_match| {
        if (maybe_match) |match| {
            const outer_capture = match.captures[0];
            const start = ts._c.ts_node_start_byte(outer_capture.node);
            const end = ts._c.ts_node_end_byte(outer_capture.node);
            _ = writer.write(query_ctx.buff[i..start]) catch unreachable;
            i = end;

            const transformed_ast = MatchTransformer.transform_match(query_ctx, ctx, match.*, transform);

            if (std.os.getenv("DEBUG") != null) chibi._sexp_debug(ctx, "transform ast:", transformed_ast);
            const transform_result = chibi._sexp_eval(ctx, transformed_ast, null);

            if (chibi._sexp_exceptionp(transform_result) != 0) {
                chibi._sexp_debug(ctx, "exception: ", transform_result);
                chibi._sexp_print_exception(ctx, transform_result, chibi._sexp_current_error_port(ctx));
                @panic("can't return exception with this signature yet so boom");
            }

            // TODO: implicit ast->string?
            if (chibi._sexp_stringp(transform_result) == 0) {
                chibi._sexp_debug(ctx, "not a string: ", transform_result);
                @panic("don't have ast->string implemented in zig yet so haven't done this yet; boom");
            }

            const transform_as_str = chibi._sexp_string_data(transform_result);
            const transform_as_str_len = chibi._sexp_string_size(transform_result);
            _ = writer.write(transform_as_str[0..transform_as_str_len]) catch unreachable;
        }
    }
    _ = writer.write(query_ctx.buff[i..]) catch unreachable;
    
    return &result[0];
}

