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
                const slice = cursor.current_node().in_source(parse_ctx.buff);
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

const none: chibi.sexp = null;

const MatchTransformer = struct {
    r: *bindings.ExecQueryResult,
    ctx: chibi.sexp,
    transform: chibi.sexp,

    env: chibi.sexp,
    sexp_self: chibi.sexp, // for exceptions

    fn new(r: *bindings.ExecQueryResult, ctx: chibi.sexp, transform: chibi.sexp) @This() {
        const env = chibi._sexp_context_env(ctx);
        const sexp_self = chibi.sexp_env_ref(
            ctx, env,
            chibi.sexp_intern(ctx, "transform_ExecQueryResult", -1), none
        );
        if (sexp_self == none) @panic("could not find owning function bindings in environment");

        return @This(){
            .r = r,
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
        const root_node = match.captures[match.capture_count - 1].node;
        const result = MatchTransformer
            .new(query_ctx, chibi_ctx, transform)
            .transform_match_impl(match, transform, root_node);
        return result;
    }

    const InnerTransformResult = struct {
        sexp: chibi.sexp,
        node: ts.Node,
    };

    /// Given "match", a query matching context, a "transform" s-exp, and a root node, "node"
    /// - traverse the transform and each time we find an s-exp list starting with a `@capture`,
    ///   skip its subtree replace it with:
    ///   - the extended tree-sitter s-exp representation (defined by node_to_ast above)
    ///     of that capture's node in the match, except
    ///   - read through the "arguments" of the caller (list elements following that first one)
    ///     - if they are a `field:` identifier, replace that field's representation in the caller's expansion
    ///       with transform_match_impl(match, sibling_after_field:_in_transform, field_node)
    ///     - otherwise append to the caller's expansion:
    ///       transform_match_impl(match_ctx, the_non_field_transform_subexpr, node)
    fn transform_match_impl(
        self: @This(),
        match: ts._c.TSQueryMatch,
        transform_expr: chibi.sexp,
        node: ts.Node,
    ) chibi.sexp {
        const isPair = chibi._sexp_pairp(transform_expr) != 0;

        if (isPair) {
            const car = chibi._sexp_car(transform_expr);
            const cdr = chibi._sexp_cdr(transform_expr);
            const new_car = self.transform_match_impl(match, car, node);
            const cdr_not_null = chibi._sexp_nullp(cdr) == 0;
            const new_cdr =
                if (cdr_not_null) self.transform_match_impl(match, cdr, node)
                else chibi.SEXP_NULL;
            return chibi._sexp_cons(self.ctx, new_car, new_cdr);

        } else {
            const isSymbol = chibi._sexp_symbolp(transform_expr) != 0; 
            if (isSymbol) {
                // TODO: add util func for this
                const symbol_str = chibi._sexp_string_data(chibi._sexp_symbol_to_string(self.ctx, transform_expr));
                const symbol_slice = symbol_str[0..std.mem.len(symbol_str)];

                // TODO: check if in map instead of fail
                const isCaptureRef = std.mem.startsWith(u8, symbol_slice, "@");
                if (isCaptureRef) {
                    // FIXME: instead propagate return some kind of parse error
                    const capture_index = self.r.capture_name_to_index.get(symbol_slice)
                        orelse std.debug.panic("couldn't find capture {s}", .{symbol_slice});
                    const capture = match.captures[capture_index];
                    const ast = node_to_ast(self.ctx, capture.node, self.r);
                    const quote_sym = chibi.sexp_intern(self.ctx, "quote", -1);
                    const ast_list = chibi.sexp_list2(self.ctx, quote_sym, ast);
                    return ast_list;
                }

                const isFieldRef = std.mem.endsWith(u8, symbol_slice, ":");
                if (isFieldRef) {
                    // FIXME:
                    // to do this, need to know the current level of the ast we're in...
                    const capture = match.captures[capture_index.*];
                    const capture_node = ts.Node { ._c = capture.node };
                    const capture_sexp_string = capture_node.string();
                    defer capture_sexp_string.free();
                    const string_to_expr = chibi.sexp_env_ref(
                        self.ctx, env,
                        chibi.sexp_intern(self.ctx, "string->expr", -1), none
                    );
                    if (string_to_expr == none) @panic("could not find 'string->expr' in environment");
                    const capture_sexp_string_sexp = chibi.sexp_c_string(self.ctx, capture_sexp_string.ptr, -1);
                    const capture_sexp = chibi.sexp_apply(self.ctx, string_to_expr, capture_sexp_string_sexp);
                    return capture_sexp;
                }

                // just an ast function name, ignore and let it do what it wants later
                // NEXT

                return transform_expr;
            } else {
                return transform_expr;
            }
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

            const transformed_ast = MatchTransformer.transform_match(r, ctx, transform, match.*);

            if (std.os.getenv("DEBUG")) |_| chibi._sexp_debug(ctx, "transform ast:", transformed_ast);
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

