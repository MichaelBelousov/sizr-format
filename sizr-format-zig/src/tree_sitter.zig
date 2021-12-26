const std = @import("std");

const c_api = @cImport({
    @cInclude("tree_sitter/api.h");
});

const TsInputEncoding = enum(c_api.TSInputEncoding) {
    utf8 = c_api.TSInputEncoding.TSInputEncodingUTF8,
    utf16 = c_api.TSInputEncoding.TSInputEncodingUTF16,
};

// TODO: doing this by hand for now but some kind of clang plugin could probably generate bindings better
// alternatively, tree-sitter queries themselves or even sizr-format could potentially generate this
const TsParser = struct {
    _c: *c_api.TSParser,

    const Self = @This();

    pub fn init() Self {
        const maybe_c_parser = c_api.ts_parser_new();
        if (maybe_c_parser) |c_parser| {
            return Self{ ._c = c_parser };
        } else unreachable;
    }

    // TODO: check zig idioms
    pub fn free(self: Self) void {
        c_api.ts_parser_delete(self._c);
    }

    pub fn set_language(self: Self, language: TsLanguage) void {
        return c_api.ts_parser_set_language(self._c, language._c);
    }

    // TODO: steal the documentation from the C header
    // would be nice if this could be done with reflection
    pub fn parse_string(self: Self, old_tree: ?TsTree, src_string: []const u8) TsTree {
        // FIXME: I actually have no idea what the input encoding is when unspecified
        return self.parse_string_encoding(old_tree, src_string, TsInputEncoding.utf8);
    }

    pub fn parse_string_encoding(self: Self, old_tree: ?TsTree, src_string: []const u8, encoding: TsInputEncoding) TsTree {
        return TsTree{
            ._c = c_api.ts_parser_parse_string_encoding(
                self._c,
                if (old_tree) |old_tree_val| old_tree_val._c else null,
                src_string,
                src_string.len,
                @enumToInt(encoding)
            )
        };
    }
};

test "TsParser" {
    var parser = TsParser.init();
    defer {
        parser.free();
    }
}

const TsNode = struct {
    _c: *c_api.TSNode,
};

const TsTree = struct {
    _c: *c_api.TSTree,
};

//const TsLanguage = struct {
    //_c: *c_api.TSLanguage,
//};

/// generate a type from the tree-sitter header
fn wrapTsType(comptime name: []const u8, comptime TsType: type) type {
    const decls_count = comptime decls: {
        var i = @as(i32, 0);
        for (std.meta.declarations(c_api)) |decl| {
            if (std.mem.startsWith(decl.name, name)) {
                i += 1;
            }
        }
        break :decls i;
    };

    const decls = comptime decls: {
        var i = @as(i32, 0);
        var result = [decls_count]std.builtin.TypeInfo.StructField;
        for (std.meta.declarations(c_api)) |decl| {
            if (std.mem.startsWith(decl.name, name)) {
                result[i] = std.builtin.TypeInfo.Declaration{
                    .name = decl.name[decl.len + 1..decl.name.len],
                    .data = if (decl.data == .Fn) decl.data else decl.data
                };
                i += 1;
            }
        }
        break :decls result;
    };

    return @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = [_]std.builtin.TypeInfo.StructField{
                .{ .name = "_c", .field_type = *TsType, .default_value = undefined, .is_comptime = false, .alignment = 8 }
            },
            .decls = decls,
            .is_tuple = false
        }
    });
}

const TsLanguage = wrapTsType("ts_language", c_api.TSLanguage);

test "TsLanguage" {
    std.meta.eql(
        std.meta.declarations(@Type(TsLanguage)),
        [_]std.builtin.TypeInfo.Declaration{
            .{ .name = "symbol_name", .data = c_api.ts_language_symbol_name },
            .{ .name = "symbol_for_name", .data = c_api.ts_language_symbol_for_name },
            .{ .name = "field_count", .data = c_api.ts_language_field_count },
            .{ .name = "field_name_for_id", .data = c_api.ts_language_field_name_for_id },
            .{ .name = "field_id_for_name", .data = c_api.ts_language_field_id_for_name },
            .{ .name = "symbol_type", .data = c_api.ts_language_symbol_type },
            .{ .name = "version", .data = c_api.ts_language_version },
        }
    );
    const lang = TsLanguage.new();
    _ = lang;
    defer {
        lang.free();
    }
}
