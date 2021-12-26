//! TODO: doing this by hand for now but some kind of clang plugin could probably generate bindings better
//! alternatively, tree-sitter queries themselves or even sizr-format could potentially generate this

const std = @import("std");

const c_api = @cImport({
    @cInclude("tree_sitter/api.h");
});

pub const _c = c_api;

pub const InputEncoding = enum(c_api.TSInputEncoding) {
    utf8 = c_api.TSInputEncoding.TSInputEncodingUTF8,
    utf16 = c_api.TSInputEncoding.TSInputEncodingUTF16,
};

// TODO: remove Ts prefix from all types since user can namespace it themselves
pub const Parser = struct {
    _c: *c_api.TSParser,

    const Self = @This();

    pub fn new() Self {
        const maybe_c_parser = c_api.ts_parser_new();
        if (maybe_c_parser) |c_parser| {
            return Self{ ._c = c_parser };
        } else unreachable;
    }

    // TODO: check zig idioms
    pub fn free(self: Self) void {
        c_api.ts_parser_delete(self._c);
    }

    pub fn set_language(self: Self, language: Language) void {
        return c_api.ts_parser_set_language(self._c, language._c);
    }

    // TODO: steal the documentation from the C header
    // would be nice if this could be done with reflection
    pub fn parse_string(self: Self, old_tree: ?Tree, src_string: []const u8) Tree {
        // FIXME: I actually have no idea what the input encoding is when unspecified
        return self.parse_string_encoding(old_tree, src_string, InputEncoding.utf8);
    }

    pub fn parse_string_encoding(self: Self, old_tree: ?Tree, src_string: []const u8, encoding: InputEncoding) Tree {
        return Tree{
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

test "Parser" {
    var parser = Parser.new();
    defer {
        parser.free();
    }
}

pub const Node = struct {
    _c: *c_api.TSNode,
};

pub const Tree = struct {
    _c: *c_api.TSTree,
};

// these are just typedefs to native types
pub const FieldId = c_api.TSFieldId;
pub const Symbol = c_api.TSSymbol;

pub const SymbolType = enum(c_api.TSSymbolType) {
    regular = c_api.TSSymbolType.TSSymbolTypeRegular,
    anonymous = c_api.TSSymbolType.TSSymbolTypeAnonymous,
    auxiliary = c_api.TSSymbolType.TSSymbolTypeAuxiliary,
};

pub const Language = struct {
    _c: *c_api.TSLanguage,

    const Self = @This();

    pub fn symbol_count(self: Self) u32 {
        return c_api.ts_language_symbol_count(self._c);
    }

    pub fn symbol_name(self: Self, symbol: Symbol) [:0]const u8 {
        return c_api.ts_language_symbol_name(self._c, symbol);
    }
    pub fn symbol_for_name(self: Self, string: []const u8, is_named: bool) Symbol {
        return c_api.ts_language_symbol_for_name(self._c, string, string.len, is_named);
    }
    pub fn field_count(self: Self) u32 {
        return c_api.ts_language_field_count(self._c);
    }
    pub fn field_name_for_id(self: Self, field_id: FieldId) []const u8 {
        // FIXME: implicitly converts from [:0] const u8
        return c_api.ts_language_field_name_for_id(self._c, field_id);
    }
    pub fn field_id_for_name(self: Self, name: []const u8) FieldId {
        return c_api.ts_language_field_id_for_name(self._c, name, name.len);
    }
    pub fn symbol_type(self: Self, symbol: Symbol) SymbolType {
        return c_api.ts_language_symbol_type(self._c, symbol);
    }
    pub fn version(self: Self) u32 {
        return c_api.ts_language_version(self._c);
    }
};

/// generate a type from the tree-sitter header
fn wrapType(comptime name: []const u8, comptime Type: type) type {
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
                .{ .name = "_c", .field_type = *Type, .default_value = undefined, .is_comptime = false, .alignment = 8 }
            },
            .decls = decls,
            .is_tuple = false
        }
    });
}

test "wrapType" {
    _ = wrapType;
//     // unused because wrapType's iteration of all fields in c_api drags in unsupported stuff like glibc long double functions
//     const Language = wrapType("ts_language", c_api.TSLanguage);
//     std.meta.eql(
//         std.meta.declarations(@Type(Language)),
//         [_]std.builtin.TypeInfo.Declaration{
//             .{ .name = "symbol_name", .data = c_api.ts_language_symbol_name },
//             .{ .name = "symbol_for_name", .data = c_api.ts_language_symbol_for_name },
//             .{ .name = "field_count", .data = c_api.ts_language_field_count },
//             .{ .name = "field_name_for_id", .data = c_api.ts_language_field_name_for_id },
//             .{ .name = "field_id_for_name", .data = c_api.ts_language_field_id_for_name },
//             .{ .name = "symbol_type", .data = c_api.ts_language_symbol_type },
//             .{ .name = "version", .data = c_api.ts_language_version },
//         }
//     );
//     const lang = Language.new();
//     _ = lang;
//     defer {
//         lang.free();
//     }
}
