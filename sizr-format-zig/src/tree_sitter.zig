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

const TsLanguage = struct {
    _c: *c_api.TSLanguage,
};

fn wrapTsType(comptime name: []const u8, comptime TsType: type) type {
    const fields = comptime fields: {
        var i = @as(i32, 0);
        var result = [std.meta.fields(c_api).len:null]?std.builtin.TypeInfo.StructField;
        for (std.meta.fields(c_api)) |field| {
            if (std.mem.startsWith(field.name, name)) {
                result[i] = std.builtin.TypeInfo.StructField{
                    .name = field.name[name.len + 1..field.name.len],
                    .field_type = field.field_type,
                    .default_value = field.default_value,
                    .is_comptime = field.is_comptime,
                    .alignment = field.alignment,
                };
                i += 1;
            }
        }
        break :fields result;
    };
    return @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = fields,
            .decls = &[_]std.builtin.TypeInfo.Declaration{},
            .is_tuple = false
        }
    });
}

test "TsLanguage" {}