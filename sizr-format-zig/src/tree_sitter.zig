const c_api = @cImport({
    @cInclude("tree_sitter/api.h");
});

// TODO: doing this by hand for now but some kind of guided clang plugin could probably generate bindings better
const TsParser = struct {
    _c: *c_api.TSParser,

    const Self = @This();

    fn init() Self {
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
        return TsTree{ ._c = c_api.ts_parser_parse_string(self._c, if (old_tree) |old_tree_val| old_tree_val._c else null, src_string, src_string.len) };
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

test "TsLanguage" {}
