const c_api = @cImport({
    @cInclude("tree_sitter/api.h");
});

// TODO: doing this by hand for now but some kind of guided clang plugin could probably generate bindings better
const TsParser = struct {
    _ts_parser: *c_api.TSParser,

    const Self = @This();

    fn init() Self {
        const maybe_c_parser = c_api.ts_parser_new();
        if (maybe_c_parser) |c_parser| {
            return Self{ ._ts_parser = c_parser };
        } else unreachable;
    }

    // TODO: check zig idioms
    fn free(self: Self) void {
        c_api.ts_parser_delete(self._ts_parser);
    }

    fn set_language(self: Self, language: TSLanguage) void {
        c_api.ts_parser_set_language(self._ts_parser, language.c);
    }
};

const TsNode = struct {
    _ts_node: *c_api.TSNode,
};

const TsLanguage = struct {
    _c: *c_api.TSLanguage,
};

test "TsParser" {
    var parser = TsParser.init();
    defer {
        parser.free();
    }
}
