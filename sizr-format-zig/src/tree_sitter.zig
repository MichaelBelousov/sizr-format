const c_api = @cImport({
    @cInclude("tree_sitter/api.h");
});

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
};

test "TsParser" {
    var parser = TsParser.init();
    defer {
        parser.free();
    }
}
