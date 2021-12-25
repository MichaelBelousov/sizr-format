const c_api = @cImport({
    @cInclude("tree_sitter/api.h");
    //@cInclude("api.h");
});

const TsParser = struct {
    _ts_parser: [*c]c_api.TSParser,

    const Self = @This();

    fn init() Self {
        return Self{
            ._ts_parser = c_api.ts_parser_new(),
        };
    }

    // TODO: check zig idioms
    fn free(self: Self) void {
        c_api.ts_parser_delete(self._ts_parser);
    }
};

test "TsParser" {
    var parser = TsParser.init();
    defer {
        parser.delete();
    }
}
