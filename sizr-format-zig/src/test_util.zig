//! various utilities for tests in this code base

const ts = @import("./tree_sitter.zig");

/// get a simple node for use in tests that must operate on ts.Node
pub fn simpleTestNode() ts.Node {
    const src =
        \\void test(){}
    ;
    const parser = ts.Parser.new();
    // FIXME! need to figure out how to let the caller dealloc the parser deallocation
    //defer parser.free();
    if (!parser.set_language(ts.cpp()))
        @panic("couldn't set cpp lang");
    const tree = parser.parse_string(null, src);
    //defer ts._c.ts_tree_delete(tree._c);
    const root = ts._c.ts_tree_root_node(tree._c);
    return ts.Node{ ._c = root };
}