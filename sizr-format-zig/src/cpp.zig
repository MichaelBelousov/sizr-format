//! C++ support for sizr format

const std = @import("std");
const code = @import("./code.zig");

// probably want to generate this from tree_sitter grammar for C++
fn nodeFormats(key: []const u8) code.WriteCommand {
    // FIXME: need to use tree-sitter support of non-string based AST
    // node type tags instead of expensive string checks
    return if (std.meta.eql(key, "translation-unit")) (
        code.WriteCommand{.trivial={}}
    ) else if (std.meta.eql(key, "function_definition")) (
        code.WriteCommand{.trivial={}}
    ) else if (std.meta.eql(key, "primitive_type")) (
        code.WriteCommand{.trivial={}}
    ) else if (std.meta.eql(key, "function_declarator")) (
        code.WriteCommand{.trivial={}}
    ) else if (std.meta.eql(key, "identifier")) (
        code.WriteCommand{.trivial={}}
    ) else if (std.meta.eql(key, "parameter_list")) (
        code.WriteCommand{.trivial={}}
    ) else if (std.meta.eql(key, "compound_statement")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else (
        // just for dev, in the future this should be unreachable 
        code.WriteCommand{.trivial={}}
        //@panic("The cpp node format map is incomplete, this is a bug.")
    );
}

pub const languageFormat = code.LanguageFormat{
    .nodeFormats = nodeFormats,
    .rootNodeKey = "translation_unit"
};