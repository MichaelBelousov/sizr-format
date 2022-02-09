//! C++ support for sizr format

const std = @import("std");
const code = @import("./code.zig");

// probably want to generate this from tree_sitter grammar for C++
fn nodeFormats(key: []const u8) code.WriteCommand {
    // FIXME: need to use tree-sitter support of non-string based AST
    // node type tags instead of expensive string checks
    return if (std.meta.eql(key, "translation-unit")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else if (std.meta.eql(key, "function_definition")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else if (std.meta.eql(key, "primitive_type")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else if (std.meta.eql(key, "function_declarator")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else if (std.meta.eql(key, "identifier")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else if (std.meta.eql(key, "parameter_list")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else if (std.meta.eql(key, "compound_statement")) (
        code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}}
    ) else (
        // kind of a bad idea during dev but this should be unreachable 
        @panic("The cpp node format map is incomplete, this is a bug.")
    );
}

pub const languageFormat = code.LanguageFormat{
    .nodeFormats = nodeFormats,
    .rootNodeKey = "translation_unit"
};