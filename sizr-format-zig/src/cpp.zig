//! C++ support for sizr format

const std = @import("std");
const code = @import("./code.zig");

/// typed keys in nodes
const NodeKey = enum(code.NodeKey) {
    _in, // placeholder for 0 for now
    declarator,
    parameters,
    body,
};

const AliasKey = enum(code.AliasKey) {
    body,
    params,
    name,
};

// TODO: generate from tree_sitter grammar
/// for each node name get its corresponding key
fn nodeKeyFromName(name: []const u8) NodeKey {
    // there should be a more effective way to generate this multi-way string comp:
    return switch (name[0]) {
        'c' => if (std.meta.eql(name, "compound_statement")) .compoundStatement else unreachable,
        'f' => if (std.meta.eql(name, "function_definition")) .functionDefinition
               else if (std.meta.eql(name, "function_declarator")) .functionDeclarator
               else unreachable,
        'i' => if (std.meta.eql(name, "identifier")) .identifier else unreachable,
        'p' => if (std.meta.eql(name, "primitive_type")) .primitiveType
               else if (std.meta.eql(name, "parameter_list")) .parameterList
               else unreachable,
        't' => if (std.meta.eql(name, "translation_unit")) .translationUnit else unreachable,
        else => @panic("unreachable: the key map is incomplete"),
    };
}

fn aliasKeyFromName(name: []const u8) AliasKey {
    return switch (name[0]) {
        'b' => if (std.meta.eql(name, "body")) .body else unreachable,
        'n' => if (std.meta.eql(name, "name")) .name else unreachable,
        'p' => if (std.meta.eql(name, "params")) .params else unreachable,
        else => @panic("unreachable: the key map is incomplete"),
    };
}

// probably want to generate this from tree_sitter grammar for C++
fn nodeFormats(_key: code.NodeKey) code.WriteCommand {
    const key = @intToEnum(NodeKey, _key);
    // FIXME: need to use tree-sitter support of non-string based AST
    // node type tags instead of expensive string checks
    return switch (key) {
        .translationUnit => code.WriteCommand{.trivial={}},
        .functionDefinition => code.WriteCommand{.trivial={}},
        .primitiveType => code.WriteCommand{.trivial={}},
        .functionDeclarator => code.WriteCommand{.trivial={}},
        .identifier => code.WriteCommand{.trivial={}},
        .parameterList => code.WriteCommand{.trivial={}},
        .compoundStatement => code.WriteCommand{.ref=.{.name=code.Expr{.name="0"}}},
        else => (
            // just for dev, in the future this should be unreachable 
            code.WriteCommand{.trivial={}}
            //@panic("The cpp node format map is incomplete, this is a bug.")
        )
    };
}

const NodePath = []const NodeKey;

const defaultAlias: NodePath = []NodeKey{._in};

/// set of shortcuts from a given key
fn aliasing(_alias: code.AliasKey, _from: code.NodeKey) *const code.NodePath {
    const alias = @intToEnum(AliasKey, _alias);
    const from = @intToEnum(NodeKey, _from);
    const result: NodePath = switch (from) {
        translationUnit => defaultAlias,
        functionDefinition => switch(alias) {
            .body => &[_]NodeKey{},
            .name => &[_]NodeKey{},
            .params => &[_]NodeKey{._in, .declarator},
        },
        functionDeclarator => defaultAlias,
        primitiveType => defaultAlias,
        identifier => defaultAlias,
        parameterList => defaultAlias,
        compoundStatement => defaultAlias,
    };
    return @ptrCast(code.NodePath, result);
}

const AliasKey = enum(code.AliasKey) {
    body,
    params,
    name,
};

pub const languageFormat = code.LanguageFormat{
    .nodeFormats = nodeFormats,
    .rootNodeKey = "translation_unit"
};