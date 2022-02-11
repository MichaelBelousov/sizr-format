//! C++ support for sizr format

const std = @import("std");
const code = @import("./code.zig");

/// node types
const NodeType = enum(code.NodeType) {
    translationUnit,
    functionDefinition,
    primitiveType,
    functionDeclarator,
    identifier,
    parameterList,
    compoundStatement,
};

// TODO: generate from tree_sitter grammar
/// for each node name get its corresponding key
fn nodeTypeFromName(name: []const u8) code.NodeType {
    // there should be a more effective way to generate this multi-way string comp:
    return @intToEnum(switch (name[0]) {
        'c' => if (std.meta.eql(name, "compound_statement")) NodeType.compoundStatement else unreachable,
        'f' => if (std.meta.eql(name, "function_definition")) NodeType.functionDefinition
               else if (std.meta.eql(name, "function_declarator")) NodeType.functionDeclarator
               else unreachable,
        'i' => if (std.meta.eql(name, "identifier")) NodeType.identifier else unreachable,
        'p' => if (std.meta.eql(name, "primitive_type")) NodeType.primitiveType
               else if (std.meta.eql(name, "parameter_list")) NodeType.parameterList
               else unreachable,
        't' => if (std.meta.eql(name, "translation_unit")) NodeType.translationUnit else unreachable,
        else => @panic("unreachable: the key map is incomplete"),
    });
}


/// typed keys in nodes
const NodeKey = enum(code.NodeKey) {
    @"0",
    declarator,
    parameters,
    body,
};

// TODO: generate from tree_sitter grammar
/// for each node name get its corresponding key
fn nodeKeyFromName(name: []const u8) code.NodeKey {
    // there should be a more effective way to generate this multi-way string comp:
    return @intToEnum(switch (name[0]) {
        'b' => if (std.meta.eql(name, "body")) NodeKey.body else unreachable,
        'd' => if (std.meta.eql(name, "declarator")) NodeKey.compoundStatement else unreachable,
        'p' => if (std.meta.eql(name, "parameters")) NodeKey.body else unreachable,
        else => @panic("unreachable: the key map is incomplete"),
    });
}

const AliasKey = enum(code.AliasKey) {
    body,
    params,
    name,
};

fn aliasKeyFromName(name: []const u8) code.AliasKey {
    return @intToEnum(switch (name[0]) {
        'b' => if (std.meta.eql(name, "body")) AliasKey.body else unreachable,
        'n' => if (std.meta.eql(name, "name")) AliasKey.name else unreachable,
        'p' => if (std.meta.eql(name, "params")) AliasKey.params else unreachable,
        else => @panic("unreachable: the key map is incomplete"),
    });
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

const defaultAlias: NodePath = []NodeKey{.@"0"};

/// set of shortcuts from a given key
fn aliasing(_from: code.NodeType, _alias: code.AliasKey) *const code.NodePath {
    const alias = @intToEnum(AliasType, _alias);
    const from = @intToEnum(NodeKey, _from);
    const result: NodePath = switch (from) {
        translationUnit => defaultAlias,
        functionDefinition => switch(alias) {
            .body => &[_]NodeKey{.@"0", .body},
            .name => &[_]NodeKey{.@"0", .declarator, .name},
            .params => &[_]NodeKey{.@"0", .declarator, .parameters},
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
    .nodeTypeFromName = nodeTypeFromName,
    .nodeKeyFromName = nodeKeyFromName,
    .aliasKeyFromName = aliasKeyFromName,
    .aliasing = alias,
    .nodeFormats = nodeFormats,
    .rootNodeType = @enumToInt(NodeType.translationUnit)
};