//! C++ support for sizr format

const std = @import("std");
const code = @import("./code.zig");

fn dbgunreachable(str: []const u8) noreturn {
    if (std.os.getenv("DEBUG") != null) {
        std.debug.print("value: '{s}'\n", .{str});
    }
    unreachable;
}

/// node types
pub const NodeType = enum(code.NodeType) {
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
    return @enumToInt(switch (name[0]) {
        'c' => if (std.mem.eql(u8, name, "compound_statement")) NodeType.compoundStatement else dbgunreachable(name),
        'f' => if (std.mem.eql(u8, name, "function_definition")) NodeType.functionDefinition
               else if (std.mem.eql(u8, name, "function_declarator")) NodeType.functionDeclarator
               else dbgunreachable(name),
        'i' => if (std.mem.eql(u8, name, "identifier")) NodeType.identifier else dbgunreachable(name),
        'p' => if (std.mem.eql(u8, name, "primitive_type")) NodeType.primitiveType
               else if (std.mem.eql(u8, name, "parameter_list")) NodeType.parameterList
               else dbgunreachable(name),
        't' => if (std.mem.eql(u8, name, "translation_unit")) NodeType.translationUnit else dbgunreachable(name),
        else => dbgunreachable(name),
    });
}


/// typed keys in nodes
pub const NodeKey = enum(code.NodeKey) {
    @"0" = 0,
    declarator,
    parameters,
    body,
};

// TODO: generate from tree_sitter grammar
/// for each node name get its corresponding key
fn nodeKeyFromName(name: []const u8) code.NodeKey {
    // there should be a more effective way to generate this multi-way string comp:
    return @enumToInt(switch (name[0]) {
        'b' => if (std.mem.eql(u8, name, "body")) NodeKey.body else dbgunreachable(name),
        'd' => if (std.mem.eql(u8, name, "declarator")) NodeKey.declarator else dbgunreachable(name),
        'p' => if (std.mem.eql(u8, name, "parameters")) NodeKey.body else dbgunreachable(name),
        else => dbgunreachable(name),
    });
}

pub const AliasKey = enum(code.AliasKey) {
    body,
    params,
    name,
};

fn aliasKeyFromName(name: []const u8) code.AliasKey {
    return @enumToInt(switch (name[0]) {
        'b' => if (std.mem.eql(u8, name, "body")) AliasKey.body else dbgunreachable(name),
        'n' => if (std.mem.eql(u8, name, "name")) AliasKey.name else dbgunreachable(name),
        'p' => if (std.mem.eql(u8, name, "params")) AliasKey.params else dbgunreachable(name),
        else => dbgunreachable(name),
    });
}

// probably want to generate this from tree_sitter grammar for C++
fn nodeFormats(_key: code.NodeKey) code.WriteCommand {
    const key = @intToEnum(NodeKey, _key);
    // FIXME: need to use tree-sitter support of non-string based AST
    // node type tags instead of expensive string checks
    return switch (key) {
        .@"0" => code.WriteCommand{.ref=.{.name=code.Expr{.name=0}}},
        .declarator => code.WriteCommand{.trivial={}},
        .parameters => code.WriteCommand{.trivial={}},
        .body => code.WriteCommand{.trivial={}},
    };
}

pub const NodePath = []const NodeKey;

pub const defaultAlias: NodePath = &[_]NodeKey{.@"0"};

/// set of shortcuts from a given key
fn aliasing(_from: code.NodeType, _alias: code.AliasKey) *const code.NodePath {
    const from = @intToEnum(NodeType, _from);
    const alias = @intToEnum(AliasKey, _alias);
    const result: *const NodePath = switch (from) {
        .translationUnit => &defaultAlias,
        .functionDefinition => switch(alias) {
            .body => &@as(NodePath, &[_]NodeKey{.@"0", .body}),
            .name => &@as(NodePath, &[_]NodeKey{.@"0", .declarator, .declarator}),
            .params => &@as(NodePath, &[_]NodeKey{.@"0", .declarator, .parameters}),
        },
        .functionDeclarator => &defaultAlias,
        .primitiveType => &defaultAlias,
        .identifier => &defaultAlias,
        .parameterList => &defaultAlias,
        .compoundStatement => &defaultAlias,
    };
    return @ptrCast(*const code.NodePath, result);
}

pub const languageFormat = code.LanguageFormat{
    .nodeTypeFromName = nodeTypeFromName,
    .nodeKeyFromName = nodeKeyFromName,
    .aliasKeyFromName = aliasKeyFromName,
    .aliasing = aliasing,
    .nodeFormats = nodeFormats,
    .rootNodeType = @enumToInt(NodeType.translationUnit)
};