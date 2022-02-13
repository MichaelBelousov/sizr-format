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
    // need to generate these by invoking tree_sitter
    translationUnit = 165, // are these hackily gathered ones even right?
    functionDefinition = 166,
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
    @"type" = 32,
    declarator = 9,
    parameters,
    body = 5,
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
    @"0" = 0,
    body,
    params,
    name,
    returnType,
};

fn aliasKeyFromName(name: []const u8) code.AliasKey {
    return @enumToInt(switch (name[0]) {
        'b' => if (std.mem.eql(u8, name, "body")) AliasKey.body else dbgunreachable(name),
        'n' => if (std.mem.eql(u8, name, "name")) AliasKey.name else dbgunreachable(name),
        'p' => if (std.mem.eql(u8, name, "params")) AliasKey.params else dbgunreachable(name),
        'r' => if (std.mem.eql(u8, name, "returnType")) AliasKey.returnType else dbgunreachable(name),
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
        .declarator => .trivial,
        .parameters => .trivial,
        .body => .trivial,
        .type => .trivial,
        //else => .trivial
    };
}

pub const NodePath = []const NodeKey;

// rename to trivialAliasPath or something
pub const defaultAliasPath: NodePath = &[_]NodeKey{.@"0"};

/// set of shortcuts from a given key
fn aliasing(_from: code.NodeType, _alias: code.AliasKey) *const code.NodePath {
    const alias = @intToEnum(AliasKey, _alias);
    // @"0" is the null alias it seems...
    const from = @intToEnum(NodeType, _from);
    const result: *const NodePath =
    if (alias == .@"0")
        &defaultAliasPath
    else switch (from) {
        .translationUnit => &defaultAliasPath,
        .functionDefinition => switch(alias) {
            .@"0" => &defaultAliasPath,
            .body => &@as(NodePath, &[_]NodeKey{.body}),
            .name => &@as(NodePath, &[_]NodeKey{.declarator, .declarator}),
            .params => &@as(NodePath, &[_]NodeKey{.declarator, .parameters}),
            .returnType => &@as(NodePath, &[_]NodeKey{.type}),
            //else => unreachable,
        },
        .functionDeclarator => &defaultAliasPath,
        .primitiveType => &defaultAliasPath,
        .identifier => &defaultAliasPath,
        .parameterList => &defaultAliasPath,
        .compoundStatement => &defaultAliasPath,
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