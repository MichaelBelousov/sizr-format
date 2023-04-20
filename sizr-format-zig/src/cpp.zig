//! C++ support for sizr format

const std = @import("std");
const code = @import("./code.zig");
const ts = @import("./tree_sitter.zig");

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
    functionDefinition = 186,
    primitiveType = 78,
    functionDeclarator = 216,
    identifier = 1,
    parameterList = 239,
    compoundStatement = 225,
    @"(" = 5,
    @")" = 8,
    @"{" = 59,
    @"}" = 60,
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
    parameters = 24,
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
fn nodeFormats(_key: code.NodeType) code.WriteCommand {
    std.debug.print("nodeFormats> key: {}\n", .{_key});
    std.debug.print("nodeFormats> field: {any}\n", .{ts.cpp().field_name_for_id(_key)});
    std.debug.print("nodeFormats> symbol: {any}\n", .{ts.cpp().symbol_name(_key)});
    const key = @intToEnum(NodeType, _key);
    // FIXME: need to use tree-sitter support of non-string based AST
    // node type tags instead of expensive string checks
    return switch (key) {
        .translationUnit => .trivial,
        .functionDefinition => .trivial,
        .primitiveType => .trivial,
        .functionDeclarator => .trivial,
        .identifier => .trivial,
        .parameterList => .trivial,
        .compoundStatement => .trivial,
        .@"(" => code.WriteCommand{.raw="("},
        .@")" => code.WriteCommand{.raw=")"},
        .@"{" => code.WriteCommand{.raw="{"},
        .@"}" => code.WriteCommand{.raw="}"},
        // TODO: remove by generating exhaustively
        //else => .trivial,
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
        // TODO: remove once generating the entire alias/virtual field map
        else => &defaultAliasPath,
    };
    return @ptrCast(*const code.NodePath, result);
}

pub const languageFormat = code.LanguageFormat{
    .nodeTypeFromName = nodeTypeFromName,
    .nodeKeyFromName = nodeKeyFromName,
    .aliasKeyFromName = aliasKeyFromName,
    .aliasing = aliasing,
    .nodeFormats = nodeFormats,
};
