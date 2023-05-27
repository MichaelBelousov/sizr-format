const std = @import("std");
const ts = @import("tree-sitter");

pub fn serialize_ts_tree(tree: ts.Tree, writer: *std.io.Writer) !void {
    const serializer = struct {
        tree: ts.Tree,
        writer: *std.io.Writer,

        const Self = @This();

        pub fn serialize(self: Self) !void {
            _ = self;
            // self.writer.write();
        }
    }{ .tree = tree, .writer = writer };

    return serializer.serialize(tree, writer);
}
