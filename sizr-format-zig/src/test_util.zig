//! various utilities for tests in this code base

const std = @import("std");
const dbglog = @import("./util.zig").dbglog;
const dbglogv = @import("./util.zig").dbglogv;
const testing = std.testing;
const ts = @import("./tree_sitter.zig");
const code = @import("./code.zig");
const cpp = @import("./cpp.zig");

pub const simpleTestSource = 
    \\void test(){}
;

/// testing utilities
/// uses a shared 4KB buffer
pub var write_commands = struct {
    buf: [4 * 1024]u8,

    /// given a C++ translation unit source string, a write-command, expect a given string to be written
    pub fn expectWrittenStringStatic(self: *@This(), src: []const u8, comptime wcmd: code.WriteCommand, expected: []const u8) !void {
        dbglog("\n");
        const bufWriter = std.io.fixedBufferStream(&self.buf).writer();
        const TestLanguageFormat = struct { fn nodeFormats(_: u16) code.WriteCommand { return wcmd; } };
        var ctx = code.EvalCtx(@TypeOf(bufWriter)).init(.{
            .source = src,
            .writer = bufWriter,
            .allocator = std.testing.allocator,
            .desiredLineSize = 60,
            .languageFormat = code.LanguageFormat{
                .nodeFormats = TestLanguageFormat.nodeFormats,
                .aliasing = cpp.languageFormat.aliasing,
                .nodeTypeFromName = cpp.languageFormat.nodeTypeFromName,
                .nodeKeyFromName = cpp.languageFormat.nodeKeyFromName,
                .aliasKeyFromName = cpp.languageFormat.aliasKeyFromName,
                .rootNodeType = 0,
            }
        }) catch unreachable;
        defer ctx.free(std.testing.allocator);
        ctx.writeSrc() catch unreachable;
        bufWriter.writeByte(0) catch unreachable;
        const len = 1 + (std.mem.indexOf(u8, self.buf[0..], "\x00") orelse self.buf.len);
        dbglogv("buf content: '{s}'\n", .{self.buf[0..len]});
        return testing.expectEqualStrings(expected, self.buf[0..len], );
    }

    /// given a C++ translation unit source string, a write-command, expect a given string to be written
    pub fn expectWrittenStringCpp(self: *@This(), src: []const u8, expected: []const u8) !void {
        dbglog("\n");
        const bufWriter = std.io.fixedBufferStream(&self.buf).writer();
        var ctx = code.EvalCtx(@TypeOf(bufWriter)).init(.{
            .source = src,
            .writer = bufWriter,
            .allocator = std.testing.allocator,
            .desiredLineSize = 60,
            .languageFormat = cpp.languageFormat,
        }) catch unreachable;
        defer ctx.free(std.testing.allocator);
        ctx.writeSrc() catch unreachable;
        bufWriter.writeByte(0) catch unreachable;
        const len = 1 + (std.mem.indexOf(u8, self.buf[0..], "\x00") orelse self.buf.len);
        dbglogv("buf content: '{s}'\n", .{self.buf[0..len]});
        return testing.expectEqualStrings(expected, self.buf[0..len], );
    }
}{
    .buf = undefined,
};