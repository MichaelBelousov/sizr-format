const builtin = @import("builtin");
const std = @import("std");
const json = std.json;
const clap = @import("zig-clap");
// not used and therefore ignored off posix
const mman = @cImport({ @cInclude("sys/mman.h"); });

const sexp = union (enum) {
    str: []const u8,
    symbol: []const u8,
    pair: struct {
        car: *sexp,
        cdr: *sexp,
    },

    pub fn write(self: @This(), writer: *std.io.Writer) !void {
        switch (self) {
            .str => |data| writer.write(data),
            .symbol => |data| {
                writer.write("'");
                writer.write(data);
            },
            .pair => |data| {
                writer.write("(");
                data.car.write(writer);
                writer.write(" ");
                data.cdr.write(writer);
                writer.write(")");
            },
        }
    }
};

/// A buffer that contains the entire contents of a file
const FileBuffer = struct {
    buffer: []const u8,

    const Self = @This();

    pub fn from_path(alloc: std.mem.Allocator, path: []const u8) !Self {
        const file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();
        var file_len = (try file.stat()).size;

        switch (builtin.os.tag) {
            .windows => {
                const buffer = alloc.alloc(u8, file_len);
                file.readAll(buffer);
                return Self{ .buffer = buffer };
            },
            // assuming posix currently
            else => {
                var src_ptr = @alignCast(
                    std.mem.page_size,
                    std.c.mmap(null, file_len, mman.PROT_READ, mman.MAP_FILE | mman.MAP_SHARED, file.handle, 0)
                );

                if (src_ptr == mman.MAP_FAILED) {
                    var mmap_result = std.c.getErrno(@ptrToInt(src_ptr));
                    if (mmap_result != .SUCCESS) {
                        std.debug.print("mmap errno: {any}\n", .{ mmap_result });
                        @panic("mmap failed");
                    }
                }

                const buffer = @ptrCast([*]const u8, src_ptr)[0..file_len];
                return Self{ .buffer = buffer };
            }
        }
    }

    pub fn free(self: Self, alloc: std.mem.Allocator) !void {
        switch (builtin.os.tag) {
            .windows => {
                alloc.free(self.buffer);
            },
            else => {
                const munmap_result = std.c.munmap(@alignCast(std.mem.page_size, self.buffer.ptr), self.buffer.len);
                const errno = std.c.getErrno(munmap_result);
                if (errno != .SUCCESS)
                    std.log.err("munmap errno: {any}", .{ errno });
                return errno;
            }
        }
    }
};

const Grammar = struct {
    name: []const u8,
    word: ?[]const u8,
    /// interface Alias {
    ///   type: "ALIAS",
    ///   content: Rule,
    ///   named: boolean,
    ///   value: string,
    /// }
    /// interface Rule {
    ///   type: "CHOICE" | "REPEAT" | "SEQ" | "ALIAS" | "PATTERN"
    ///       | "STRING" | "FIELD" | "IMMEDIATE_TOKEN" | "BLANK",
    ///   name?: string,
    ///   members?: Rule[],
    /// }
    rules: json.ValueTree,
    extras: []struct { type: []const u8, name: []const u8 },
    conflicts: [][][]const u8,
    precedences: [][]const u8,
    externals: []struct { type: []const u8, name: []const u8 },
    @"inline": [][]const u8,
    supertypes: [][]const u8,
};

// high-level overview:
// for each named grammar rule (not starting with a `_`), construct a scheme function definition
// with default tokens between fields
// like so:
//
// "system_lib_string": {
//   "type": "TOKEN",
//   "content": {
//     "type": "SEQ",
//     "members": [
//       {
//         "type": "STRING",
//         "value": "<"
//       },
//       {
//         "type": "REPEAT",
//         "content": {
//           "type": "CHOICE",
//           "members": [
//             {
//               "type": "PATTERN",
//               "value": "[^>\\n]"
//             },
//             {
//               "type": "STRING",
//               "value": "\\>"
//             }
//           ]
//         }
//       },
//       {
//         "type": "STRING",
//         "value": ">"
//       }
//     ]
//   }
// },
//
// (define (system_lib_string token) (token))
// 
// this simple example demonstrates that this is too complicated and I will start with
// hand writing it per language I guess...

pub fn convertGrammars(allocator: std.mem.Allocator, grammar_paths: []const []const u8) !void {
    var parser = json.Parser.init(allocator, false);
    defer parser.deinit();

    if (grammar_paths.len == 0) {
        std.debug.print("no grammars provided\n", .{});
        return;
    }

    for (grammar_paths) |rel_path| {
        // TODO: don't use realpath, just some path join operation
        const abs_path = try std.fs.cwd().realpathAlloc(allocator, rel_path);
        const grammar_file = try FileBuffer.from_path(allocator, abs_path);
        // TODO: use typed json parsing with Grammar type
        const grammar = try parser.parse(grammar_file.buffer);
        //std.debug.print("grammar: {any}\n", .{grammar.root.get("name")});
        std.debug.print("{s}\n", .{grammar.root.Object.get("name").?.String});
        const rules = grammar.root.Object.get("rules").?.Object.?;
        const top_level_rule = rules.keys()[0];
        std.debug.print("{s}\n", .{grammar.root.Object.get("name").?.String});
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const cli_params = comptime clap.parseParamsComptime(
        \\-h, --help    Display this help and exit
        \\<str>...
        \\
    );

    var diag = clap.Diagnostic{};
    var cli_args = clap.parse(clap.Help, &cli_params, clap.parsers.default, .{
        .diagnostic = &diag,
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch unreachable;
        return err;
    };
    defer cli_args.deinit();

    if (cli_args.args.help)
        return clap.usage(std.io.getStdErr().writer(), clap.Help, &cli_params);
    
    try convertGrammars(allocator, cli_args.positionals);
}

// TODO: create automated tests for grammars like c++
