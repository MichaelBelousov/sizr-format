const std = @import("std");
const json = std.json;
const clap = @import("zig-clap");

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

pub fn main() !void {

}

