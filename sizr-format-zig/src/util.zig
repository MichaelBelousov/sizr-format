const expect = @import("std").testing.expect;
const expectError = @import("std").testing.expectError;

// based on std.mem.indexOfAny
pub fn indexOfNotAny(comptime T: type, slice: []const T, values: []const T) ?usize {
    var i: usize = 0;
    while (i < slice.len) : (i += 1) {
        var wasInValues = false;
        for (values) |value| {
            if (slice[i] == value)
                wasInValues = true;
        }
        if (!wasInValues) return i;
    }
    return null;
}

test "indexOfNotAny" {
    const vals = [_]i64{ 10, 17, 25 };
    const items = [_]i64{ 1, 2, 3, 4, 5, 6, 7 };
    try expect(indexOfNotAny(i64, &items, &[_]i64{ 1, 2, 3 }) == @as(usize, 3));
    try expect(indexOfNotAny(i64, &items, &[_]i64{ 8, -5, 0 }) == @as(usize, 0));
    try expect(indexOfNotAny(i64, &items, &items) == null);
    try expect(indexOfNotAny(i64, &[_]i64{ 1, 2, 1, 2, 1, 2 }, &[_]i64{ 1, 2 }) == null);
}
