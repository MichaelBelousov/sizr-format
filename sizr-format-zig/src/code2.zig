fn f(param: u32) ?u32 {
    return if (param == 1) @as(u32, 2)
           else            @as(u32, 3);
}

pub fn main() void {
    _ = f(4);
}
