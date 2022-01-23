// works if I make it return not optional
fn f(param: u32) ?u32 {
    return
    if (param == 1) @as(u32, 2)
    else if (param == 2) @as(u32, 4)
        else if (param == 3) @as(u32, 5)
            else @as(u32, 6);
}

pub fn main() void {
    _ = f(7);
}
