// works if I make it return not optional
fn f(use: bool, param: u32) ?u32 {
    var val: u32 = 9;
    //var 
    return switch (use) {
        true => (
            if (param == 8)
                @as(u32, 0)
            else if (val == 10)
                @as(u32, 1)
            else
                @as(u32, 2)
        ),
        else => @as(u32, 3)
    };
}

test "" {
    _ = f(true, 0);
}
