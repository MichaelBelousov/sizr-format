// works if I make it return not optional
fn f(use: bool, param: u32) ?u32 {
    var val: u32 = 0;
    return switch (use) {
        true => (               // SwitchProng
            if (param == 1)
                @as(u32, 2)     // Then
            else                // Else
                if (val == 3)
                    @as(u32, 4) // Then1
                else            // Else2
                    @as(u32, 5)
        ),
        else =>                 // SwitchElse
            @as(u32, 6)
    };
}

pub fn main() void {
    _ = f(true, 7);
}
