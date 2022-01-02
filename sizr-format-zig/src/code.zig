// works if I make it return not optional
fn f(use: bool, param: u32) ?u32 {
    var val: u32 = 0;
    return switch (use) {
        true => (               // SwitchProng
            if (param == 1)
                @as(u32, 2)     // Then
            else if (val == 3)  // Else
                    @as(u32, 4) // Then1 ; here is where a return value address is created
                else
                    @as(u32, 5) // Else2 ; here is where the return value address from the other branch is mistakenly consumed
        ),
        else =>                 // SwitchElse
            @as(u32, 6)
    };
}

pub fn main() void {
    _ = f(true, 7);
}
