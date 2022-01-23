
const _2: u32 = 2;
const _4: u32 = 4;
const _5: u32 = 5;
const _6: u32 = 6;

// works if I make it return not optional
fn f(param: u32) ?u32 {
    return if (param == 1) _2
    else   if (param == 2) _4
    else   if (param == 3) _5
    else                   _6;
}

pub fn main() void {
    _ = f(7);
}
