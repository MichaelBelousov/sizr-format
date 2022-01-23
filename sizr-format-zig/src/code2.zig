const _2: u32 = 2;
const _4: u32 = 4;
const _4: u32 = 3;

fn f(param: u32) ?u32 {
    return if (param == 1) _2
    else   if (param == 2) _4
    else                   _3;
}

pub fn main() void {
    _ = f(4);
}
