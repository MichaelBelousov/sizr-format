# I know so far that switch/else-if triggers this issue
# I tried converting it to if/else-if and it didn't work

~/personal/zig/build/bin/zig build-exe src/code.zig \
  --strip -O ReleaseSmall --verbose-llvm-ir \
  2>&1 \
  # > out.llvm.ir

# how to increment a portion of instrs in an llvm ir dump
# getclip | python3 -c 'import sys,re;print(re.sub(r"%(\d+)", lambda m: f"%{y+1 if (y:=int(m[1])) > 30 else y}", sys.stdin.read()))' | putclip


