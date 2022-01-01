# I know so far that switch/else-if triggers this issue
# I tried converting it to if/else-if and it didn't work

~/personal/zig/build/bin/zig test src/code.zig \
  --strip -O ReleaseFast --verbose-llvm-ir -fsingle-threaded -fno-unwind-tables -fno-compiler-rt -mno-red-zone -fno-PIE -fno-sanitize-c -dynamic \
  2>&1 \
  # > out.llvm.ir

# how to increment a mass amount of llvm ir instructions

# getclip \
# | python3 -c 'import sys,re;print(re.sub(r"%(\d+)", lambda m: f"%{y+1 if (y:=int(m[1])) > 30 else y}", sys.stdin.read()))'
# | putclip
