~/personal/zig/build/bin/zig test src/code.zig \
  --strip -O ReleaseFast --verbose-llvm-ir -fsingle-threaded -fno-unwind-tables -fno-compiler-rt -mno-red-zone -fno-PIE -fno-sanitize-c -dynamic \
  2>&1 \
  # > out.llvm.ir
