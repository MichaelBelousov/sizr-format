#!/usr/bin/bash

clang -x c - <<EOF && ./a.out zig-out/lib/libbindings.so
#include <stdio.h>
#include <dlfcn.h>
#include <assert.h>

int main(int argc, const char** argv) {
  assert(argc >= 2);
  void* handle = dlopen(argv[1], RTLD_NOW);
  printf("arg: %s\n", argv[1]);
  printf("handle: %p\n", handle);
  printf("error: %s\n", dlerror());
}
EOF

rm a.out

