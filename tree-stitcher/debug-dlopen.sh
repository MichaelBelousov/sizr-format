#!/usr/bin/bash

cat debug-dlopen.c | clang -x c - && ./a.out zig-out/lib/libbindings.so
rm a.out

