#!/bin/bash

script_dir="$(dirname $0)"

# tests
find $script_dir/tests -name '*.test.scm' | sed -e 's,^,(load ",' -e 's,$,"),' \
  | chibi-scheme
# TODO: I think I can just pass $(find ...) to chibi-scheme

