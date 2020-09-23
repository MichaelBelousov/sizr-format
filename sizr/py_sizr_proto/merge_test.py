#!/usr/bin/python3
"""
test merge.py
"""

# TODO: figure out python unittest project organization conventions

import unittest
from textwrap import dedent
import merge
import libcst as cst


block_1 = """
pass
def myfunc(arg1):
    pass
x = 5
"""

block_2 = """
pass
def myfunc(arg1):
    y = 10
x = 5
"""

block_3 = """
pass
def myfunc(arg1):
    pass
x = 5

class C:
    pass
"""

block_4 = """
class C:
    pass
pass
def myfunc(arg1):
    pass
x = 5
"""


class MergeTests(unittest.TestCase):
    def test_merge_same(self):
        a = cst.parse_module(block_1).body
        b = cst.parse_module(block_1).body
        self.assertTrue(
            all(map(lambda received, expected: received.deep_equals(
                expected), merge.mergeSequences(a, b), a))
        )

    def test_merge_append(self):
        a = cst.parse_module(block_1).body
        b = cst.parse_module(block_3).body
        self.assertTrue(
            all(map(lambda received, expected: received.deep_equals(
                expected), merge.mergeSequences(a, b), a))
        )

    def test_merge_prepend(self):
        a = cst.parse_module(block_1).body
        b = cst.parse_module(block_4).body
        self.assertTrue(
            all(map(lambda received, expected: received.deep_equals(
                expected), merge.mergeSequences(a, b), a))
        )

    def test_merge_change_element(self):
        a = cst.parse_module(block_2).body
        b = cst.parse_module(block_3).body
        expected = cst.parse_module(dedent("""
        pass
        def myfunc(arg1):
            pass
        x = 5

        class C:
            pass
        """)).body
        print('#################')
        print(cst.Module(body=tuple(merge.mergeSequences(a, b))).code)
        print('#################')
        self.assertTrue(
            all(map(lambda received, expected: received.deep_equals(
                expected), merge.mergeSequences(a, b), expected))
        )


if __name__ == '__main__':
    unittest.main()
