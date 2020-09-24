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
    def test_1(self):
        self.assertEqual("")


if __name__ == '__main__':
    unittest.main()
