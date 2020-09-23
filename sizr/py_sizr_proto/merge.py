"""
ast merging utils
"""

import libcst as cst
from inspect import isclass
from typing import Sequence, Iterator, List
from itertools import product

cst_node_nums = dict(enumerate(filter(
    lambda c: isclass(c) and issubclass(c, cst.CSTNode),
    cst.__dict__.values())))


def hashTree():  # TODO
    ...


def mergeSequences(a: List[cst.CSTNode], b: List[cst.CSTNode]) -> Sequence[cst.CSTNode]:
    # XXX: too much complexity, need to adapt myers diff algorithm instead, with hashed trees/source
    collisions = list(filter(lambda p: p[0][1].deep_equals(
        p[1][1]), product(enumerate(a), enumerate(b))))
    last_a_index = 0
    last_b_index = 0
    for (a_index, _), (b_index, b_item) in collisions:
        print(a_index, b_index)
        print('yield from', a[last_a_index+1:a_index])
        yield from a[last_a_index+1:a_index]
        print('yield from', b[last_b_index+1:b_index])
        yield from b[last_b_index+1:b_index]
        print('yield', b_item)
        yield b_item
        last_a_index = a_index
        last_b_index = b_index
    yield from a[last_a_index+1:]
    print('yield from', a[last_a_index+1:])
    yield from b[last_b_index+1:]
    print('yield from', b[last_b_index+1:])
