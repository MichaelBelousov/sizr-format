"""
general utilities
"""

from typing import Callable, Iterator, Iterable, Dict, List
import collections
from operator import eq
from functools import reduce


def find(func: Callable, itr: Iterable):
    try:
        return next(filter(func, itr))
    except StopIteration:
        raise IndexError('could not find a matching element')


class NotFound:
    def __bool__(self):
        return False


notFound = NotFound()


def tryFind(func: Callable, itr: Iterable):
    try:
        return next(filter(func, itr))
    except StopIteration:
        return notFound


def dictKeysAndValues(d): return d.keys(), d.values()


def first(i: Iterable):
    return next(iter(i))


def tryFirst(i: Iterable):
    try:
        return next(iter(i))
    except StopIteration:
        return notFound


def only(i: Iterable):
    itr = iter(i)
    first = next(itr)
    try:
        next(itr)
    except StopIteration:
        return first
    raise ValueError("'only' requires one element be in the entire iteratable")


class FrozenDict(dict, collections.Mapping):
    def __init__(self, other: Dict):
        self._hash = None

    def __hash__(self):
        if self._hash is None:
            self._hash = hash(frozenset(self.items()))
        return self._hash

    def _disabled(self, *args, **kwargs):
        raise TypeError("FrozenDicts are immutable and can't be modified")

    __delattr__ = __setattr__ = __setitem__ = pop = update = setdefault = clear = popitem = _disabled


# TODO: probably rename to something like "partialPathMatch"
def stackPathMatches(path: List, stack: List) -> bool:
    """
    given a path, read down the stack and check that the path has been pushed on
    to the stack, matching if so
    """
    return all(map(eq, path, stack))
