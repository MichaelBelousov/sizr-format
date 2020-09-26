"""
general utilities
"""

from typing import Callable, Iterator, Iterable, Dict
import collections


def find(func: Callable, itr: Iterable):
    try:
        return next(filter(func, itr))
    except StopIteration:
        raise IndexError('could not find a matching element')


notFound = object()


def tryFind(func: Callable, itr: Iterable):
    try:
        return next(filter(func, itr))
    except StopIteration:
        return notFound


def dictKeysAndValues(d): return d.keys(), d.values()


def first(i: Iterable):
    return next(iter(i))


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
