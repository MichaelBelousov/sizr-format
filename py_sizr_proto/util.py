"""
general utilities
"""

from typing import Callable, Iterator, Iterable, Dict, List, Tuple, Any
import collections
from operator import eq
from functools import reduce
import libcst as cst


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


class Opt():
    """ basically bad optional chaining ala typescript via proxy for python """

    def __init__(self, obj):
        self._obj = obj

    def __getattr__(self, key):
        return Opt(getattr(self._obj, key))


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


def partialPathMatch(path: List, stack: List) -> bool:
    """
    given a path, read down the stack and check that the path has been pushed on
    to the stack, matching if so
    """
    return all(map(eq, path, stack))


class RelativePathDict(dict):
    """
    like a dict but given a path will return the first matching relative
    path entry i.e. {('a', 'b'): 5}[('a', 'b', 'c')] == 5
    """

    def __init__(self, in_dict: Dict[Tuple[cst.CSTNode], Any]):
        self.update(in_dict)

    def __contains__(self, key: Tuple[cst.CSTNode]):
        # may want to think of a way to get actual dict search performance instead of linear...
        return any(partialPathMatch(path, key) for path in self.keys())

    def __getitem__(self, key: Tuple[cst.CSTNode]):
        try:
            return first(path for path in self.keys() if partialPathMatch(path, key))
        except StopIteration:
            raise KeyError(f"'{key}' not in dictionary")

    def get(self, key: Tuple[cst.CSTNode], default):
        try:
            return self[key]
        except KeyError:
            return default
