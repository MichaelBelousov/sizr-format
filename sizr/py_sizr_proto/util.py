"""
general utilities
"""

from typing import Callable, Iterator, Iterable


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
