"""
cst utilities
"""

import libcst as cst
from typing import Callable
from functools import wraps
from inspect import isclass

node_names = [c.__name__ for c in cst.__dict__.values()
              if isclass(c) and issubclass(c, cst.CSTNode)]


def unified_leave(wrapped: Callable) -> Callable:
    for node_name in node_names:
        setattr(wrapped, f'leave_{node_name}', wrapped._leave)
    return wrapped
