"""
cst utilities
"""

import libcst as cst
from typing import Callable
from functools import wraps
from inspect import isclass

node_names = [c.__name__ for c in cst.__dict__.values()
              if isclass(c) and issubclass(c, cst.CSTNode)]


def unified_visit(wrapped: Callable) -> Callable:
    """wrap a cst.CSTVisitor with a unified/generic visit/leave interface"""
    for node_name in node_names:
        if hasattr(wrapped, '_visit'):
            setattr(wrapped, f'visit_{node_name}', wrapped._visit)
        if hasattr(wrapped, '_leave'):
            setattr(wrapped, f'leave_{node_name}', wrapped._leave)
    return wrapped
