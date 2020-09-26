"""
AST transformation engine prototype for Sizr transform language
"""

from operator import and_
import ast
import libcst as cst
from typing import Optional, List, Set, Iterator, Tuple, Sequence
from functools import reduce
from .code import Query, Transform, ScopeExpr, capture_any
from .cst_util import unified_visit
from .util import tryFind, notFound, dictKeysAndValues, first
import operator
import difflib


# TODO: better name
node_table = {
    cst.ClassDef: {
        'func': lambda n: True,
        'class': lambda n: True,
        'name': lambda node: node.name,
    },
    cst.FunctionDef: {
        'func': True,
    },
}

property_testers = {
    'func': lambda val, node: isinstance(node, cst.FunctionDef),
    'class': lambda val, node: isinstance(node, cst.ClassDef),
}


nesting_op_children_getter = {
    '.': lambda node: node.body.body if isinstance(node, cst.ClassDef) else (),
    '(': lambda node: node.params if isinstance(node, cst.FunctionDef) else (),
    None: lambda node: node.children
}

possible_node_classes_per_prop = {
    'type': lambda val: {},  # TODO: make a special "any" set
    'func': lambda val: {cst.FunctionDef},
    'class': lambda val: {cst.ClassDef},
    'var': lambda val: {cst.Name, cst.Assign}
}


def possibleElemTypesFromScopeExpr(scope: ScopeExpr) -> Set[type(cst.CSTNode)]:
    return set(
        reduce(and_,
               map(lambda key, val: possible_node_classes_per_prop[key](val),
                   *dictKeysAndValues(scope.properties))))


def elemNameFromNode(node: cst.CSTNode) -> Set[str]:
    node_class = node.__class__
    per_node_class_name_accessors = {
        cst.FunctionDef: lambda: {node.name.value},
        cst.ClassDef: lambda: {node.name.value},
        cst.Assign: lambda: {t.target.value for t in node.targets},
    }
    return per_node_class_name_accessors.get(node_class, lambda: set())()

# future code organization
# default_prop_values = {}
# node_from_prop_builders = {cst.ClassDef: lambda props: cst.ClassDef }


class Capture:
    def __init__(self, node: cst.CSTNode, name: Optional[str] = None):
        self.node = node
        self.name = name
    # FIXME: fix CaptureExpr vs Capture[Ref?] naming
    # maybe I can leave this to be fixed by the alpha phase :)
    __repr__ = __str__ = lambda s: f'<CaptureRef|name={s.name},node={s.node}>'


class SelectionMatch:
    CaptureType = Capture

    def __init__(self, captures: List[CaptureType]):
        self.captures = captures

    # probably want capture list to be retrievable by name,
    # perhaps abstracting over int-indexable map/dict is in order
    def getCaptureByName(self, name: str) -> CaptureType:
        first, *_ = filter(lambda c: c.name == name, self.captures)
        return first

    __repr__ = __str__ = lambda s: f'<Match|{s.captures}>'


# TODO: proof of the need to clarify datum names, as `Element` or `ProgramElement` or `Unit` or `Name`
def astNodeFromAssertion(assertion: Query,
                         match: SelectionMatch,
                         destructive: bool) -> Sequence[cst.CSTNode]:
    # TODO: aggregate intersect possible_nodes_per_prop and and raise on multiple
    # results (some kind of "ambiguity error"). Also need to match with anchor placement
    cur_scope, *next_scopes = assertion.nested_scopes
    cur_capture, *next_captures = match.captures
    name = cur_scope.capture.literal or cur_capture.node.name
    next_assertion = Query()
    next_assertion.nested_scopes = next_scopes

    body = cur_capture.node
    while not isinstance(body, Sequence):
        body = body.body if hasattr(body, 'body') else []
    if next_captures:
        inner = astNodeFromAssertion(
            next_assertion, SelectionMatch(next_captures), destructive)
        if destructive:
            body = [s for s in body if not s.deep_equals(
                next_captures[-1].node)]
        body = (*body, *inner)
    if not body:
        body = [cst.SimpleStatementLine(body=[cst.Pass()])]

    BodyType = cst.IndentedBlock
    try:
        BodyType = type(cur_capture.node.body)
    except AttributeError:
        pass

    if cur_scope.properties.get('class'):
        return [cur_capture.node.with_changes(
            name=cst.Name(name),
            body=BodyType(
                body=body
            ),
        )]
    if cur_scope.properties.get('func'):
        # TODO: need properties to be a dictionary subclass that returns false for unknown keys
        node_props = {
            'params': cst.Parameters(),
            'decorators': (),
        }
        if cur_scope.properties.get('async'):
            node_props['asynchronous'] = cst.Asynchronous()
        # TODO: need to check if this is defined in a class, defaults to having a self argument
        return [cur_capture.node.with_changes(
            name=cst.Name(name),
            body=BodyType(  # NOTE: wrapping in indented block may not be a good idea
                # because nesting ops like `;` may return a block in the future spec
                body=body
            ),
            **node_props
        )]
    elif cur_scope.properties.get('var'):
        # TODO: need properties to be a dictionary that returns false for unknown keys
        return [cur_capture.node.with_changes(
            targets=[cst.AssignTarget(target=cst.Name(name))],
            value=cst.Name("None")
        )]
    raise Exception("Could not determine a node type from the name properties")


def select(root: cst.CSTNode, selector: Query) -> List[SelectionMatch]:
    selected: List[SelectionMatch] = []

    # TODO: dont root search at global scope, that's not the original design
    # I'll probably need to change the parser to store the prefixing nesting op
    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    # NOTE: I saw other typing usage briefly and I'm pretty sure it doesn't work this way
    def search(node: cst.CSTNode, scopes, nesting_op: Optional[str] = None, captures: Optional[List[cst.CSTNode]] = None):
        if captures is None:
            captures = []
        cur_scope, *rest_scopes = scopes
        for node in nesting_op_children_getter[nesting_op](node):
            # FIXME: autopep8 is making this really ugly... (or maybe I am)
            if ((cur_scope.capture == capture_any
                 # TODO: switch to elemNameFromNode
                 or (hasattr(node, 'name')  # TODO: prefer isinstance()
                     and cur_scope.capture.pattern.match(node.name.value) is not None))
                    # TODO: abstract to literate function "matchesScopeProps"?
                    and all(map(lambda k, v: property_testers[k](v, node),
                                *dictKeysAndValues(cur_scope.properties)))):
                next_captures = [*captures,
                                 Capture(node, cur_scope.capture.name)]
                if rest_scopes:
                    search(node, rest_scopes,
                           cur_scope.nesting_op, next_captures)
                else:
                    selected.append(SelectionMatch(next_captures))

    search(root, selector.nested_scopes)
    return selected  # maybe should have search return the result list


def assert_(py_ast: cst.CSTNode,
            assertion: Query,
            matches: Optional[Iterator[SelectionMatch]],
            destructive: bool) -> cst.CSTNode:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen. Maybe *ensure*?
    """
    if matches is None:
        matches = set()

    @ unified_visit
    class Transformer(cst.CSTTransformer):

        def _leave(self, original: cst.CSTNode, updated: cst.CSTNode) -> cst.CSTNode:
            # TODO: if global scope query create a module tree from scratch?
            # NOTE: in the future can cache lib cst node comparisons for speed
            match = tryFind(lambda m: original.deep_equals(
                m.captures[0].node), matches)
            if match is notFound:
                return updated
            else:
                from_assert = first(astNodeFromAssertion(
                    assertion, match, destructive))
                return from_assert

    transformed_tree = py_ast.visit(Transformer())

    return transformed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: Transform) -> str:
    py_ast = cst.parse_module(src)
    selection = None
    if transform.selector:
        selection = select(py_ast, transform.selector)
    if transform.assertion:
        py_ast = assert_(py_ast, transform.assertion,
                         selection, transform.destructive)
    result = py_ast.code
    diff = ''.join(
        difflib.unified_diff(
            src.splitlines(1),
            result.splitlines(1)
        )
    )
    if diff:
        print(diff)
    else:
        print('no changes!')
    return result
