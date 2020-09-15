"""
AST transformation engine prototype for Sizr transform language
"""

import ast
import libcst as cst
from typing import Optional, List, Set, Iterator, Tuple
from functools import reduce
from .code import Query, Transform, ScopeExpr, capture_any
import operator


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
    'var': lambda val: {cst.Name}
}


class Capture:
    def __init__(self, node: cst.CSTNode, name: Optional[str] = None):
        self.node = node
        self.name = name
    # FIXME: fix CaptureExpr vs Capture[Ref?] naming
    __repr__ = __str__ = lambda s: f'<CaptureRef|name={s.name}>'


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


# TODO: proof of the need to clarify a datum names, as `Element` or `ProgramElement` or `Unit` or `Name`
def astNodeFromAssertion(assertion: Query, match: SelectionMatch) -> cst.CSTNode:
    if not assertion.nested_scopes:
        return
    # TODO: mass intersect possible_nodes_per_prop and and raise on multiple
    # results (some kind of "ambiguous error"). Also need to match with captured/anchored
    cur_scope, *next_scopes = assertion.nested_scopes
    cur_capture, *next_captures = match.captures
    name = cur_scope.capture.literal or cur_capture.node.name
    next_assertion = Query()
    next_assertion.nested_scopes = next_scopes
    inner = astNodeFromAssertion(next_assertion, SelectionMatch(next_captures))
    if 'class' in cur_scope.properties and cur_scope.properties['class']:
        return cst.ClassDef(
            name=cst.Name(name),
            body=cst.IndentedBlock(
                body=inner if inner is not None else [
                    cst.SimpleStatementLine(body=[cst.Pass()])]
            ),
            bases=[],
            keywords=[],
            decorators=[]
        )
    if 'func' in cur_scope.properties and cur_scope.properties['func']:
        # TODO: need properties to be a dictionary that returns false for unknown keys
        return cst.FunctionDef(
            name=cst.Name(name),
            params=cst.Parameters(),
            body=cst.IndentedBlock(
                body=inner if inner is not None else [
                    cst.SimpleStatementLine(body=[cst.Pass()])]
            ),
            decorators=[],
            asynchronous=cur_scope.properties.get(
                'async') and cst.Asynchronous(),
            returns=None
        )
    elif cur_scope.properties.get('var') != False:
        # TODO: need properties to be a dictionary that returns false for unknown keys
        return cst.Assign(
            targets=(cst.AssignTarget(cst.Name(name))),
            value=cst.Name("None")
        )
    raise Exception("Could not determine a node type from the name properties")


def dictKeysAndValues(d): return d.keys(), d.values()


def select(root: cst.CSTNode, selector: Query) -> List[SelectionMatch]:
    selected: List[SelectionMatch] = []

    # TODO: dont root search at global scope, that's not the original design
    # I'll probably need to change the parser to store the prefixing nesting op
    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    # NOTE: I saw other typing usage briefly and I'm pretty sure it doesn't work this way
    def search(node: cst.CSTNode, scopes, nesting_op: str or None = None, captures: Optional[List[cst.CSTNode]] = None):
        if captures is None:
            captures = []
        cur_scope, *rest_scopes = scopes
        for node in nesting_op_children_getter[nesting_op](node):
            # FIXME: autopep8 is making this really ugly... (or maybe I am)
            if ((cur_scope.capture == capture_any
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


# maybe rename override, since it's really eclipsing/overriding
def mergeAsts(a: cst.CSTNode, b: cst.CSTNode) -> cst.CSTNode:
    """ merge b into a, mutating a """
    if type(a) != type(b):
        return b

    assert a._fields == b._fields, "to merge ast nodes must have same attrs"

    for attr, a_val, b_val in ((f, getattr(a, f), getattr(b, f)) for f in a._fields):
        if isinstance(a_val, cst.CSTNode):
            assert isinstance(b_val, cst.CSTNode)
            setattr(a, attr, mergeAsts(a_val, b_val))
        if isinstance(a_val, list):
            assert isinstance(b_val, list)
            a_index = {n.path: n for n in a_val}
            b_index = {n.path: n for n in b_val}
            merged_dict = {**a_index, **b_index}
            for node_path in a_index.keys() & b_index.keys():
                merged_dict[node_path] = mergeAsts(
                    a_index[node_path], b_index[node_path])
            merged_list = [*merged_dict.values()]
            setattr(a, attr, merged_list)

    return a


def find(func, itr: Iterator):
    """
    TODO: move to general utilities
    """
    try:
        return next(filter(func, itr))
    except:
        pass


def destroy_selection(py_ast: cst.CSTNode, matches: Iterator[SelectionMatch] = {}) -> cst.CSTNode:
    """ remove the selected nodes from the AST, for destructive queries """

    class DestroySelection(cst.CSTTransformer):
        def __getattr__(self, attr):
            if attr.startswith('leave_'):
                return self._leave
            else:
                raise AttributeError(f"no such attribute '{attr}'")

        def _leave(self, prev: cst.CSTNode, next: cst.CSTNode) -> cst.CSTNode:
            # TODO: create a module tree from scratch for the assertion and merge the trees
            if any(lambda m: prev.deep_equals(m.captures[-1].node), matches):
                # XXX: may need to fix the lack/gain of pass in bodies...
                # return cst.RemoveFromParent()
                return cst.Pass()
            return next

    post_destroy_tree = py_ast.visit(DestroySelection())
    # bodies_fixed_tree = FixEmptyBodies().visit(post_destroy_tree)
    # fixed_tree = cst.fix_missing_locations(bodies_fixed_tree)
    fixed_tree = post_destroy_tree
    return fixed_tree


def assert_(py_ast: cst.CSTNode, assertion: Query, matches: Optional[Iterator[SelectionMatch]]) -> cst.CSTNode:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen
    """
    if matches is None:
        matches = set()

    class Transformer(cst.CSTTransformer):
        def __getattr__(self, attr):
            if attr.startswith('leave_'):
                return self._leave
            else:
                raise AttributeError(f"no such attribute '{attr}'")

        def _leave(self, prev: cst.CSTNode, next: cst.CSTNode) -> cst.CSTNode:
            # TODO: create a module tree from scratch for the assertion and merge the trees at the anchor point
            target = find(lambda m: prev.deep_equals(
                m.captures[0].node), matches)
            if target is not None:
                transformed_node = astNodeFromAssertion(assertion, target)
                return mergeAsts(next, transformed_node)
            else:
                return next

        leave_FunctionDefinition = leave_ClassDef = _leave

    transformed_tree = py_ast.visit(Transformer())

    return transformed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: Transform) -> str:
    py_ast = cst.parse_module(src)
    selection = None
    if transform.selector:
        selection = select(py_ast, transform.selector)
        print(selection)
    if transform.destructive:
        py_ast = destroy_selection(py_ast, selection)
    if transform.assertion:
        py_ast = assert_(py_ast, transform.assertion, selection)
    print(py_ast)
    result = py_ast.code
    print(repr(result))
    import difflib
    print(''.join(
        difflib.unified_diff(
            src.splitlines(1),
            result.splitlines(1)
        )
    ))
    return result

    # TODO: use difflib to show a diff of the changes and confirm unless in pre-confirmed mode
