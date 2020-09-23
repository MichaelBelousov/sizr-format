"""
AST transformation engine prototype for Sizr transform language
"""

from inspect import isclass
import ast
import libcst as cst
from typing import Optional, List, Set, Iterator, Tuple, Sequence
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
                body=(
                    inner if inner is not None
                    else cst.SimpleStatementLine(body=(cst.Pass(),)),
                )
            ),
            bases=(),
            keywords=(),
            decorators=()
        )
    if 'func' in cur_scope.properties and cur_scope.properties['func']:
        # TODO: need properties to be a dictionary subclass that returns false for unknown keys
        return cst.FunctionDef(
            name=cst.Name(name),
            params=cst.Parameters(),
            body=cst.IndentedBlock(  # NOTE: wrapping in indented block may not be a good idea
                # because nesting ops like `;` may return a block in the future spec
                body=(
                    inner if inner is not None
                    else cst.SimpleStatementLine(body=(cst.Pass(),)),
                )
            ),
            decorators=(),
            asynchronous=cur_scope.properties.get(
                'async') and cst.Asynchronous(),
            # returns=None
        )
    elif cur_scope.properties.get('var') != False:
        # TODO: need properties to be a dictionary that returns false for unknown keys
        return cst.Assign(
            targets=(cst.AssignTarget(target=cst.Name(name)),),
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

    changes = {}
    for attr, a_val, b_val in ((s, getattr(a, s), getattr(b, s)) for s in a.__slots__):
        print(f"merging '{attr}'", type(a_val), type(b_val))
        if isinstance(a_val, cst.CSTNode) and isinstance(b_val, cst.CSTNode):
            changes[attr] = mergeAsts(a_val, b_val)
        if None in (a_val, b_val):
            print('one is none:', a_val, b_val)
            if a_val is not b_val:
                changes[attr] = a_val or b_val
        if isinstance(a_val, (tuple, list)):  # is Sequence and not str?
            sequence_type = type(a_val)
            assert isinstance(b_val, (tuple, list))
            # using dict over set for guaranteed order in python>=3.7
            a_index = {n: None for n in a_val}
            # probably ought to use collections.OrderedDict and switch to 2to3 in general for backporting to python2
            # maybe that I can do with the beta product :P
            b_index = {n: None for n in b_val}
            merged_dict = {**a_index, **b_index}
            for n in a_index.keys() & b_index.keys():  # XXX: this shouldn't work because of reference equality...?
                merged_dict[n] = mergeAsts(a_index[n], b_index[n])
            merged_list = sequence_type(merged_dict.values())
            changes[attr] = merged_list

    if changes:
        print('changes:', changes)
        return a.with_changes(**changes)
    else:
        return b


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

        def _leave(self, prev: cst.CSTNode, next: cst.CSTNode) -> cst.CSTNode:
            # TODO: if unanchored assertion create a module tree from scratch
            # NOTE: in the future can cache lib cst node comparisons for speed
            target = find(lambda m: prev.deep_equals(
                m.captures[-1].node), matches)
            if target is not None:
                # XXX: need some TDD on the merge routine
                transformed_node = astNodeFromAssertion(assertion, target)
                merged_node = mergeAsts(next, transformed_node)
                print(merged_node, 'prev:', prev, 'next:', next)
                return merged_node
            else:
                return next

    # TODO: make this into a decorator for cst visitors
    node_names = [c.__name__ for c in cst.__dict__.values(
    ) if isclass(c) and issubclass(c, cst.CSTNode)]
    for node_name in node_names:
        setattr(Transformer, f'leave_{node_name}', Transformer._leave)

    transformed_tree = py_ast.visit(Transformer())
    print(transformed_tree)

    return transformed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: Transform) -> str:
    py_ast = cst.parse_module(src)
    selection = None
    if transform.selector:
        selection = select(py_ast, transform.selector)
    if transform.destructive:
        py_ast = destroy_selection(py_ast, selection)
    if transform.assertion:
        py_ast = assert_(py_ast, transform.assertion, selection)
    result = py_ast.code
    import difflib
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
