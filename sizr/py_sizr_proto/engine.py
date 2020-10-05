"""
AST transformation engine prototype for Sizr transform language
"""

from operator import and_
import libcst as cst
from typing import Optional, List, Set, Iterable, Tuple, Sequence
from functools import reduce
from .code import Query, TransformExpr, TransformContext, ScopeExpr, pattern_any, CapturedElement, CaptureExpr, Match
from .cst_util import unified_visit
from .util import tryFind, notFound, first, only, tryFirst
import operator
import difflib
from os import environ as env


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
                   scope.properties.keys(),
                   scope.properties.values())))


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


def getNodeBody(node: cst.CSTNode):
    if not hasattr(node, 'body'):
        return None, None

    body = node.body
    BodyType = None
    while not isinstance(body, Sequence):
        BodyType = type(body)
        body = body.body if hasattr(body, 'body') else []

    return body, BodyType


def astNodeFromAssertion(transform: TransformContext,
                         match: Match,
                         index=0) -> Sequence[cst.CSTNode]:
    # TODO: aggregate intersect possible_nodes_per_prop and and raise on multiple
    # results (some kind of "ambiguity error"). Also need to match with anchor placement
    cur_scope_expr = transform.assertion.nested_scopes[index]
    cur_capture = match.by_name(cur_scope_expr.capture.name)
    if env.get('SIZR_DEBUG'):
        print('astNodeFromAssertion', cur_scope_expr, cur_capture)

    name = cur_scope_expr.capture.name
    body = ()
    node = BodyType = None

    if cur_capture is not None:
        node = cur_capture.node
        name = only(elemNameFromNode(node))
        body, BodyType = getNodeBody(node)

    if index < len(transform.assertion.nested_scopes) - 1:
        inner = astNodeFromAssertion(transform, match, index+1)
        if transform.destructive:
            body = [s for s in body if not s.deep_equals(
                match.elem_path[-1].node)]
        body = (*body, *inner)

    if not body:
        body = [cst.SimpleStatementLine(body=[cst.Pass()])]

    body = body or ()
    BodyType = BodyType or cst.IndentedBlock

    if cur_capture is not None:
        return [node.with_changes(
            name=cst.Name(name),
            body=BodyType(
                body=body
            ),
        )]
    if cur_scope_expr.properties.get('class'):
        return [(node.with_changes if node else cst.ClassDef)(
            name=cst.Name(name),
            body=BodyType(
                body=body
            ),
        )]
    if cur_scope_expr.properties.get('func'):
        # TODO: need properties to be a dictionary subclass that returns false for unknown keys
        node_props = {
            'params': cst.Parameters(),
            'decorators': (),
        }
        if cur_scope_expr.properties.get('async'):
            node_props['asynchronous'] = cst.Asynchronous()
        # TODO: need a rigorous way to determine defaults for a node type given context
        # e.g. python functions in classes contain a first argument, 'self' by default
        # e.g. C++ 'interface' scope property is a context where all methods are by default pure virtual
        # TODO: need to check if this is defined in a class, defaults to having a self argument
        return [(node.with_changes if node else cst.FunctionDef)(
            name=cst.Name(name),
            body=BodyType(  # NOTE: wrapping in indented block may not be a good idea
                # because nesting ops like `;` may return a block in the future spec
                body=body
            ),
            **node_props
        )]
    elif cur_scope_expr.properties.get('var'):
        # TODO: need properties to be a dictionary that returns false for unknown keys
        return [(node.with_changes if node else cst.Assign)(
            targets=[cst.AssignTarget(target=cst.Name(name))],
            value=cst.Name("None")
        )]
    raise Exception("Could not determine a node type from the name properties")


def select(root: cst.Module, transform: TransformExpr) -> TransformContext:
    result = TransformContext(transform, root)

    # TODO: dont root search at global scope, that's not the original design
    # I'll probably need to change the parser to store the prefixing nesting op
    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    # NOTE: I saw other typing usage briefly and I'm pretty sure it doesn't work this way
    def search(node: cst.CSTNode, scopes, nesting_op: Optional[str] = None, captures: Optional[List[cst.CSTNode]] = None):
        if captures is None:
            captures = []
        # XXX: need to think out this algorithm slightly more (base cases are lacking)
        if not scopes:
            return
        cur_scope, *rest_scopes = scopes
        for node in nesting_op_children_getter[nesting_op](node):
            # FIXME: autopep8 is making this really ugly... (or maybe I am)
            if ((cur_scope.capture.pattern == pattern_any
                 # TODO: switch to elemNameFromNode
                 or (hasattr(node, 'name')  # TODO: prefer isinstance()
                     and cur_scope.capture.pattern.match(node.name.value) is not None))
                    # TODO: abstract to literate function "matchesScopeProps"?
                    and all(map(lambda k, v: property_testers[k](v, node),
                                cur_scope.properties.keys(),
                                cur_scope.properties.values()))):
                next_captures = [*captures,
                                 CapturedElement(cur_scope.capture, node)]
                if rest_scopes:
                    search(node, rest_scopes,
                           cur_scope.nesting_op, next_captures)
                else:
                    result.add_match(next_captures)

    search(root, transform.selector.nested_scopes)
    return result  # maybe should have search return the result list


def assert_(py_ast: cst.CSTNode, transformCtx: TransformContext) -> cst.CSTNode:
    """
    TODO: in programming `assert` has a context of being passive, not fixing if it finds that it's incorrect,
    perhaps a more active word should be chosen. Maybe *ensure*?
    """
    matches = transformCtx.matches

    first_ref_index = None
    find_attempt = tryFirst(transformCtx.capture_reference_indices)
    if find_attempt is not notFound:
        _, (first_ref_index, _) = first(transformCtx.capture_reference_indices)

    @unified_visit
    class Transformer(cst.CSTTransformer):
        def _leave(self, original: cst.CSTNode, updated: cst.CSTNode) -> cst.CSTNode:
            # TODO: if global scope query create a module tree from scratch?
            # NOTE: in the future can cache lib cst node comparisons for speed
            match = notFound
            if first_ref_index is not None:
                match = tryFind(lambda m: original.deep_equals(
                    m.elem_path[first_ref_index].node), matches)
            if match is not notFound:
                from_assert = first(astNodeFromAssertion(transformCtx, match))
                return from_assert
            elif original in transformCtx.references:
                # TODO: replace references to anything destroyed by the transform
                pass
            elif find_attempt is notFound and isinstance(updated, cst.Module):
                module_match = Match([CapturedElement(CaptureExpr(), updated)])
                return updated.with_changes(body=(*updated.body, *astNodeFromAssertion(transformCtx, module_match)))
            else:
                return updated

    transformed_tree = py_ast.visit(Transformer())

    return transformed_tree


# NOTE: default to print to stdout, take a cli arg for target file for now
def exec_transform(src: str, transform: TransformExpr) -> str:
    py_ast = cst.parse_module(src)
    old_ast = py_ast
    if env.get('SIZR_DEBUG'):
        print('<<<<< ORIGINAL:', py_ast)
    transform_ctx = None

    if transform.selector:
        transform_ctx = select(py_ast, transform)
    if transform.assertion:
        py_ast = assert_(py_ast, transform_ctx)

    if env.get('SIZR_DEBUG'):
        print('>>>>> TRANSFORMED:', py_ast)
        print('!!!!! DIFF', ''.join(
            difflib.unified_diff(
                str(old_ast).splitlines(1),
                str(py_ast).splitlines(1)
            )
        ))
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
