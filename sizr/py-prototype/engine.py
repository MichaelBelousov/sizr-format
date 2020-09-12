"""
AST transformation engine prototype for Sizr transform language
"""

import ast
import astor
from code import Query, Transform, capture_any

node_table = {
    ast.ClassDef: {
        'func': lambda n: True,
        'class': lambda n: True,
        'name': lambda node: node.name,
    },
    ast.FunctionDef: {
        'func': True,
    },
}

property_testers = {
    'func': lambda val, node: isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)),
    'class': lambda val, node: isinstance(node, ast.ClassDef),
}

nesting_op_filters = {
    '.': lambda node: ast.iter_child_nodes(node) if isinstance(node, ast.ClassDef) else (),
    '(': lambda node: ast.iter_child_nodes(node.args) if isinstance(node, ast.FunctionDef, ast.AsyncFunctionDef) else (),
    None: lambda node: ast.iter_child_nodes(node),
}


def select(py_src: str, selector: Query):
    selected = []
    root = ast.parse(py_src)

    # NOTE: I have no idea how mypy works yet, I'm just pretending it's typescript
    def search(node: ast.AST, scopes, nesting_op: str or None = None):
        cur_scope, *rest_scopes = scopes
        for node in nesting_op_filters[nesting_op](node):
            # FIXME: autopep8 is making this really ugly...
            if ((cur_scope.capture == capture_any
                 or ('name' in node.__dict__
                     and cur_scope.capture.pattern.match(node.name) is not None))
                    # TODO: abstract to literate function "matchesScopeProps"?
                    and all(map(lambda p: property_testers[p.key](p.val, node), cur_scope.properties))):
                if rest_scopes:
                    search(node, rest_scopes, cur_scope.nesting_op)
                else:
                    selected.append(node)

    search(root, selector.nested_scopes)
    return selected  # maybe should have search return the result list


def assert_(py_src: str, assertion: Query):
    py_ast = ast.parse(py_src)
    astor.to_source(py_ast)

# should be just select and assert but python makes it easy to do just Transform


def transform():
    class Transformer(ast.NodeTransformer):
        Transformer.__dict__[f'visit_{ast.FunctionDef.__name__}']
    Transformer.__dict__['']
    # transformed_tree = Transformer.visit(tree)
    # fixed_tree = ast.fix_missing_locations(transformed_tree)
