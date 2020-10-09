#!/usr/bin/python3
"""
test sizr stuff
"""

# TODO: figure out python unittest project organization conventions

import unittest
import libcst as cst
import subprocess


def sh(*args, **kwargs):
    return subprocess.check_output(*args, **kwargs, shell=True)


def dedent_on_nextline(text: str):
    """textwrap.dedent doesn't seem to be working for my purposes"""
    lines = text.splitlines()
    first_indented_line = lines[1]
    finish_indent = 0
    for c in first_indented_line:
        if not c.isspace():
            break
        finish_indent += 1
    return '\n'.join(l[finish_indent:] for l in lines)


class CliTests(unittest.TestCase):
    def test_add_method(self):
        received = sh("""\
            echo $'C . f >>> class C . func g\n' \
            | python3 -m sizr.py_sizr_proto sizr/samples/small.py\
        """)
        expected = bytes(dedent_on_nextline("""\
        sizr> --- 
        +++ 
        @@ -4,6 +4,8 @@
         
             def G(self):
                 return self.f() + C.x
        +    def g(self):
        +        pass
         
         
         y = C.f

        sizr> 
        """), encoding='utf8')
        self.assertEqual(received, expected)

    def test_add_uncaptured_global_func(self):
        received = sh("""\
            echo $'>>> func x\n' \
            | python3 -m sizr.py_sizr_proto sizr/samples/small.py\
        """)
        expected = bytes(dedent_on_nextline("""\
        sizr> --- 
        +++ 
        @@ -4,6 +4,8 @@
         
             def G(self):
                 return self.f() + C.x
        +    def g(self):
        +        pass
         
         
         y = C.f

        sizr> 
        """), encoding='utf8')
        self.assertEqual(received, expected)

    def test_move_captured_func(self):
        received = sh("""\
            echo $'C . f >>! C . class A . f \n' \
            | python3 -m sizr.py_sizr_proto sizr/samples/small.py\
        """)
        expected = bytes(dedent_on_nextline("""\
        sizr> --- 
        +++ 
        @@ -1,9 +1,10 @@
         class C:
             x = 2
        -    def f(self): return 5
         
             def G(self):
                 return self.f() + C.x
        +    class A:
        +        def f(self): return 5
         
         
         y = C.f
        
        sizr> 
        """), encoding='utf8')
        print('recieved:', repr(received))
        print('expected:', repr(expected))
        self.assertEqual(received, expected)

    def test_destroy_func(self):
        received = sh("""\
            echo $'C . f >>!\n' \
            | python3 -m sizr.py_sizr_proto sizr/samples/small.py\
        """)
        expected = bytes(dedent_on_nextline("""\
        sizr> --- 
        +++ 
        @@ -4,6 +4,8 @@
         
             def G(self):
                 return self.f() + C.x
        +    def g(self):
        +        pass
         
         
        y = C.f

        sizr> 
        """), encoding='utf8')
        self.assertEqual(received, expected)


if __name__ == '__main__':
    unittest.main()
