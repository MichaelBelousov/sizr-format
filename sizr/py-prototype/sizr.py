#!/usr/bin/python3
"""
Sizr REPL
"""

from parser import parseTransform
from engine import select
from pprint import pprint


def main():
    try:
        while True:
            command = input('sizr> ')
            parsed = parseTransform(command)
            print('Parsed:', parsed)
            sample_src = open('samples/test1.py', 'r').read()
            selection = select(sample_src, parsed.selector)
            print('Selected:')
            pprint(selection)
    except KeyboardInterrupt:
        print()
    except EOFError:
        print()


if __name__ == '__main__':
    main()
