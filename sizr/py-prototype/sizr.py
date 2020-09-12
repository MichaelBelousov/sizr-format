#!/usr/bin/python3
"""
Sizr REPL
"""

from pprint import pprint
import astor

from parser import parseTransform
from engine import select


def main():
    try:
        while True:
            command = input('sizr> ')
            parsed = parseTransform(command)
            print('Parsed:', parsed)
            sample_src = open('samples/test1.py', 'r').read()
            selection = select(sample_src, parsed.selector)
            print('\nSelected:')
            for s in selection:
                print(astor.to_source(s))
    except KeyboardInterrupt:
        print()
    except EOFError:
        print()


if __name__ == '__main__':
    main()
