#!/usr/bin/python3
"""
Sizr REPL
"""

from pprint import pprint
import astor
import argparse

from parser import parseTransform
from engine import select, assert_


def repl(input_file):
    try:
        while True:
            command = input('sizr> ')
            parsed = parseTransform(command)
            print('Parsed:', parsed)
            input_src = input_file.read()
            selection = select(input_src, parsed.selector)
            print('Selected:')
            for s in selection:
                for c in s.captures:
                    print("#########################################")
                    print(astor.to_source(c.node))
            transformed = assert_(input_src, parsed.assertion, selection)
            print('Transformed:')
            print("#########################################")
            print(transformed)
    except KeyboardInterrupt:
        print()
    except EOFError:
        print()


def main():
    argparser = argparse.ArgumentParser('sizr')
    argparser.add_argument('INPUT_FILE', help='source file to read')
    args = argparser.parse_args()
    input_file = open(args.INPUT_FILE, 'r')
    repl(input_file)


if __name__ == '__main__':
    main()
