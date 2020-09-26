#!/usr/bin/python3
"""
Sizr REPL
"""

from pprint import pprint
import ast
import astor
import argparse
import readline  # implicit gnu command line editing

from .parser import parseTransform
from .engine import exec_transform


def repl(input_file):
    input_src = input_file.read()
    try:
        while True:
            command = input('sizr> ')
            if not command:
                break
            parsed = parseTransform(command)
            exec_transform(input_src, parsed)
    except KeyboardInterrupt:
        pass
    print()


def main():
    argparser = argparse.ArgumentParser('sizr')
    argparser.add_argument('INPUT_FILE', help='source file to read')
    args = argparser.parse_args()
    input_file = open(args.INPUT_FILE, 'r')
    repl(input_file)


if __name__ == '__main__':
    main()
