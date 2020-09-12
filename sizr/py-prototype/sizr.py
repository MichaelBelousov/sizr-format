#!/usr/bin/python3
"""
Sizr REPL
"""

from parser import parseTransform

def main():
    try:
        while True:
            command = input('sizr> ')
            parsed = parseTransform(command)
            print(parsed)
    except KeyboardInterrupt:
        print()

if __name__ == '__main__': main()
