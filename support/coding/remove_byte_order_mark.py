"""
OpenSpace

Copyright (c) 2014-2017

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be included in all copies
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


This script removes a UTF-8 byte-mark order character 'ï' from the passed file
"""

import sys

if len(sys.argv) == 1:
    print('Usage: remove_byte_order_mark.py <file>')
else:
    remove_bom = False
    file = sys.argv[1]
    l = []
    with open(file, 'r+') as f:
        lines = f.readlines()
        c = lines[0][0]
        if c == 'ï':
            remove_bom = True
            l = lines
        else:
            print('No UTF-8 BOM present in this file')

    if remove_bom:
        with open(file, 'w') as f:
            print('Removing UTF-8 BOM from file')
            # We the representation of the BOM is three characters long
            lines[0] = lines[0][3:]
            f.write(''.join(lines))
