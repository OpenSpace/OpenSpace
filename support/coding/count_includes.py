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

This file takes the output of compiling in Visual Studio with /showIncludes enabled and
counts the number of times that each header file is included (excluding system headers).
This gives an indicator as to which header files can be made more efficient the most
efficiently

The script requires one argument, which is the text file of one or many runs of the
Visual Studio compiler
"""

import sys

if len(sys.argv) != 2:
    print("Usage: count_includes.py <text file containing /showIncludes results")
    exit(0)

file = sys.argv[1]

with open(file) as f:
    lines = f.readlines()

    # Only get the lines that actually have includes
    lines = [l for l in lines if "Note: including file" in l]

    # Prefix to remove from each line an
    prefix = "X>  Note: including file:"
    lines = [l[len(prefix):-1].strip() for l in lines]

    # Remove system header
    lines = [l for l in lines if "windows kits" not in l.lower()]
    lines = [l for l in lines if "microsoft visual studio " not in l.lower()]

    # Remove external headers
    lines = [l for l in lines if "ghoul\\ext\\" not in l.lower()]

    count = {}
    for l in lines:
        if l in count:
            count[l] = count[l] + 1
        else:
            count[l] = 1

    for key, value in sorted(count.iteritems(), key=lambda (k,v): (v,k)):
        print("%s: %s" % (key.ljust(100), value))
