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


This script traverses the file tree of OpenSpace and will check all files' include
guards for correctness. At the moment this includes:
 * Correctness (file has a #ifndef. #define, and #endif lines)
 * Equality (using the same name for the #ifdef and #define)
 * Styling (no empty line between #ifndef and #define lines, empty lines before and 
   after #ifndef #define block)
 * Correct usage of the name in the final comment of the file
 * Naming convention (OpenSpace include guards start with OPENSPACE, Ghoul with GHOUL,
   module includes have the module name in it)
 * Checking for duplicates between all files

If this script is executed from the base directory of OpenSpace, no arguments need to
be passed, otherwise the first and only argument has to point to the base directory.
Thus, the default value of the first argument is '.'
"""

import glob
import re
import sys

def get_ifndef_symbol(lines):
    index = [i for i,s in enumerate(lines) if '#ifndef ' in s]

    if len(index) == 0:
        return '', -1

    result = re.search('#ifndef (.*)\n', lines[index[0]])
    return result.group(1), index[0]

def get_define_symbol(lines):
    index = [i for i,s in enumerate(lines) if '#define ' in s]

    if len(index) == 0:
        return '', -1

    result = re.search('#define (.*)\n', lines[index[0]])
    return result.group(1), index[0]

def check_correctness(lines):
    ifndef_symbol, line_number = get_ifndef_symbol(lines)
    if line_number == -1:
        return 'No #ifndef in file'

    define_symbol, line_number = get_define_symbol(lines)
    if (line_number == -1):
        return 'No #define in file'

    index = [i for i,s in enumerate(lines) if '#endif' in s]
    if len(index) == 0:
        return 'No #endif in file'

    return ''

def check_equality(lines):
    ifndef, _ = get_ifndef_symbol(lines)
    define, _ = get_define_symbol(lines)

    if ifndef == define:
        return ''
    else:
        return ifndef + ' ' + define

def check_styling(lines):
    _, ifndef_line = get_ifndef_symbol(lines)
    _, define_line = get_define_symbol(lines)

    if abs(ifndef_line - define_line) != 1:
        return '#ifndef and #define lines are not subsequent'

    if lines[ifndef_line - 1].strip() != '':
        return 'Preceding line is not empty'

    if lines[define_line + 1].strip() != '':
        return 'Following line is not empty'

    return ''

def check_comment(lines):
    ifndef_symbol, _ = get_ifndef_symbol(lines)

    index = [i for i,s in enumerate(lines) if '#endif' in s]
    endif_line = lines[index[-1]].strip()

    if endif_line != '#endif // ' + ifndef_symbol:
        return '#endif line is not correctly formatted'
    else:
        return ''

def check_duplicates(lines, previousSymbols):
    ifndef_symbol, _ = get_ifndef_symbol(lines)

    if ifndef_symbol in previousSymbols:
        return False, ifndef_symbol
    else:
        return True, ifndef_symbol

basePath = './'
if len(sys.argv) > 1:
    basePath = sys.argv[1] + '/'

positivePathList = [
    'include/**/*.h',
    'apps/**/*.h',
    'modules/**/*.h',
    'ext/ghoul/include/**/*.h'
]

negativePathList = [
    'modules/**/ext/**/*.h',
    'apps/**/ext/**/*.h',
]

# Collect all the files that we might apply this script to
files = []
for p in positivePathList:
    f = glob.glob(basePath + p, recursive=True)
    files = files + f

# Collect all files that we want to remove from the full list
# These are mostly the external directories from modules
negativeFiles = []
for p in negativePathList:
    f = glob.glob(basePath + p, recursive=True)
    negativeFiles = negativeFiles + f

# Actually remove the negative files from the positive ones
files = [f for f in files if f not in negativeFiles]

previousSymbols  = {}

for file in files:
    success = True
    with open(file, 'r+') as f:
        lines = f.readlines()

        correctness = check_correctness(lines)
        if correctness:
            print(file, '\t', 'Correctness check failed', '\t', correctness)
            continue

        equality = check_equality(lines)
        if equality:
            print(file, '\t', 'Equality check failed', '\t', equality)
            continue

        styling = check_styling(lines)
        if styling:
            print(file, '\t',  'Styling check failed', '\t', styling)
            continue

        comment = check_comment(lines)
        if comment:
            print(file, '\t',  'Comment check failed', '\t', comment)
            continue

        duplicates, symbol = check_duplicates(lines, previousSymbols)
        if not duplicates:
            print(file, '\t',  'Duplicate include guard', symbol, 'first in', previousSymbols[symbol])
            continue
        else:
            previousSymbols[symbol] = file
