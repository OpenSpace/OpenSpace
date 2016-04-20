"""
OpenSpace

Copyright (c) 2014

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


This script traverses the file tree of OpenSpace and converts all tabs into spaces.
If this script is executed from the base directory of OpenSpace, no arguments need to
be passed, otherwise the first and only argument has to point to the base directory.
Thus, the default value of the first argument is '.'
"""

import glob
import sys

basePath = './'
if len(sys.argv) > 1:
    basePath = sys.argv[1] + '/'


positivePathList = [
    'src/**/*.cpp',
    'include/**/*.h',
    'include/**/*.inl',
    'apps/**/*.cpp',
    'apps/**/*.h',
    'apps/**/*.inl',
    'modules/**/*.cpp',
    'modules/**/*.h',
    'modules/**/*.inl',
    'modules/**/*.glsl',
    'ext/ghoul/src/**/*.cpp',
    'ext/ghoul/include/**/*.h',
    'ext/ghoul/include/**/*.inl',
    'shaders/**/*.glsl',
    'shaders/**/*.hglsl',
    'shaders/**/*.vert',
    'shaders/**/*.gs',
    'shaders/**/*.frag'
]

negativePathList = [
    'modules/**/ext/**',
    'apps/**/ext/**'
]

def convertFile(file):
    result = False
    with open(file, 'r+') as f:
        lines = f.readlines()
        for l in lines:
            if l.find('\t') != -1:
                result = True
                break

        if result:
            lines[:] = [l.replace('\t', '    ') for l in lines]
            # print(lines)

        f.seek(0)
        f.writelines(lines)
        f.truncate()

    return result

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

totalNumber = 0
for f in files:
    hasChanged = convertFile(f)
    if hasChanged:
        print("Changed tabs to spaces in " + f)
        totalNumber = totalNumber + 1

print("Total number of changed files: " + str(totalNumber))
sys.exit(totalNumber)