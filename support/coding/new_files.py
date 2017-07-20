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

This script creates new files for use in individual modules. The files are created in the
current working directory. The script will ask for required parameters interactively.
"""

import sys

Project = {
    'OpenSpace Core': 'OpenSpace',
    'OpenSpace Module': 'OpenSpace',
    'Ghoul': 'Ghoul'
}

License = {
    'OpenSpace': '/*****************************************************************************************\n *                                                                                       *\n * OpenSpace                                                                             *\n *                                                                                       *\n * Copyright (c) 2014-2017                                                               *\n *                                                                                       *\n * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *\n * software and associated documentation files (the "Software"), to deal in the Software *\n * without restriction, including without limitation the rights to use, copy, modify,    *\n * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *\n * permit persons to whom the Software is furnished to do so, subject to the following   *\n * conditions:                                                                           *\n *                                                                                       *\n * The above copyright notice and this permission notice shall be included in all copies *\n * or substantial portions of the Software.                                              *\n *                                                                                       *\n * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *\n * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *\n * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *\n * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *\n * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *\n * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *\n ****************************************************************************************/',
    'Ghoul': '/*****************************************************************************************\n *                                                                                       *\n * GHOUL                                                                                 *\n * General Helpful Open Utility Library                                                  *\n *                                                                                       *\n * Copyright (c) 2012-2017                                                               *\n *                                                                                       *\n * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *\n * software and associated documentation files (the "Software"), to deal in the Software *\n * without restriction, including without limitation the rights to use, copy, modify,    *\n * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *\n * permit persons to whom the Software is furnished to do so, subject to the following   *\n * conditions:                                                                           *\n *                                                                                       *\n * The above copyright notice and this permission notice shall be included in all copies *\n * or substantial portions of the Software.                                              *\n *                                                                                       *\n * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *\n * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *\n * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *\n * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *\n * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *\n * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *\n ****************************************************************************************/'
}

Namespace = {
    'OpenSpace': 'openspace',
    'Ghoul': 'ghoul'
}


def buildIncludeGuard(information):
    Include = {
        'OpenSpace Core': 'OPENSPACE_CORE',
        'OpenSpace Module': 'OPENSPACE_MODULE',
        'Ghoul': 'GHOUL'
    }

    if information.module:
        # If there is a module name
        m = information.module.upper()
        beg = Include[information.project] + '_' + m
        end = information.className.upper()
        return '__' + beg + '___' + end + '___H__'
    else:
        # If there is no module name
        beg = Include[information.project]
        end = information.className.upper()
        return '__' + beg + '___' + end + '___H__'

class FileInformation:
    def __init__(self):
        # Input variables
        self.project = ''
        self.module = ''
        self.className = ''

    # Derived variables
    def deriveValues(self):
        self.license = License[Project[self.project]]
        self.namespace = Namespace[Project[self.project]]
        self.filename = self.className.lower()

        self.includeGuard = buildIncludeGuard(self)

    def writeFiles(self):
        # Header file
        header = open(self.filename + '.h', 'w')
        header.write(self.license + '\n')
        header.write('\n')
        header.write('#ifndef ' + self.includeGuard + '\n')
        header.write('#define ' + self.includeGuard + '\n')
        header.write('\n')
        header.write('namespace ' + self.namespace + ' {\n')
        header.write('\n\n\n')
        header.write('} // namespace ' + self.namespace + '\n')
        header.write('\n')
        header.write('#endif // ' + self.includeGuard)
        header.write('\n')
        header.close()

        source = open(self.filename + '.cpp', 'w')
        source.write(self.license + '\n')
        source.write('\n')
        source.write('#include "' + self.filename + '.h' + '"\n')
        source.write('\n')
        source.write('namespace ' + self.namespace + ' {\n')
        source.write('\n\n\n')
        source.write('} // namespace ' + self.namespace)
        source.write('\n')
        source.close()


i = FileInformation()

p = int(input('Project (1: OpenSpace Core; 2: OpenSpace Module; 3: Ghoul): '))
if p == 1:
    i.project = "OpenSpace Core"
elif p == 2:
    i.project = "OpenSpace Module"
    i.module = input('Module name: ')
elif p == 3:
    i.project = "Ghoul"

i.className = input('Class name: ')

i.deriveValues()
i.writeFiles()