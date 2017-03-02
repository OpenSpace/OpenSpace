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
 * Styling 
   * no empty line between #ifndef and #define lines
   * Empty lines before and after #ifndef #define block
   * Files end with an empty line
   * Copyright header is correctly indented
   * Include guard correctly uses the filename
   * Include guard is all upper case
 * Correct usage of the name in the final comment of the file
 * Correct year of copyright notice
 * Naming convention
   * OpenSpace include guards start with OPENSPACE, Ghoul with GHOUL,
     module includes have the module name in it
   * The correct submodule is used
 * Checking for duplicates between all files
 * Checking that no file includes glm header directly

If this script is executed from the base directory of OpenSpace, no arguments need to
be passed, otherwise the first and only argument has to point to the base directory.
Thus, the default value of the first argument is '.'
"""

import fnmatch
import glob
import os
import re
import sys

current_year = '2017'

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
    ifndef_symbol, ifndef_line = get_ifndef_symbol(lines)
    _, define_line = get_define_symbol(lines)

    if abs(ifndef_line - define_line) != 1:
        return '#ifndef and #define lines are not subsequent'

    if lines[ifndef_line - 1].strip() != '':
        return 'Preceding line is not empty'

    if lines[define_line + 1].strip() != '':
        return 'Following line is not empty'

    if not lines[-1][-1] in ['\n', '\r']:
        return 'Last line must end with a newline'

    for l in lines[2:23]:
        if l[0] != ' ':
            return 'Copyright header must be indented'

    if ifndef_symbol != ifndef_symbol.upper():
        return 'Include guard is not all upper case'

    return ''



def check_styling_filename(lines, filename):
    ifndef_symbol, _ = get_ifndef_symbol(lines)
    file = os.path.splitext(os.path.basename(filename))[0].upper()

    if not (file in ifndef_symbol or file in ifndef_symbol.replace('_', '')):
        return 'Malformed include guard: ' + ifndef_symbol + ' || ' + file



def check_comment(lines):
    ifndef_symbol, _ = get_ifndef_symbol(lines)

    index = [i for i,s in enumerate(lines) if '#endif' in s]
    endif_line = lines[index[-1]].strip()

    if endif_line != '#endif // ' + ifndef_symbol:
        return '#endif line is not correctly formatted'
    else:
        return ''



def check_copyright(lines):
    index = [i for i,s in enumerate(lines[0:23]) if 'Copyright' in s]

    if len(index) == 0:
        return 'No copyright header found'

    beginning_string = ' * Copyright (c) 2012-'
    #  * Copyright (c) 2014-

    year = lines[index[0]][len(beginning_string) : len(beginning_string) + 4]

    if year != current_year:
        return 'Out of date copyright notice ' + year + ' || ' + current_year
    else:
        return ''



def check_naming_convention_component(lines, component):
    ifndef_symbol, _ = get_ifndef_symbol(lines)

    component_part = ifndef_symbol[2:2 + len(component)]

    if component_part != component.upper():
        return '#ifndef naming convention broken: ' + ifndef_symbol + ' || ' + component.upper() 
    else:
        return ''



def check_naming_convention_subcomponent(lines, component, file):
    ifndef_symbol, _ = get_ifndef_symbol(lines)

    if component == "ghoul" or component == "openspace_core":
        return

    subcomponent_part = ifndef_symbol[2 + len(component) + 1 :]
    subcomponent_part = subcomponent_part[: subcomponent_part.find('_')]

    path_part = file.split(os.sep)[2]

    if path_part.upper() != subcomponent_part:
        return 'Subcomponent naming convention broken: ' + ifndef_symbol
    else:
        return ''



def check_duplicates(lines, previousSymbols):
    ifndef_symbol, _ = get_ifndef_symbol(lines)

    if ifndef_symbol in previousSymbols:
        return False, ifndef_symbol
    else:
        return True, ifndef_symbol



def check_glm_header(lines, file):
    Allowed_Files = [
        'ghoul/glm.h'
    ]

    for f in Allowed_Files:
        if f in file:
            return ''

    index = [i for i,s in enumerate(lines)
                if '#include <glm/glm.hpp>' in s or 
                '#include "glm/glm.hpp>"' in s]

    if len(index) > 0:
        return 'File used wrong glm include. Use "#include <ghoul/glm.h>" instead'
    else:
        return ''

def check_core_dependency(lines, component):
    if component != "openspace_core":
        return ''

    index = [i for i,s in enumerate(lines) if 'OPENSPACE_MODULE_' in s]

    if len(index) > 0:
        return lines[index[0]][:-1]
    else:
        return ''

def check_using_namespace(lines):
    index = [i for i,s in enumerate(lines) if "using namespace" in s.strip()]

    if len(index) > 0:
        return lines[index[0]]
    else:
        return ''


previousSymbols  = {}
def check_header_file(file, component):
    with open(file, 'r+') as f:
        lines = f.readlines()

        correctness = check_correctness(lines)
        if correctness:
            print(file, '\t', 'Correctness check failed', '\t', correctness)
            return

        equality = check_equality(lines)
        if equality:
            print(file, '\t', 'Equality check failed', '\t', equality)
            return

        styling = check_styling(lines)
        if styling:
            print(file, '\t',  'Styling check failed', '\t', styling)
            return

        styling_filename = check_styling_filename(lines, file)
        if styling_filename:
            print(file, '\t',  'Filename styling check failed', '\t', styling_filename)
            return

        comment = check_comment(lines)
        if comment:
            print(file, '\t',  'Comment check failed', '\t', comment)
            return

        copyright = check_copyright(lines)
        if copyright:
            print(file, '\t', 'Copyright check failed', '\t', copyright)
            return

        naming_component = check_naming_convention_component(lines, component)
        if naming_component:
            print(file, '\t', 'Naming convention broken', '\t', naming_component)
            return

        naming_subcomponent = check_naming_convention_subcomponent(lines, component, file)
        if naming_subcomponent:
            print(file, '\t', 'Naming convention broken', '\t', naming_subcomponent)
            return

        duplicates, symbol = check_duplicates(lines, previousSymbols)
        if not duplicates:
            print(file, '\t', 'Duplicate include guard', symbol, 'first in', previousSymbols[symbol])
            return
        else:
            previousSymbols[symbol] = file

        header = check_glm_header(lines, file)
        if header:
            print(file, '\t', 'Illegal glm header include', header)
            return

        core_dependency = check_core_dependency(lines, component)
        if core_dependency:
            print(file, '\t', 'Wrong dependency (core depends on module)', core_dependency)

        using_namespaces = check_using_namespace(lines)
        if using_namespaces:
            print(file, '\t', 'Using namespace found in header file')

def check_inline_file(file, component):
    with open(file, 'r+') as f:
        lines = f.readlines()

        copyright = check_copyright(lines)
        if copyright:
            print(file, '\t', 'Copyright check failed', '\t', copyright)
            return

        header = check_glm_header(lines, file)
        if header:
            print(file, '\t', 'Illegal glm header include', header)
            return

        core_dependency = check_core_dependency(lines, component)
        if core_dependency:
            print(file, '\t', 'Wrong dependency (core depends on module)', core_dependency)

        if (not '_doc.inl' in file):
            # The _doc.inl files are allowed to use using namespace as they are inclued
            # from the cpp files and thus don't leak it
            using_namespaces = check_using_namespace(lines)
            if using_namespaces:
                print(file, '\t', 'Using namespace found in inline file')


def check_source_file(file, component):
    with open(file, 'r+') as f:
        lines = f.readlines()

        header = check_glm_header(lines, file)
        if header:
            print(file, '\t',  'Illegal glm header include', header)
            return

        core_dependency = check_core_dependency(lines, component)
        if core_dependency:
            print(file, '\t' 'Wrong core dependency', core_dependency)

        copyright = check_copyright(lines)
        if copyright:
            print(file, '\t', 'Copyright check failed', '\t', copyright)
            return




def check_files(positiveList, negativeList, component, check_function):
    files = glob.glob(positiveList, recursive=True)
    negativeFiles = glob.glob(negativeList, recursive=True)

    files = [f for f in files if f not in negativeFiles]

    for file in files:
        check_function(file, component)




basePath = './'
if len(sys.argv) > 1:
    basePath = sys.argv[1] + '/'

# Check header files
print("Checking header files")
print("=====================")
check_files(basePath + 'include/**/*.h', '', 'openspace_core', check_header_file)
check_files(basePath + 'apps/**/*.h', basePath + 'apps/**/ext/**/*.h', 'openspace_app', check_header_file)
check_files(basePath + 'modules/**/*.h', basePath + 'modules/**/ext/**/*.h', 'openspace_module', check_header_file)
check_files(basePath + 'ext/ghoul/include/**/*.h', '', 'ghoul', check_header_file)
print("")

print("Checking inline files")
print("=====================")
check_files(basePath + 'include/**/*.inl', '', 'openspace_core', check_inline_file)
check_files(basePath + 'src/**/*.inl', '', 'openspace_core', check_inline_file)
check_files(basePath + 'apps/**/*.inl', basePath + 'apps/**/ext/**/*.h', 'openspace_app', check_inline_file)
check_files(basePath + 'modules/**/*.inl', basePath + 'modules/**/ext/**/*.h', 'openspace_module', check_inline_file)
check_files(basePath + 'ext/ghoul/include/**/*.inl', '', 'ghoul', check_inline_file)
print("")

print("Checking source files")
print("=====================")
check_files(basePath + 'src/**/*.cpp', '', 'openspace_core', check_source_file)
check_files(basePath + 'apps/**/*.cpp', basePath + 'apps/**/ext/**/*.cpp', 'openspace_app', check_source_file)
check_files(basePath + 'modules/**/*.cpp', basePath + 'modules/**/ext/**/*.cpp', 'openspace_module', check_source_file)
check_files(basePath + 'ext/ghoul/src/**/*.cpp', '', 'ghoul', check_source_file)
