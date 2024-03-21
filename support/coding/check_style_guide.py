# coding=utf-8

"""
OpenSpace

Copyright (c) 2014-2024

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
 * Checking whether any files starts with the UTF-8 Byte-order mark
 * Checking whether a file as empty-only lines
 * Checking whether the default assert macros are used anywhere instead of the
   ghoul_assert macro
 * Checking whether there are TABs in the file

If this script is executed from the base directory of OpenSpace, no arguments need to
be passed, otherwise the first and only argument has to point to the base directory.
Thus, the default value of the first argument is '.'
"""

import fnmatch
import glob
import os
import re
import sys

current_year = '2024'
is_strict_mode = False
is_silent_mode = False

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
    print(ifndef_symbol)
    print(endif_line)
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

  if lines[index[0] + 1][0] != ' ':
    return 'Copyright header is not correctly indented'

  if year != current_year:
    return 'Out of date copyright notice ' + year + ' || ' + current_year


  return ''



def check_byte_order_mark_character(lines):
  c = lines[0][0]
  if c == 'ï':
    return 'File contains UTF-8 byte mark order character'

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

  path_part = file.split('/')[1]
  second_path_part = file.split('/')[2]

  if (path_part.upper() != subcomponent_part) and (second_path_part.upper() != subcomponent_part):
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
    if f in file.replace('\\', '/'):
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



def check_end_of_line(lines):
  if lines[-1][-1] != '\n':
    return lines[-1][-1]
  else:
    return ''



def check_empty_only_line(lines):
  # Disable this check in non-strict mode
  if not is_strict_mode:
    return ''

  index = [i + 1 for i, s in enumerate(lines) if s.translate({ord(c): None for c in '\n\r'}).isspace()]
  if len(index) > 0:
    return index
  else:
    return ''



def check_assert_usage(lines):
  # _assert checks for both ghoul_assert and static_assert, which are both reasonable
  index = [i + 1 for i,s in enumerate(lines) if ('assert(' in s and not '_assert(' in s) and s.strip()[0:2] != '//']
  if len(index) > 0:
    return index
  else:
    return '';



def check_line_length(lines):
  # Disable this check in non-strict mode
  if not is_strict_mode:
    return ''

  index = [i + 1 for i, s in enumerate(lines) if len(s) > (90 + 1)]
  if len(index) > 0:
    return index
  else:
    return ''



def check_empty_character_at_end(lines):
  # Disable this check in non-strict mode
  if not is_strict_mode:
    return ''

  index = [i + 1 for i, s in enumerate(lines) if len(s) > 1 and s[-2] == ' ' and not s.strip() == '']
  if len(index) > 0:
    return index
  else:
    return ''


def check_for_tab(lines):
  index = [i + 1 for i, s in enumerate(lines) if '\t' in s]
  if len(index) > 0:
    return index
  else:
    return ''



def check_for_std_getline(lines):
  index = [i for i,s in enumerate(lines)
          if 'std::getline' in s]
  if len(index) > 0:
    return 'File used wrong std::getline function. Use ghoul::getline from "ghoul/misc/stringhelper.h" instead'
  else:
    return ''



previousSymbols  = {}
def check_header_file(file, component):
  with open(file, 'r+', encoding="utf8") as f:
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

    end_of_line = check_end_of_line(lines)
    if end_of_line:
      print(file, '\t', 'Last line does not contain a newline character: ', end_of_line)
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

    if (not 'ghoul_gl.h' in file):
      # ghoul_gl.h is allowed to use 'using namespace' to pull the gl namespace in
      using_namespaces = check_using_namespace(lines)
      if using_namespaces:
        print(file, '\t', 'Using namespace found in header file')

    bom = check_byte_order_mark_character(lines)
    if bom:
      print(file, '\t', 'Byte order mark failed:', bom)

    empty_only_lines = check_empty_only_line(lines)
    if empty_only_lines:
      print(file, '\t', 'Empty only line: ', empty_only_lines)

    line_length = check_line_length(lines)
    if line_length:
      print(file, '\t', 'Line length exceeded: ', line_length)

    empty_character_at_end = check_empty_character_at_end(lines)
    if empty_character_at_end:
      print(file, '\t', 'Empty character at end: ', empty_character_at_end)

    assert_usage = check_assert_usage(lines)
    if assert_usage:
      print(file, '\t', 'Wrong assert usage: ', assert_usage)

    tabs = check_for_tab(lines)
    if tabs:
      print(file, '\t', 'TABs found: ', tabs)



def check_inline_file(file, component):
  with open(file, 'r+', encoding="utf8") as f:
    lines = f.readlines()

    copyright = check_copyright(lines)
    if copyright:
      print(file, '\t', 'Copyright check failed', '\t', copyright)

    header = check_glm_header(lines, file)
    if header:
      print(file, '\t', 'Illegal glm header include', header)

    core_dependency = check_core_dependency(lines, component)
    if core_dependency:
      print(file, '\t', 'Wrong dependency (core depends on module)', core_dependency)

    end_of_line = check_end_of_line(lines)
    if end_of_line:
      print(file, '\t', 'Last line does not contain a newline character: ', end_of_line)
      return

    bom = check_byte_order_mark_character(lines)
    if bom:
      print(file, '\t', 'Byte order mark failed:', bom)

    empty_only_lines = check_empty_only_line(lines)
    if empty_only_lines:
      print(file, '\t', 'Empty only line: ', empty_only_lines)

    line_length = check_line_length(lines)
    if line_length:
      print(file, '\t', 'Line length exceeded: ', line_length)

    if (not '_doc.inl' in file and not '_lua.inl'):
      # The _doc.inl files are allowed to use using namespace as they are inclued
      # from the cpp files and thus don't leak it
      using_namespaces = check_using_namespace(lines)
      if using_namespaces:
        print(file, '\t', 'Using namespace found in inline file')

    line_length = check_line_length(lines)
    if line_length:
      print(file, '\t', 'Line length exceeded: ', line_length)

    empty_character_at_end = check_empty_character_at_end(lines)
    if empty_character_at_end:
      print(file, '\t', 'Empty character at end: ', empty_character_at_end)

    assert_usage = check_assert_usage(lines)
    if assert_usage:
      print(file, '\t', 'Wrong assert usage: ', assert_usage)

    tabs = check_for_tab(lines)
    if tabs:
      print(file, '\t', 'TABs found: ', tabs)




def check_source_file(file, component):
  with open(file, 'r+', encoding="utf8") as f:
    lines = f.readlines()

    header = check_glm_header(lines, file)
    if header:
      print(file, '\t',  'Illegal glm header include', header)

    core_dependency = check_core_dependency(lines, component)
    if core_dependency:
      print(file, '\t' 'Wrong core dependency', core_dependency)

    end_of_line = check_end_of_line(lines)
    if end_of_line:
      print(file, '\t', 'Last line does not contain a newline character: ', end_of_line)
      return

    copyright = check_copyright(lines)
    if copyright:
      print(file, '\t', 'Copyright check failed', '\t', copyright)

    bom = check_byte_order_mark_character(lines)
    if bom:
      print(file, '\t', 'Byte order mark failed:', bom)

    empty_only_lines = check_empty_only_line(lines)
    if empty_only_lines:
      print(file, '\t', 'Empty only line: ', empty_only_lines)

    line_length = check_line_length(lines)
    if line_length:
      print(file, '\t', 'Line length exceeded: ', line_length)

    empty_character_at_end = check_empty_character_at_end(lines)
    if empty_character_at_end:
      print(file, '\t', 'Empty character at end: ', empty_character_at_end)

    assert_usage = check_assert_usage(lines)
    if assert_usage:
      print(file, '\t', 'Wrong assert usage: ', assert_usage)

    tabs = check_for_tab(lines)
    if tabs:
      print(file, '\t', 'TABs found: ', tabs)

    std_getlines = check_for_std_getline(lines)
    if std_getlines:
      if not 'ghoul/src/misc/stringhelper.cpp' in file:
        print(file, '\t', 'std::getline found instead of ghoul::getline: ', std_getlines)



def check_files(positiveList, negativeList, component, check_function):
  files = []
  for p in positiveList:
    f = glob.glob(p, recursive=True)
    f = [fi.replace('\\', '/') for fi in f]
    files.extend(f)

  negativeFiles = []
  for n in negativeList:
    f = glob.glob(n, recursive=True)
    f = [fi.replace('\\', '/') for fi in f]
    negativeFiles.extend(f)

  filtered_files = [f for f in files if f not in negativeFiles]

  for file in filtered_files:
    check_function(file, component)





basePath = './'
if len(sys.argv) > 1:
  if sys.argv[1] != "strict":
    basePath = sys.argv[1] + '/'

for a in sys.argv:
  if a == "strict":
    is_strict_mode = True
  if a == "silent":
    is_silent_mode = True


# Check header files
if not is_silent_mode:
  print("Checking header files")
  print("=====================")

check_files(
  [basePath + 'include/**/*.h'],
  [],
  'openspace_core',
  check_header_file
)
check_files(
  [basePath + 'apps/**/*.h'],
  [basePath + 'apps/**/ext/**/*.h'],
  'openspace_app',
  check_header_file
)
check_files(
  [basePath + 'modules/**/*.h'],
  [
    basePath + 'modules/**/ext/**/*.h',
    basePath + 'modules/**/node_modules/**/*.h',
    basePath + 'modules/webbrowser/resource.h'
  ],
  'openspace_module',
  check_header_file
)
check_files(
  [basePath + 'ext/ghoul/include/**/*.h'],
  [],
  'ghoul',
  check_header_file
)

if not is_silent_mode:
  print("")
  print("Checking inline files")
  print("=====================")

check_files(
  [basePath + 'include/**/*.inl'],
  [],
  'openspace_core',
  check_inline_file
)
check_files(
  [basePath + 'src/**/*.inl'],
  [],
  'openspace_core',
  check_inline_file
)
check_files(
  [basePath + 'apps/**/*.inl'],
  [basePath + 'apps/**/ext/**/*.inl'],
  'openspace_app',
  check_inline_file
)
check_files(
  [basePath + 'modules/**/*.inl'],
  [basePath + 'modules/**/ext/**/*.h'],
  'openspace_module',
  check_inline_file
)
check_files(
  [basePath + 'ext/ghoul/include/**/*.inl'],
  [],
  'ghoul',
  check_inline_file
)

if not is_silent_mode:
  print("")
  print("Checking source files")
  print("=====================")

check_files(
  [basePath + 'src/**/*.cpp'],
  [basePath + 'src/**/*_codegen.cpp'],
  'openspace_core',
  check_source_file
)
check_files(
  [basePath + 'apps/**/*.cpp'],
  [basePath + 'apps/**/ext/**/*.cpp', basePath + 'apps/**/*_codegen.cpp'],
  'openspace_app',
  check_source_file
)
check_files(
  [basePath + 'modules/**/*.cpp'],
  [
    basePath + 'modules/**/ext/**/*.cpp',
    basePath + 'modules/**/node_modules/**/*.cpp',
    basePath + 'modules/**/*_codegen.cpp'
  ],
  'openspace_module',
  check_source_file
)
check_files(
  [basePath + 'ext/ghoul/src/**/*.cpp'],
  [],
  'ghoul',
  check_source_file
)
