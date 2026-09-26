##########################################################################################
#                                                                                        #
# GHOUL                                                                                  #
# General Helpful Open Utility Library                                                   #
#                                                                                        #
# Copyright (c) 2012-2026                                                                #
#                                                                                        #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#                                                                                        #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#                                                                                        #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

# These macros are inspired by the work of the MinVR team at
# https://github.com/MinVR/MinVR

# The h1, h2, h3, hx macros print messages decorated according to their importance
macro(h1 title)
  string(TOUPPER ${title} _title)
  message(STATUS "======= ${_title} =======")
endmacro ()



macro (h2 title)
  message(STATUS "**** ${title} ****")
endmacro ()



macro (h3 title)
  message(STATUS "--- ${title} ---")
endmacro ()



macro (hx title)
  message(STATUS "${title}")
endmacro ()



# This method uses the currently active indentation level to automatically call the
# correct version of the h1, h2, h3, hx methods. A call to `begin_header` **has** to have
# a mached `end_header` call or bad things will happen
macro (begin_header title)
  if (NOT DEFINED __current_header_indent)
    set(__current_header_indent 0)
  endif ()
  if (${__current_header_indent} EQUAL 0)
    h1("${title}")
  elseif (${__current_header_indent} EQUAL 1)
    h2("${title}")
  elseif (${__current_header_indent} EQUAL 2)
    h3("${title}")
  else ()
    hx("${title}")
  endif ()
  MATH(EXPR __current_header_indent "${__current_header_indent}+1")
endmacro ()



# Ends the block defined by a `begin_header`. If an optional argument is provided to this
# macro, it is printed with the same level of importance as the initial `begin_header`
# call
macro (end_header)
  if (NOT DEFINED __current_header_indent)
    message("No previous header call to match end_header")
  endif ()

  if (${__current_header_indent} GREATER 0)
    MATH(EXPR __current_header_indent "${__current_header_indent}-1")
  endif ()

  set(extra_macro_argv ${ARGN})
  list(LENGTH extra_macro_argv extra_macro_argc)
  if (extra_macro_argc GREATER 0)
    if (${__current_header_indent} EQUAL 0)
      h1("${extra_macro_argv}")
    elseif (${__current_header_indent} EQUAL 1)
      h2("${extra_macro_argv}")
    elseif (${__current_header_indent} EQUAL 2)
      h3("${extra_macro_argv}")
    else ()
      hx("${extra_macro_argv}")
    endif ()
  endif ()
endmacro()



macro (begin_dependency name)
  message(STATUS "Dependency: ${name}")
endmacro ()



macro (end_dependency)
  set(extra_macro_argv ${ARGN})
  list(LENGTH extra_macro_argv extra_macro_argc)
  if (extra_macro_argc GREATER 0)
    message(STATUS "End dependency: ${extra_macro_argv}")
  endif ()
  message(STATUS "")
endmacro ()



macro (begin_module name)
  set(__current_module_name ${name})
  begin_header("Module: ${name}")
endmacro ()



macro (end_module)
  set(extra_macro_argv ${ARGN})
  list(LENGTH extra_macro_argv extra_macro_argc)
  if (extra_macro_argc GREATER 0)
    end_header("End module: ${extra_macro_argv}")
  else ()
    end_header()
  endif ()
  message(STATUS "")
endmacro ()
