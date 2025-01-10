##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2024                                                                #
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

function (create_library_name module_name library_name)
  string(TOLOWER ${module_name} module_name)
  set(${library_name} "openspace-module-${module_name}" PARENT_SCOPE)
endfunction ()

function (create_option_name module_name option_name)
  string(TOUPPER ${module_name} module_name)
  set(${option_name} "OPENSPACE_MODULE_${module_name}" PARENT_SCOPE)
endfunction ()

function (create_define_name module_name define_name)
  string(TOUPPER ${module_name} module_name)
  set(${define_name} "OPENSPACE_MODULE_${module_name}_ENABLED" PARENT_SCOPE)
endfunction ()

function (create_module_header_filepath module_name module_path header_filepath)
  string(TOLOWER ${module_name} module_name)
  string(REPLACE "${PROJECT_SOURCE_DIR}/" "" module_path ${module_path})
  set(header_filepath "${module_path}/${module_name}module.h" PARENT_SCOPE)
endfunction ()

function (create_module_class_name module_name module_class_name)
  # Capitalize the first character
  string(SUBSTRING ${module_name} 0 1 FIRST_LETTER)
  string(TOUPPER ${FIRST_LETTER} FIRST_LETTER)
  string(REGEX REPLACE "^.(.*)" "${FIRST_LETTER}\\1" module_name "${module_name}")
endfunction ()
