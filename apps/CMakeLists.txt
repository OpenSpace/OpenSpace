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

# Returns whether the application located at 'path' is a default application or not
function (get_application_attribute_default path result)
  set(${result} OFF PARENT_SCOPE)
  if (EXISTS "${path}/include.cmake")
    unset(DEFAULT_APPLICATION)
    include(${path}/include.cmake)
    if (DEFINED DEFAULT_APPLICATION)
      set(${result} ${DEFAULT_APPLICATION} PARENT_SCOPE)
    endif ()
  endif ()
endfunction()

# Get a list of all of the application directories
file(GLOB app_dirs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/*)
# Remove the .DS_Store present on Mac
list(REMOVE_ITEM app_dirs ".DS_Store")
list(REMOVE_ITEM app_dirs "CMakeLists.txt")




message(STATUS "Enabled applications:")
foreach (app ${app_dirs})
  string(TOUPPER ${app} upper_app)
  if (OPENSPACE_APPLICATION_${upper_app})
    message(STATUS "\t${app}  (${CMAKE_CURRENT_SOURCE_DIR}/${app})")
  endif ()
endforeach ()

# First create all of the options for the applications. In case that one of the
# applications fail to include later, we still want all of them listed
foreach (app ${app_dirs})
  set(app_dir "${CMAKE_CURRENT_SOURCE_DIR}/${app}")
  string(TOUPPER ${app} upper_app)

  get_application_attribute_default(${app_dir} is_default_application)
  option(OPENSPACE_APPLICATION_${upper_app} "Build ${app} application" ${is_default_application})
endforeach ()

foreach (app ${app_dirs})
  set(app_dir "${CMAKE_CURRENT_SOURCE_DIR}/${app}")
  string(TOUPPER ${app} upper_app)
  if (OPENSPACE_APPLICATION_${upper_app})
    begin_header("Application: ${app}")
    add_subdirectory(${app_dir})
    end_header("End: Application: ${app}")
  endif ()
endforeach ()
