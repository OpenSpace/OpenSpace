##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2022                                                                #
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

include(${GHOUL_BASE_DIR}/support/cmake/copy_shared_libraries.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/application_definition.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/global_variables.cmake)

set(MACOSX_BUNDLE_ICON_FILE openspace.icns)

set(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT ON CACHE BOOL "" FORCE)
add_subdirectory(ext/minvr)

create_new_application(OpenSpace-MinVR
  ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.rc
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.icns
)

set_source_files_properties(
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.icns
  PROPERTIES MACOSX_PACKAGE_LOCATION "Resources"
)

target_include_directories(OpenSpace-MinVR PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/ext/minvr/src)

# @FRAGILE:  This will break if we ever compile MinVR without GLFW support.
#            Maybe there is a better way to access glfwGetProcAddress, which is probably
#            The only thing that we need from this include
target_include_directories(OpenSpace-MinVR PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/ext/minvr/external/GLFW/src/include)


target_link_libraries(OpenSpace-MinVR PUBLIC openspace-core MinVR)

# Web Browser and Web gui
# Why not put these in the module's path? Because they do not have access to the
# target as of July 2017, which is needed.
if (OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  if (WIN32)
    set(RESOURCE_FILE ${OPENSPACE_APPS_DIR}/OpenSpace-MinVR/openspace.rc)
  endif ()

  # Add the CEF binary distribution's cmake/ directory to the module path and
  # find CEF to initialize it properly.
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBBROWSER_MODULE_PATH}/cmake")
  include(webbrowser_helpers)

  set_cef_targets("${CEF_ROOT}" OpenSpace-MinVR)
  run_cef_platform_config("${CEF_ROOT}" "${CEF_TARGET}" "${WEBBROWSER_MODULE_PATH}")
elseif (OPENSPACE_MODULE_WEBBROWSER)
  message(WARNING "Web configured to be included, but no CEF_ROOT was found, please try configuring CMake again.")
endif ()

if (MSVC)
  # This library is used for being able to output the callstack if an exception escapes
  target_link_libraries(OpenSpace-MinVR PUBLIC Dbghelp.lib)
endif ()
