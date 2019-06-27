##########################################################################################
##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2019                                                                #
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
include(${GHOUL_BASE_DIR}/support/cmake/message_macros.cmake)
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

# Add dependency on main MinVR library
target_include_directories(OpenSpace-MinVR PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/ext/minvr/src)
target_link_libraries(OpenSpace-MinVR MinVR)

# Additional depends needed for MinVR
# Note: MinVR's GLFW plugin (a .dll) links with glfw -- we wouldn't ordinarily need to include
# GLFW headers here, but OpenSpace requires special access to glGetProcAddress
target_include_directories(OpenSpace-MinVR PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/ext/minvr/external/GLFW/src/include)

# Add dependency on main OpenSpace library
target_link_libraries(OpenSpace-MinVR openspace-core)


if (MSVC)
  # This library is used for being able to output the callstack if an exception escapes
  target_link_libraries(OpenSpace-MinVR Dbghelp.lib)
endif()
