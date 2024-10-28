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

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

set(HEADER_FILES
  renderableplanespout.h
  renderablespherespout.h
  screenspacespout.h
  spoutlibrary.h
  spoutwrapper.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  renderableplanespout.cpp
  renderablespherespout.cpp
  screenspacespout.cpp
  spoutwrapper.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SGCT_SPOUT_SUPPORT ON CACHE BOOL "" FORCE)

create_new_module(
  "Spout"
  spout_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)

add_library(spout SHARED IMPORTED)
target_include_directories(spout SYSTEM INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/ext/spout)
set_target_properties(
  spout PROPERTIES
  IMPORTED_IMPLIB ${CMAKE_CURRENT_SOURCE_DIR}/ext/spout/SpoutLibrary.lib
  IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/ext/spout/SpoutLibrary.dll
)
target_link_libraries(openspace-module-spout PRIVATE spout)

target_compile_definitions(openspace-module-spout PUBLIC "OPENSPACE_HAS_SPOUT")
