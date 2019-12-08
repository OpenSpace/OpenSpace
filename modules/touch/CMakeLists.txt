##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2018                                                                #
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

include(${OPENSPACE_CMAKE_EXT_DIR}/module_definition.cmake)

set(HEADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/ext/levmarq.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/tuioear.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/touchinteraction.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/touchmarker.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/win32_touch.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/ext/levmarq.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tuioear.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/touchinteraction.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/touchmarker.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/win32_touch.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/marker_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/marker_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Touch"
  touch_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

include_external_library(${touch_module} PUBLIC libTUIO11 ${CMAKE_CURRENT_SOURCE_DIR}/ext)

disable_external_warnings_for_file(${CMAKE_CURRENT_SOURCE_DIR}/ext/levmarq.cpp)
