#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014                                                                    #
#                                                                                       #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this  #
# software and associated documentation files (the "Software"), to deal in the Software #
# without restriction, including without limitation the rights to use, copy, modify,    #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    #
# permit persons to whom the Software is furnished to do so, subject to the following   #
# conditions:                                                                           #
#                                                                                       #
# The above copyright notice and this permission notice shall be included in all copies #
# or substantial portions of the Software.                                              #
#                                                                                       #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         #
#########################################################################################


#set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

set(SOURCE_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(HEADER_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})

file(GLOB MAIN_SOURCE ${SOURCE_ROOT_DIR}/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${MAIN_SOURCE})
file(GLOB MAIN_HEADER ${SOURCE_ROOT_DIR}/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${MAIN_HEADER})
source_group(Main FILES ${MAIN_SOURCE} ${MAIN_HEADER})

file(GLOB EXTERNALCONTROL_SOURCE ${SOURCE_ROOT_DIR}/externalcontrol/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${EXTERNALCONTROL_SOURCE})
file(GLOB EXTERNALCONTROL_HEADER ${HEADER_ROOT_DIR}/externalcontrol/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${EXTERNALCONTROL_HEADER})
source_group(ExternalControl FILES ${EXTERNALCONTROL_SOURCE} ${EXTERNALCONTROL_HEADER})

file(GLOB SCENEGRAPH_SOURCE ${SOURCE_ROOT_DIR}/scenegraph/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${SCENEGRAPH_SOURCE})
file(GLOB SCENEGRAPH_HEADER ${HEADER_ROOT_DIR}/scenegraph/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${SCENEGRAPH_HEADER})
source_group(ExternalControl FILES ${SCENEGRAPH_SOURCE} ${SCENEGRAPH_HEADER})

file(GLOB UTIL_SOURCE ${SOURCE_ROOT_DIR}/util/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${UTIL_SOURCE})
file(GLOB UTIL_HEADER ${HEADER_ROOT_DIR}/util/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${UTIL_HEADER})
source_group(Util FILES ${UTIL_SOURCE} ${UTIL_HEADER})

include_directories(${HEADER_ROOT_DIR})

add_executable(OpenSpace ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
target_link_libraries(OpenSpace ${DEPENDENT_LIBS})
