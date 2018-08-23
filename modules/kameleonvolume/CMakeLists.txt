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
  ${CMAKE_CURRENT_SOURCE_DIR}/kameleonvolumereader.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablekameleonvolume.h
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/kameleondocumentationtask.h
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/kameleonmetadatatojsontask.h
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/kameleonvolumetorawtask.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/kameleonvolumereader.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablekameleonvolume.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/kameleondocumentationtask.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/kameleonmetadatatojsontask.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/kameleonvolumetorawtask.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "KameleonVolume"
    kameleonvolume_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES}
)
