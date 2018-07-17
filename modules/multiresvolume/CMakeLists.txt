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
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/atlasmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/brickmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/brickselector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/brickcover.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/brickselection.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/multiresvolumeraycaster.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/shenbrickselector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/tfbrickselector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/localtfbrickselector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simpletfbrickselector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemultiresvolume.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/tsp.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/histogrammanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/errorhistogrammanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/localerrorhistogrammanager.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/atlasmanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/brickcover.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/brickmanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/brickselection.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/multiresvolumeraycaster.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/shenbrickselector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/tfbrickselector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/localtfbrickselector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simpletfbrickselector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemultiresvolume.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/tsp.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/histogrammanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/errorhistogrammanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/localerrorhistogrammanager.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "MultiresVolume"
    multiresvolume_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES}
)
