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
    ${CMAKE_CURRENT_SOURCE_DIR}/util/iswamanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessor.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessortext.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessorjson.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessorkameleon.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswacygnet.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/dataplane.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/textureplane.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/kameleonplane.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/datasphere.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspacecygnet.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswabasegroup.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswadatagroup.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswakameleongroup.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/texturecygnet.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/datacygnet.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/util/iswamanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessor.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessortext.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessorjson.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/dataprocessorkameleon.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswacygnet.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/dataplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/textureplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/kameleonplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/datasphere.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspacecygnet.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswabasegroup.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswadatagroup.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/iswakameleongroup.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/texturecygnet.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/datacygnet.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/dataplane_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/dataplane_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/textureplane_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/textureplane_vs.glsl
)

source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "Iswa"
    iswa_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
