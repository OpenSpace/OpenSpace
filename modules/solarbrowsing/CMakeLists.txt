##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2017                                                                #
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

set(OPENJPEG_ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ext/openjpeg/")
#set(H264_ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ext/libde265/")
#add_subdirectory(${OPENJPEG_ROOT_DIR})
#add_subdirectory(${H264_ROOT_DIR})

set(HEADER_FILES
    # Other
    ${CMAKE_CURRENT_SOURCE_DIR}/util/spacecraftimagerymanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/j2kcodec.h

    ${CMAKE_CURRENT_SOURCE_DIR}/util/structs.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/timedependentstate.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/timedependentstatesequence.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/timedependentstatesequence.inl

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesolarimagery.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesolarimageryprojection.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/spacecraftcameraplane.h
)

source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/util/spacecraftimagerymanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/j2kcodec.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesolarimagery.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesolarimageryprojection.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/spacecraftcameraplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/pixelbufferobject.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module("SolarBrowsing" solarbrowsing_module ${HEADER_FILES} ${SOURCE_FILES})

include_external_library(openspace-module-solarbrowsing PRIVATE openjp2 ${OPENJPEG_ROOT_DIR})
target_include_directories(openspace-module-solarbrowsing SYSTEM PUBLIC ${HEADER_FILES})

target_include_directories(
    openspace-module-solarbrowsing PUBLIC
    # Openjpeg stuff
    ${OPENJPEG_BINARY_DIR}/src/lib/openjp2
    ${OPENJPEG_BINARY_DIR}/src/bin/common
    ${OPENJPEG_ROOT_DIR}/src/lib/openjp2
    ${OPENJPEG_ROOT_DIR}/src/bin/jp2
    ${OPENJPEG_ROOT_DIR}/src/bin/common
    ${Z_INCLUDE_DIRNAME}
    ${PNG_INCLUDE_DIRNAME}
    ${TIFF_INCLUDE_DIRNAME}
    ${CMAKE_SOURCE_DIR}/include
    ${TIFF_INCLUDE_DIRNAME}
)

# Force static linking to prevent __imp* linker errors
target_compile_definitions(openspace-module-solarbrowsing PUBLIC "OPJ_STATIC")