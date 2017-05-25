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
add_subdirectory(${OPENJPEG_ROOT_DIR})

set(HEADER_FILES
     # Openjpeg stuff
    ${OPENJPEG_BINARY_DIR}/src/lib/openjp2
    ${OPENJPEG_BINARY_DIR}/src/bin/common
    ${OPENJPEG_SOURCE_DIR}/src/lib/openjp2
    ${OPENJPEG_SOURCE_DIR}/src/bin/jp2
    ${OPENJPEG_SOURCE_DIR}/src/bin/common
    ${Z_INCLUDE_DIRNAME}
    ${PNG_INCLUDE_DIRNAME}
    ${TIFF_INCLUDE_DIRNAME}
    ${CMAKE_SOURCE_DIR}/include
    ${TIFF_INCLUDE_DIRNAME}

    # Other
    ${CMAKE_CURRENT_SOURCE_DIR}/util/spacecraftimagerymanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/simplej2kcodec.h

    ${CMAKE_CURRENT_SOURCE_DIR}/util/streambuffer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/streambuffer.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/util/timedependentstate.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/timedependentstatesequence.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/timedependentstatesequence.inl

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablespacecraftcameraplane.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablespacecraftcamerasphere.h

    # This should be moved to global util
    ${OPENSPACE_BASE_DIR}/modules/globebrowsing/other/threadpool.h
    ${OPENSPACE_BASE_DIR}/modules/globebrowsing/other/concurrentjobmanager.h
    ${OPENSPACE_BASE_DIR}/modules/globebrowsing/other/concurrentjobmanager.inl
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/util/spacecraftimagerymanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/simplej2kcodec.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablespacecraftcameraplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablespacecraftcamerasphere.cpp

    # This should be moved to global util
    ${OPENSPACE_BASE_DIR}/modules/globebrowsing/other/threadpool.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module("SolarBrowsing" solarbrowsing_module ${HEADER_FILES} ${SOURCE_FILES})
set(MODULE_NAME openspace-module-solarbrowsing)

target_include_directories(${MODULE_NAME} SYSTEM PUBLIC ${HEADER_FILES})
# Link to openjpeg
target_link_libraries(${MODULE_NAME} openjp2)
#target_link_libraries(${MODULE_NAME} ${GROK_LIBRARY_NAME} ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT})
