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
    ${CMAKE_CURRENT_SOURCE_DIR}/envelope.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolume.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolumemetadata.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolumereader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolumewriter.h
    ${CMAKE_CURRENT_SOURCE_DIR}/textureslicevolumereader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/textureslicevolumereader.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/transferfunction.h
    ${CMAKE_CURRENT_SOURCE_DIR}/transferfunctionhandler.h
    ${CMAKE_CURRENT_SOURCE_DIR}/transferfunctionproperty.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolumemetadata.h
    ${CMAKE_CURRENT_SOURCE_DIR}/lrucache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/lrucache.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/linearlrucache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/linearlrucache.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/volumegridtype.h
    ${CMAKE_CURRENT_SOURCE_DIR}/volumesampler.h
    ${CMAKE_CURRENT_SOURCE_DIR}/volumesampler.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/volumeutils.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletimevaryingvolume.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/basicvolumeraycaster.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/volumeclipplane.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/volumeclipplanes.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tasks/generaterawvolumetask.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/envelope.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolume.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolumemetadata.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolumereader.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/rawvolumewriter.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/textureslicevolumereader.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/transferfunction.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/transferfunctionhandler.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/transferfunctionproperty.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/volumesampler.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/volumegridtype.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/volumeutils.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletimevaryingvolume.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/basicvolumeraycaster.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/volumeclipplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/volumeclipplanes.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tasks/generaterawvolumetask.cpp
)

source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "Volume"
    volume_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
