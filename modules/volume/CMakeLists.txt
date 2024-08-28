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
  envelope.h
  rawvolume.h
  rawvolumemetadata.h
  rawvolumereader.h
  rawvolumewriter.h
  textureslicevolumereader.h
  textureslicevolumereader.inl
  transferfunction.h
  transferfunctionhandler.h
  transferfunctionproperty.h
  rawvolumemetadata.h
  lrucache.h
  lrucache.inl
  linearlrucache.h
  linearlrucache.inl
  volumegridtype.h
  volumesampler.h
  volumesampler.inl
  volumeutils.h
  rendering/renderabletimevaryingvolume.h
  rendering/basicvolumeraycaster.h
  rendering/volumeclipplane.h
  rendering/volumeclipplanes.h
  tasks/generaterawvolumetask.h
  tasks/generaterawvolumefromfiletask.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  envelope.cpp
  rawvolume.inl
  rawvolumemetadata.cpp
  rawvolumereader.inl
  rawvolumewriter.inl
  textureslicevolumereader.inl
  transferfunction.cpp
  transferfunctionhandler.cpp
  transferfunctionproperty.cpp
  volumesampler.inl
  volumegridtype.cpp
  volumeutils.cpp
  rendering/renderabletimevaryingvolume.cpp
  rendering/basicvolumeraycaster.cpp
  rendering/volumeclipplane.cpp
  rendering/volumeclipplanes.cpp
  tasks/generaterawvolumetask.cpp
  tasks/generaterawvolumefromfiletask.cpp
)

source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Volume"
  volume_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
