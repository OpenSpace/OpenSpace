##########################################################################################
#
# OpenSpace                                                                              #
#
# Copyright (c) 2014-2023                                                                #
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

set(HEADER_FILES
  softwareintegrationmodule.h
  softwareintegrationmodule.inl
  network/messagehandler.h
  network/messagehandler.inl
  network/network.h
  network/softwareconnection.h
  rendering/renderablepointscloud.h
  simp/simp.inl
  simp/simp.h
  utils/syncablestorage.h
  utils/syncablestorage.inl
  utils/interruptibleconcurrentqueue.h
  utils/interruptibleconcurrentqueue.inl
  session/session.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  softwareintegrationmodule.cpp
  softwareintegrationmodule_lua.inl
  network/messagehandler.cpp
  network/network.cpp
  network/softwareconnection.cpp
  rendering/renderablepointscloud.cpp
  simp/simp.cpp
  utils/syncablestorage.cpp
  session/session.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/point_vs.glsl
  shaders/point_fs.glsl
  shaders/point_ge.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "SoftwareIntegration"
  softwareintegration
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
