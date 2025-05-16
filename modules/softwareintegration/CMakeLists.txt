##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2022                                                                #
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
  ${CMAKE_CURRENT_SOURCE_DIR}/softwareintegrationmodule.h
  ${CMAKE_CURRENT_SOURCE_DIR}/softwareintegrationmodule.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/network/messagehandler.h
  ${CMAKE_CURRENT_SOURCE_DIR}/network/messagehandler.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/network/network.h
  ${CMAKE_CURRENT_SOURCE_DIR}/network/softwareconnection.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablepointscloud.h
  ${CMAKE_CURRENT_SOURCE_DIR}/simp/simp.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/simp/simp.h
  ${CMAKE_CURRENT_SOURCE_DIR}/utils/syncablestorage.h
  ${CMAKE_CURRENT_SOURCE_DIR}/utils/syncablestorage.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/utils/interruptibleconcurrentqueue.h
  ${CMAKE_CURRENT_SOURCE_DIR}/utils/interruptibleconcurrentqueue.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/session/session.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/softwareintegrationmodule.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/softwareintegrationmodule_lua.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/network/messagehandler.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/network/network.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/network/softwareconnection.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablepointscloud.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/simp/simp.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/utils/syncablestorage.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/session/session.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/point_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/point_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/point_ge.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "SoftwareIntegration"
  softwareintegration
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
