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
  ${CMAKE_CURRENT_SOURCE_DIR}/gaiamodule.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablegaiastars.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/octreemanager.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/octreeculler.h
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/readfilejob.h 
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/readfitstask.h 
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/readspecktask.h
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/constructoctreetask.h 
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gaiaoptions.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/gaiamodule.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablegaiastars.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/octreemanager.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/octreeculler.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/readfilejob.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/readfitstask.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/readspecktask.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/tasks/constructoctreetask.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_vbo_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_ssbo_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_billboard_nofbo_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_billboard_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_billboard_ge.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_point_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_point_ge.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_tonemapping_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_tonemapping_point_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gaia_tonemapping_billboard_fs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Gaia"
  gaia
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
