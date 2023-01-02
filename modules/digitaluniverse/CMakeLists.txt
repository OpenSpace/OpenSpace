##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2023                                                                #
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
  rendering/renderablepoints.h
  rendering/renderabledumeshes.h
  rendering/renderablebillboardscloud.h
  rendering/renderableplanescloud.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  rendering/renderablepoints.cpp
  rendering/renderabledumeshes.cpp 
  rendering/renderablebillboardscloud.cpp
  rendering/renderableplanescloud.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/billboard_fs.glsl
  shaders/billboard_gs.glsl
  shaders/billboard_vs.glsl
  shaders/billboardpolygon_fs.glsl
  shaders/billboardpolygon_gs.glsl
  shaders/billboardpolygon_vs.glsl
  shaders/dumesh_vs.glsl
  shaders/dumesh_fs.glsl
  shaders/plane_vs.glsl
  shaders/plane_fs.glsl
  shaders/points_sprite_fs.glsl
  shaders/points_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "DigitalUniverse"
  digitaluniverse_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
