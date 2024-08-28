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
  gaiamodule.h
  rendering/renderablegaiastars.h
  rendering/octreemanager.h
  rendering/octreeculler.h
  tasks/readfilejob.h
  tasks/readfitstask.h
  tasks/readspecktask.h
  tasks/constructoctreetask.h
  rendering/gaiaoptions.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  gaiamodule.cpp
  rendering/renderablegaiastars.cpp
  rendering/octreemanager.cpp
  rendering/octreeculler.cpp
  tasks/readfilejob.cpp
  tasks/readfitstask.cpp
  tasks/readspecktask.cpp
  tasks/constructoctreetask.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/gaia_vbo_vs.glsl
  shaders/gaia_ssbo_vs.glsl
  shaders/gaia_billboard_nofbo_fs.glsl
  shaders/gaia_billboard_fs.glsl
  shaders/gaia_billboard_ge.glsl
  shaders/gaia_point_fs.glsl
  shaders/gaia_point_ge.glsl
  shaders/gaia_tonemapping_vs.glsl
  shaders/gaia_tonemapping_point_fs.glsl
  shaders/gaia_tonemapping_billboard_fs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Gaia"
  gaia
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
