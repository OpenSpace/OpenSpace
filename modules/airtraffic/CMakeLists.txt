##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2021                                                                #
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
  airtrafficmodule.h
  rendering/renderableairtrafficlive.h
  rendering/renderableairtraffichistorical.h
  rendering/renderableairtrafficbound.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  airtrafficmodule.cpp
  rendering/renderableairtrafficlive.cpp
  rendering/renderableairtraffichistorical.cpp
  rendering/renderableairtrafficbound.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/airtraffic_utilities.glsl
  shaders/airtrafficlive_vs.glsl
  shaders/airtrafficlive_fs.glsl
  shaders/airtrafficlive_ge.glsl
  shaders/airtraffichistorical_vs.glsl
  shaders/airtraffichistorical_fs.glsl
  shaders/airtraffichistorical_ge.glsl
  shaders/airtrafficbound_vs.glsl
  shaders/airtrafficbound_fs.glsl
  shaders/airtrafficbound_ge.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "AirTraffic"
  airtraffic
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
