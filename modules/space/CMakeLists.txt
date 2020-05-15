##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2020                                                                #
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
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablerings.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableorbitalkepler.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesatellites.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesmallbody.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablestars.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/keplertranslation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/spicetranslation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/tletranslation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/horizonstranslation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/spicerotation.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablerings.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableorbitalkepler.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesatellites.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesmallbody.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablestars.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/keplertranslation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/spicetranslation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/tletranslation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/horizonstranslation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/spicerotation.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/constellationbounds_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/constellationbounds_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/debrisViz_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/debrisViz_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/rings_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/rings_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/star_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/star_ge.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/star_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Space"
  space_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
