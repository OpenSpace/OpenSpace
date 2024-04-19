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

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

set(HEADER_FILES
  horizonsfile.h
  kepler.h
  rendering/renderableconstellationsbase.h
  rendering/renderableconstellationbounds.h
  rendering/renderableconstellationlines.h
  rendering/renderableeclipsecone.h
  rendering/renderablefluxnodes.h
  rendering/renderablehabitablezone.h
  rendering/renderablerings.h
  rendering/renderableorbitalkepler.h
  rendering/renderablestars.h
  rendering/renderabletravelspeed.h
  translation/gptranslation.h
  translation/keplertranslation.h
  translation/spicetranslation.h
  translation/horizonstranslation.h
  rotation/spicerotation.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  horizonsfile.cpp
  kepler.cpp
  spacemodule_lua.inl
  rendering/renderableconstellationsbase.cpp
  rendering/renderableconstellationbounds.cpp
  rendering/renderableconstellationlines.cpp
  rendering/renderableeclipsecone.cpp
  rendering/renderablefluxnodes.cpp
  rendering/renderablehabitablezone.cpp
  rendering/renderablerings.cpp
  rendering/renderableorbitalkepler.cpp
  rendering/renderablestars.cpp
  rendering/renderabletravelspeed.cpp
  translation/gptranslation.cpp
  translation/keplertranslation.cpp
  translation/spicetranslation.cpp
  translation/horizonstranslation.cpp
  rotation/spicerotation.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/constellationbounds_fs.glsl
  shaders/constellationbounds_vs.glsl
  shaders/constellationlines_fs.glsl
  shaders/constellationlines_vs.glsl
  shaders/debrisVizPoints_fs.glsl
  shaders/debrisVizPoints_gs.glsl
  shaders/debrisVizPoints_vs.glsl
  shaders/debrisVizTrails_fs.glsl
  shaders/debrisVizTrails_vs.glsl
  shaders/fluxnodes_fs.glsl
  shaders/fluxnodes_vs.glsl
  shaders/habitablezone_vs.glsl
  shaders/habitablezone_fs.glsl
  shaders/rings_vs.glsl
  shaders/rings_fs.glsl
  shaders/star_fs.glsl
  shaders/star_ge.glsl
  shaders/star_vs.glsl
  shaders/travelspeed_fs.glsl
  shaders/travelspeed_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Space"
  space_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
target_precompile_headers(${space_module} PRIVATE
  <openspace/documentation/documentation.h>
  <openspace/documentation/verifier.h>
  <openspace/properties/numericalproperty.h>
  <openspace/rendering/renderable.h>
  <ghoul/opengl/programobject.h>
  <ghoul/opengl/shaderobject.h>
  <ghoul/opengl/uniformcache.h>
)
