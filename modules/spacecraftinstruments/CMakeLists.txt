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
  dashboard/dashboarditeminstruments.h
  rendering/renderablecrawlingline.h
  rendering/renderablefov.h
  rendering/renderableplaneprojection.h
  rendering/renderableplanetprojection.h
  rendering/renderableshadowcylinder.h
  rendering/renderablemodelprojection.h
  util/decoder.h
  util/hongkangparser.h
  util/instrumenttimesparser.h
  util/image.h
  util/imagesequencer.h
  util/instrumentdecoder.h
  util/labelparser.h
  util/projectioncomponent.h
  util/scannerdecoder.h
  util/sequenceparser.h
  util/targetdecoder.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  dashboard/dashboarditeminstruments.cpp
  rendering/renderablecrawlingline.cpp
  rendering/renderablefov.cpp
  rendering/renderableplaneprojection.cpp
  rendering/renderableplanetprojection.cpp
  rendering/renderableshadowcylinder.cpp
  rendering/renderablemodelprojection.cpp
  util/decoder.cpp
  util/hongkangparser.cpp
  util/instrumenttimesparser.cpp
  util/imagesequencer.cpp
  util/instrumentdecoder.cpp
  util/labelparser.cpp
  util/projectioncomponent.cpp
  util/scannerdecoder.cpp
  util/sequenceparser.cpp
  util/targetdecoder.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/crawlingline_fs.glsl
  shaders/crawlingline_vs.glsl
  shaders/fov_fs.glsl
  shaders/fov_vs.glsl
  shaders/renderableModel_fs.glsl
  shaders/renderableModel_vs.glsl
  shaders/renderableModelProjection_fs.glsl
  shaders/renderableModelProjection_vs.glsl
  shaders/renderablePlanet_fs.glsl
  shaders/renderablePlanet_vs.glsl
  shaders/renderablePlanetProjection_fs.glsl
  shaders/renderablePlanetProjection_vs.glsl
  shaders/terminatorshadow_fs.glsl
  shaders/terminatorshadow_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "SpacecraftInstruments"
  spacecraftinstruments_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

target_precompile_headers(${spacecraftinstruments_module} PRIVATE
)
