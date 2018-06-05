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
  ${CMAKE_CURRENT_SOURCE_DIR}/roverterrainrenderermodule.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/roverterrain.h
  ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/roverpathfilereader.h
  ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/subsite.h
  ${CMAKE_CURRENT_SOURCE_DIR}/loadjob/surfacemodelloadjob.h
  ${CMAKE_CURRENT_SOURCE_DIR}/loadjob/loadjob.h
  ${CMAKE_CURRENT_SOURCE_DIR}/loadjob/subsiteinitializationjob.h
  ${CMAKE_CURRENT_SOURCE_DIR}/model/modelprovider.h
  ${CMAKE_CURRENT_SOURCE_DIR}/model/multimodelprovider.h
  ${CMAKE_CURRENT_SOURCE_DIR}/model/singlemodelprovider.h
  ${CMAKE_CURRENT_SOURCE_DIR}/opengl/texturearray.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/sitemanager.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/cachingsurfacemodelprovider.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/asyncsurfacemodelprovider.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/subsitemodels.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/renderableexplorationpath.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/model.h
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/lodmodelswitch.h
  ${CMAKE_CURRENT_SOURCE_DIR}/propertymanager/sitepropertyowner.h
  ${CMAKE_CURRENT_SOURCE_DIR}/propertymanager/drivepropertyowner.h
  ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/pointcloudinfo.h


)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/roverterrainrenderermodule.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/roverterrain.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/roverpathfilereader.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/subsite.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/opengl/texturearray.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/loadjob/surfacemodelloadjob.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/loadjob/subsiteinitializationjob.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/model/modelprovider.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/model/singlemodelprovider.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/model/multimodelprovider.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/sitemanager.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/cachingsurfacemodelprovider.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/asyncsurfacemodelprovider.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/subsitemodels.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/renderableexplorationpath.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/renderable/lodmodelswitch.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/propertymanager/sitepropertyowner.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/propertymanager/drivepropertyowner.cpp

)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/fullsubsite_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/fullsubsite_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/roverpath_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/roverpath_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/roversite_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/roversite_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/subsite_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/subsite_vs.glsl
)

source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "RoverTerrainRenderer"
  rover_terrain_renderer_module
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
