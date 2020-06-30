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
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemangle.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemdate.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemdistance.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemframerate.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemmission.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemparallelconnection.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditempropertyvalue.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemsimulationincrement.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemspacing.h
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemvelocity.h
  ${CMAKE_CURRENT_SOURCE_DIR}/lightsource/cameralightsource.h
  ${CMAKE_CURRENT_SOURCE_DIR}/lightsource/scenegraphlightsource.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/multimodelgeometry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableboxgrid.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecartesianaxes.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablelabels.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemodel.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablenodeline.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplane.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneimagelocal.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneimageonline.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphere.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphericalgrid.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrail.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailorbit.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailtrajectory.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspacedashboard.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceframebuffer.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimagelocal.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimageonline.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/timelinerotation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/constantrotation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/fixedrotation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/luarotation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/staticrotation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/luascale.h
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/nonuniformstaticscale.h
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/staticscale.h
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/timedependentscale.h
  ${CMAKE_CURRENT_SOURCE_DIR}/timeframe/timeframeinterval.h
  ${CMAKE_CURRENT_SOURCE_DIR}/timeframe/timeframeunion.h
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/luatranslation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/statictranslation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/timelinetranslation.h
  
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemangle.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemdate.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemdistance.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemframerate.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemmission.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemparallelconnection.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditempropertyvalue.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemsimulationincrement.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemspacing.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemvelocity.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/lightsource/cameralightsource.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/lightsource/scenegraphlightsource.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/multimodelgeometry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableboxgrid.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecartesianaxes.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablelabels.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemodel.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablenodeline.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplane.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneimagelocal.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneimageonline.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphere.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphericalgrid.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrail.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailorbit.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailtrajectory.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspacedashboard.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceframebuffer.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimagelocal.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimageonline.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/timelinerotation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/constantrotation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/fixedrotation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/luarotation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rotation/staticrotation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/luascale.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/nonuniformstaticscale.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/staticscale.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/scale/timedependentscale.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/timeframe/timeframeinterval.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/timeframe/timeframeunion.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/luatranslation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/statictranslation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/translation/timelinetranslation.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/axes_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/axes_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/grid_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/grid_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/imageplane_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/imageplane_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/line_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/line_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/model_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/model_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/plane_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/plane_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderabletrail_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderabletrail_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/screenspace_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/screenspace_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Base"
  base_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
