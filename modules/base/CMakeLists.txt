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
  dashboard/dashboarditemangle.h
  dashboard/dashboarditemdate.h
  dashboard/dashboarditemdistance.h
  dashboard/dashboarditemelapsedtime.h
  dashboard/dashboarditemframerate.h
  dashboard/dashboarditeminputstate.h
  dashboard/dashboarditemmission.h
  dashboard/dashboarditemparallelconnection.h
  dashboard/dashboarditempropertyvalue.h
  dashboard/dashboarditemsimulationincrement.h
  dashboard/dashboarditemspacing.h
  dashboard/dashboarditemtext.h
  dashboard/dashboarditemvelocity.h
  lightsource/cameralightsource.h
  lightsource/scenegraphlightsource.h
  rendering/grids/renderableboxgrid.h
  rendering/grids/renderablegrid.h
  rendering/grids/renderableradialgrid.h
  rendering/grids/renderablesphericalgrid.h
  rendering/pointcloud/renderableinterpolatedpoints.h
  rendering/pointcloud/renderablepointcloud.h
  rendering/pointcloud/renderablepolygoncloud.h
  rendering/pointcloud/sizemappingcomponent.h
  rendering/renderablecartesianaxes.h
  rendering/renderabledisc.h
  rendering/renderablelabel.h
  rendering/renderablemodel.h
  rendering/renderablenodearrow.h
  rendering/renderablenodeline.h
  rendering/renderableplane.h
  rendering/renderableplaneimagelocal.h
  rendering/renderableplaneimageonline.h
  rendering/renderableplanetimevaryingimage.h
  rendering/renderableprism.h
  rendering/renderablesphere.h
  rendering/renderablesphereimagelocal.h
  rendering/renderablesphereimageonline.h
  rendering/renderabletimevaryingsphere.h
  rendering/renderabletrail.h
  rendering/renderabletrailorbit.h
  rendering/renderabletrailtrajectory.h
  rendering/screenspacedashboard.h
  rendering/screenspaceframebuffer.h
  rendering/screenspaceimagelocal.h
  rendering/screenspaceimageonline.h
  rotation/timelinerotation.h
  rotation/constantrotation.h
  rotation/fixedrotation.h
  rotation/luarotation.h
  rotation/staticrotation.h
  scale/luascale.h
  scale/nonuniformstaticscale.h
  scale/staticscale.h
  scale/timedependentscale.h
  timeframe/timeframeinterval.h
  timeframe/timeframeunion.h
  translation/luatranslation.h
  translation/statictranslation.h
  translation/timelinetranslation.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  dashboard/dashboarditemangle.cpp
  dashboard/dashboarditemdate.cpp
  dashboard/dashboarditemdistance.cpp
  dashboard/dashboarditemelapsedtime.cpp
  dashboard/dashboarditemframerate.cpp
  dashboard/dashboarditeminputstate.cpp
  dashboard/dashboarditemmission.cpp
  dashboard/dashboarditemparallelconnection.cpp
  dashboard/dashboarditempropertyvalue.cpp
  dashboard/dashboarditemsimulationincrement.cpp
  dashboard/dashboarditemspacing.cpp
  dashboard/dashboarditemtext.cpp
  dashboard/dashboarditemvelocity.cpp
  lightsource/cameralightsource.cpp
  lightsource/scenegraphlightsource.cpp
  rendering/grids/renderableboxgrid.cpp
  rendering/grids/renderablegrid.cpp
  rendering/grids/renderableradialgrid.cpp
  rendering/grids/renderablesphericalgrid.cpp
  rendering/pointcloud/renderableinterpolatedpoints.cpp
  rendering/pointcloud/renderablepointcloud.cpp
  rendering/pointcloud/renderablepolygoncloud.cpp
  rendering/pointcloud/sizemappingcomponent.cpp
  rendering/renderablecartesianaxes.cpp
  rendering/renderabledisc.cpp
  rendering/renderablelabel.cpp
  rendering/renderablemodel.cpp
  rendering/renderablenodearrow.cpp
  rendering/renderablenodeline.cpp
  rendering/renderableplane.cpp
  rendering/renderableplaneimagelocal.cpp
  rendering/renderableplaneimageonline.cpp
  rendering/renderableplanetimevaryingimage.cpp
  rendering/renderableprism.cpp
  rendering/renderablesphere.cpp
  rendering/renderablesphereimagelocal.cpp
  rendering/renderablesphereimageonline.cpp
  rendering/renderabletimevaryingsphere.cpp
  rendering/renderabletrail.cpp
  rendering/renderabletrailorbit.cpp
  rendering/renderabletrailtrajectory.cpp
  rendering/screenspacedashboard.cpp
  rendering/screenspacedashboard_lua.inl
  rendering/screenspaceframebuffer.cpp
  rendering/screenspaceimagelocal.cpp
  rendering/screenspaceimageonline.cpp
  rotation/timelinerotation.cpp
  rotation/constantrotation.cpp
  rotation/fixedrotation.cpp
  rotation/luarotation.cpp
  rotation/staticrotation.cpp
  scale/luascale.cpp
  scale/nonuniformstaticscale.cpp
  scale/staticscale.cpp
  scale/timedependentscale.cpp
  timeframe/timeframeinterval.cpp
  timeframe/timeframeunion.cpp
  translation/luatranslation.cpp
  translation/statictranslation.cpp
  translation/timelinetranslation.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/arrow_fs.glsl
  shaders/arrow_vs.glsl
  shaders/axes_fs.glsl
  shaders/axes_vs.glsl
  shaders/disc_fs.glsl
  shaders/disc_vs.glsl
  shaders/grid_vs.glsl
  shaders/grid_fs.glsl
  shaders/imageplane_fs.glsl
  shaders/imageplane_vs.glsl
  shaders/line_fs.glsl
  shaders/line_vs.glsl
  shaders/model_fs.glsl
  shaders/model_vs.glsl
  shaders/plane_fs.glsl
  shaders/plane_vs.glsl
  shaders/pointcloud/pointcloud_fs.glsl
  shaders/pointcloud/pointcloud_gs.glsl
  shaders/pointcloud/pointcloud_vs.glsl
  shaders/pointcloud/pointcloud_interpolated_vs.glsl
  shaders/polygon_fs.glsl
  shaders/polygon_gs.glsl
  shaders/polygon_vs.glsl
  shaders/prism_fs.glsl
  shaders/prism_vs.glsl
  shaders/renderabletrail_fs.glsl
  shaders/renderabletrail_vs.glsl
  shaders/screenspace_fs.glsl
  shaders/screenspace_vs.glsl
  shaders/sphere_fs.glsl
  shaders/sphere_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Base"
  base_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
target_precompile_headers(${base_module} PRIVATE
  <openspace/documentation/documentation.h>
  <openspace/documentation/verifier.h>
  <openspace/properties/numericalproperty.h>
  <openspace/rendering/renderable.h>
  <ghoul/opengl/programobject.h>
  <ghoul/opengl/shaderobject.h>
  <ghoul/opengl/uniformcache.h>
  <future>
  <map>
)
