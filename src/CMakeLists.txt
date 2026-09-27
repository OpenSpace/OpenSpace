##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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

include(${PROJECT_SOURCE_DIR}/support/cmake/set_openspace_compile_settings.cmake)

set(OPENSPACE_SHADERS
  ${PROJECT_SOURCE_DIR}/shaders/blending.glsl
  ${PROJECT_SOURCE_DIR}/shaders/core/screenspace_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/core/screenspace_vs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/core/xyzuvrgba_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/core/xyzuvrgba_vs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/exitframebuffer_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/fxaa_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/fxaa_vs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/hdr_and_filtering_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/hdr_and_filtering_vs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/inside.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/mergedownscaledvolume_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/mergedownscaledvolume_vs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/outside.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/raycastframebuffer_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/renderframebuffer_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/framebuffer/resolveframebuffer_vs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/powerscaling/powerscaling_fs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/powerscaling/powerscaling_vs.glsl
  ${PROJECT_SOURCE_DIR}/shaders/powerscaling/powerscalingmath.glsl
  ${PROJECT_SOURCE_DIR}/shaders/floatoperations.glsl
  ${PROJECT_SOURCE_DIR}/shaders/fragment.glsl
  ${PROJECT_SOURCE_DIR}/shaders/hdr.glsl
  ${PROJECT_SOURCE_DIR}/shaders/rand.glsl
  ${PROJECT_SOURCE_DIR}/shaders/render_fs.glsl
)
source_group("Shader Files" FILES ${OPENSPACE_SHADERS})


##########################################################################################
# Create OpenSpace target
##########################################################################################
add_library(openspace-core STATIC)

target_sources(
  openspace-core
  PRIVATE
    ${PROJECT_SOURCE_DIR}/include/openspace/format.h
    ${PROJECT_SOURCE_DIR}/include/openspace/freetype.h
    ${PROJECT_SOURCE_DIR}/include/openspace/glm.h
    ${PROJECT_SOURCE_DIR}/include/openspace/json.h
    ${PROJECT_SOURCE_DIR}/include/openspace/camera/camera.h
    ${PROJECT_SOURCE_DIR}/include/openspace/camera/camerapose.h
    ${PROJECT_SOURCE_DIR}/include/openspace/cmdparser/commandlinecommand.h
    ${PROJECT_SOURCE_DIR}/include/openspace/cmdparser/commandlinecommand.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/cmdparser/commandlineparser.h
    ${PROJECT_SOURCE_DIR}/include/openspace/cmdparser/multiplecommand.h
    ${PROJECT_SOURCE_DIR}/include/openspace/cmdparser/multiplecommand.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/cmdparser/singlecommand.h
    ${PROJECT_SOURCE_DIR}/include/openspace/cmdparser/singlecommand.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/data/colormaploader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/data/csvloader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/data/dataloader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/data/datamapping.h
    ${PROJECT_SOURCE_DIR}/include/openspace/data/speckloader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/designpattern/event.h
    ${PROJECT_SOURCE_DIR}/include/openspace/designpattern/event.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/documentation/core_registration.h
    ${PROJECT_SOURCE_DIR}/include/openspace/documentation/documentation.h
    ${PROJECT_SOURCE_DIR}/include/openspace/documentation/documentationengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/documentation/schema.h
    ${PROJECT_SOURCE_DIR}/include/openspace/documentation/verifier.h
    ${PROJECT_SOURCE_DIR}/include/openspace/documentation/verifier.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/configuration.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/downloadmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/globals.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/globalscallbacks.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/logfactory.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/moduleengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/moduleengine.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/openspaceengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/settings.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/syncengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/engine/windowdelegate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/events/event.h
    ${PROJECT_SOURCE_DIR}/include/openspace/events/eventengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/events/eventengine.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/filesystem/cachemanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/filesystem/file.h
    ${PROJECT_SOURCE_DIR}/include/openspace/filesystem/filesystem.h
    ${PROJECT_SOURCE_DIR}/include/openspace/font/font.h
    ${PROJECT_SOURCE_DIR}/include/openspace/font/fonterrors.h
    ${PROJECT_SOURCE_DIR}/include/openspace/font/fontmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/font/fontrenderer.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/action.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/actionmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/interactionhandler.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/interactionmonitor.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/joystickinputstate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/keybindingmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/keyboardinputstate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/keyframerecordinghandler.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/mouseinputstate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/sessionrecording.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/sessionrecordinghandler.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/touchinputstate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/touchmarker.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/websocketinputstate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/tasks/convertrecformattask.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelanimation.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelgeometry.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelmesh.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelnode.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelreader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelreaderassimp.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelreaderbase.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/model/modelreaderbinary.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/socket.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/socketserver.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/sockettype.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/tcpsocket.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/tcpsocket.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/tcpsocketserver.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/websocket.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/socket/websocketserver.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/texture/texturereader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/texture/texturewriter.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/volume/rawvolumereader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/io/volume/volumereader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/bufferlog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/callbacklog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/consolelog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/htmllog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/log.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/loglevel.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/logmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/logmanager.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/textlog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/logging/visualstudiooutputlog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/lua/ghoul_lua.h
    ${PROJECT_SOURCE_DIR}/include/openspace/lua/lua_helper.h
    ${PROJECT_SOURCE_DIR}/include/openspace/lua/lua_helper.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/lua/lua_types.h
    ${PROJECT_SOURCE_DIR}/include/openspace/lua/luastate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/assert.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/base64.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/boolean.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/clipboard.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/constexpr.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/crc32.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/crc32.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/csvreader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/defer.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/dictionary.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/dictionaryjsonformatter.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/dictionaryluaformatter.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/easing.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/easing.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/exception.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/integration.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/integration.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/interpolator.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/interpolator.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/invariants.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/levmarqsolver.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/map.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/managedmemoryuniqueptr.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/memorypool.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/memorypool.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/objectmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/objectmanager.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/profiling.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/sharedmemory.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/stacktrace.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/stringconversion.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/stringhelper.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/supportmacros.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/templatefactory.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/templatefactory.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/thread.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/threadpool.h
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/threadpool.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/misc/process.h
    ${PROJECT_SOURCE_DIR}/include/openspace/mission/mission.h
    ${PROJECT_SOURCE_DIR}/include/openspace/mission/missionmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathcurves/avoidcollisioncurve.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathcurves/zoomoutoverviewcurve.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/keyframenavigator.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/navigationhandler.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/navigationstate.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/directmanipulation.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/dampenedvelocity.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/dampenedvelocity.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/idlemotion.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/joystickcamerastates.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/mousecamerastates.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/orbitalcamerastates.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/orbitalinputhandler.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/orbitalnavigator.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/scriptcamerastates.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/touchcamerastates.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator/websocketcamerastates.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/path.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathcurve.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathnavigator.h
    ${PROJECT_SOURCE_DIR}/include/openspace/navigation/waypoint.h
    ${PROJECT_SOURCE_DIR}/include/openspace/network/astrocast.h
    ${PROJECT_SOURCE_DIR}/include/openspace/network/messagestructures.h
    ${PROJECT_SOURCE_DIR}/include/openspace/network/messagestructureshelper.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/bufferbinding.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/bufferbinding.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/debugcontext.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/framebufferobject.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/ghoul_gl.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/openglstatecache.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/programobject.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/programobjectmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/renderdoc.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/shaderobject.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/shaderpreprocessor.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/texture.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/textureatlas.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/texturemanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/textureunit.h
    ${PROJECT_SOURCE_DIR}/include/openspace/opengl/uniformcache.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/numericalproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/numericalproperty.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/propertyowner.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/templateproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/templateproperty.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/list/doublelistproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/list/intlistproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/list/stringlistproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/dmat2property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/dmat3property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/dmat4property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/mat2property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/mat3property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/mat4property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/misc/listproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/misc/listproperty.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/misc/optionproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/misc/selectionproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/misc/stringproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/misc/triggerproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/boolproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/doubleproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/floatproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/intproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/longproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/shortproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/uintproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/ulongproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/scalar/ushortproperty.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/dvec2property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/dvec3property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/dvec4property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/ivec2property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/ivec3property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/ivec4property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/uvec2property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/uvec3property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/uvec4property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/vec2property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/vec3property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/properties/vector/vec4property.h
    ${PROJECT_SOURCE_DIR}/include/openspace/query/query.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/colormappingcomponent.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/dashboard.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/dashboarditem.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/dashboardtextitem.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/framebufferrenderer.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/deferredcaster.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/deferredcasterlistener.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/deferredcastermanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/fadeable.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/loadingscreen.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/luaconsole.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/helper.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/labelscomponent.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/raycasterlistener.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/raycastermanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/renderable.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/renderengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/screenspacerenderableframebuffer.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/screenspacerenderable.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/screenspacerenderabletext.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/shadowmapping.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/texturecomponent.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/transferfunction.h
    ${PROJECT_SOURCE_DIR}/include/openspace/rendering/volumeraycaster.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/asset.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/assetmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/jasset.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/lightsource.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/profile.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/rotation.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/scale.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/scene.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/sceneinitializer.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/scenegraphnode.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/timeframe.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scene/translation.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scripting/lualibrary.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scripting/scriptengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scripting/scriptscheduler.h
    ${PROJECT_SOURCE_DIR}/include/openspace/scripting/systemcapabilitiesbinding.h
    ${PROJECT_SOURCE_DIR}/include/openspace/systemcapabilities/generalcapabilitiescomponent.h
    ${PROJECT_SOURCE_DIR}/include/openspace/systemcapabilities/openglcapabilitiescomponent.h
    ${PROJECT_SOURCE_DIR}/include/openspace/systemcapabilities/systemcapabilities.h
    ${PROJECT_SOURCE_DIR}/include/openspace/systemcapabilities/systemcapabilities.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/systemcapabilities/systemcapabilitiescomponent.h
    ${PROJECT_SOURCE_DIR}/include/openspace/systemcapabilities/version.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/connection.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/connectionpool.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/jsonconverters.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/notificationlog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/serverinterface.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/server.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/actionkeybindtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/authorizationtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/camerapathtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/cameratopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/documentationtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/downloadeventtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/enginemodetopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/errorlogtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/eventtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/flightcontrollertopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/getpropertytopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/luascripttopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/missiontopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/profiletopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/propertytreetopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/sessionrecordingtopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/setpropertytopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/subscriptiontopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/timetopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/topic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/triggerpropertytopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/topic/topics/versiontopic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/blockplaneintersectiongeometry.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/boxgeometry.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/collisionhelper.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/concurrentjobmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/concurrentjobmanager.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/concurrentqueue.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/concurrentqueue.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/coordinateconversion.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/distanceconstants.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/distanceconversion.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/downloadeventengine.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/dynamicfilesequencedownloader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/ellipsoid.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/factorymanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/factorymanager.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/geodetic.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/httprequest.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/interpolator.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/interpolator.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/job.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/json_helper.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/json_helper.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/keys.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/memorymanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/mouse.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/openspacemodule.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/planegeometry.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/progressbar.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/resourcesynchronization.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/screenlog.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/sphere.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/spicemanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/syncable.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/syncbuffer.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/syncbuffer.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/syncdata.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/syncdata.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/task.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/taskloader.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/time.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/timeconstants.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/timeconversion.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/timeline.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/timeline.inl
    ${PROJECT_SOURCE_DIR}/include/openspace/util/timemanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/timerange.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/touch.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/tstring.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/universalhelpers.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/updatestructures.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/versionchecker.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/transformationmanager.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/threadpool.h
    ${PROJECT_SOURCE_DIR}/include/openspace/util/histogram.h

    openspace.cpp
    camera/camera.cpp
    camera/camerapose.cpp
    cmdparser/commandlinecommand.cpp
    cmdparser/commandlineparser.cpp
    cmdparser/multiplecommand.cpp
    cmdparser/singlecommand.cpp
    data/colormaploader.cpp
    data/csvloader.cpp
    data/dataloader.cpp
    data/datamapping.cpp
    data/speckloader.cpp
    documentation/core_registration.cpp
    documentation/documentation.cpp
    documentation/documentationengine.cpp
    documentation/verifier.cpp
    engine/configuration.cpp
    engine/downloadmanager.cpp
    engine/globals.cpp
    engine/globalscallbacks.cpp
    engine/logfactory.cpp
    engine/moduleengine.cpp
    engine/moduleengine_lua.inl
    engine/openspaceengine.cpp
    engine/openspaceengine_lua.inl
    engine/settings.cpp
    engine/syncengine.cpp
    events/event.cpp
    events/eventengine.cpp
    events/eventengine_lua.inl
    filesystem/cachemanager.cpp
    filesystem/file.cpp
    filesystem/filesystem.cpp
    filesystem/filesystem.linux.cpp
    filesystem/filesystem.windows.cpp
    font/font.cpp
    font/fonterrors.cpp
    font/fontmanager.cpp
    font/fontrenderer.cpp
    interaction/actionmanager.cpp
    interaction/actionmanager_lua.inl
    interaction/interactionhandler.cpp
    interaction/interactionmonitor.cpp
    interaction/joystickinputstate.cpp
    interaction/keybindingmanager.cpp
    interaction/keybindingmanager_lua.inl
    interaction/keyframerecordinghandler.cpp
    interaction/keyframerecordinghandler_lua.inl
    interaction/keyboardinputstate.cpp
    interaction/mouseinputstate.cpp
    interaction/sessionrecording.cpp
    interaction/sessionrecordinghandler.cpp
    interaction/sessionrecordinghandler_lua.inl
    interaction/touchinputstate.cpp
    interaction/touchmarker.cpp
    interaction/websocketinputstate.cpp
    interaction/tasks/convertrecformattask.cpp
    io/model/modelanimation.cpp
    io/model/modelgeometry.cpp
    io/model/modelmesh.cpp
    io/model/modelnode.cpp
    io/model/modelreader.cpp
    io/model/modelreaderassimp.cpp
    io/model/modelreaderbase.cpp
    io/model/modelreaderbinary.cpp
    io/socket/socket.cpp
    io/socket/tcpsocket.cpp
    io/socket/tcpsocketserver.cpp
    io/socket/websocket.cpp
    io/socket/websocketserver.cpp
    io/texture/texturereader.cpp
    io/texture/texturewriter.cpp
    io/volume/rawvolumereader.cpp
    logging/bufferlog.cpp
    logging/callbacklog.cpp
    logging/consolelog.cpp
    logging/htmllog.cpp
    logging/log.cpp
    logging/logmanager.cpp
    logging/textlog.cpp
    logging/visualstudiooutputlog.cpp
    lua/lua_helper.cpp
    lua/lua_types.cpp
    lua/luastate.cpp
    misc/assert.cpp
    misc/base64.cpp
    misc/clipboard.cpp
    misc/crc32.cpp
    misc/csvreader.cpp
    misc/dictionary.cpp
    misc/dictionaryjsonformatter.cpp
    misc/dictionaryluaformatter.cpp
    misc/easing.cpp
    misc/exception.cpp
    misc/interpolator.cpp
    misc/levmarqsolver.cpp
    misc/sharedmemory.cpp
    misc/stacktrace.cpp
    misc/stringhelper.cpp
    misc/templatefactory.cpp
    misc/thread.cpp
    misc/threadpool.cpp
    misc/process.cpp
    mission/mission.cpp
    mission/missionmanager.cpp
    mission/missionmanager_lua.inl
    navigation/pathcurves/avoidcollisioncurve.cpp
    navigation/pathcurves/zoomoutoverviewcurve.cpp
    navigation/keyframenavigator.cpp
    navigation/navigationhandler.cpp
    navigation/navigationhandler_lua.inl
    navigation/navigationstate.cpp
    navigation/orbitalnavigator/directmanipulation.cpp
    navigation/orbitalnavigator/idlemotion.cpp
    navigation/orbitalnavigator/joystickcamerastates.cpp
    navigation/orbitalnavigator/mousecamerastates.cpp
    navigation/orbitalnavigator/orbitalcamerastates.cpp
    navigation/orbitalnavigator/orbitalinputhandler.cpp
    navigation/orbitalnavigator/orbitalnavigator.cpp
    navigation/orbitalnavigator/orbitalnavigator_lua.inl
    navigation/orbitalnavigator/scriptcamerastates.cpp
    navigation/orbitalnavigator/touchcamerastates.cpp
    navigation/orbitalnavigator/websocketcamerastates.cpp
    navigation/path.cpp
    navigation/pathcurve.cpp
    navigation/pathnavigator.cpp
    navigation/pathnavigator_lua.inl
    navigation/waypoint.cpp
    network/messagestructures.cpp
    network/messagestructureshelper.cpp
    network/astrocast.cpp
    network/astrocast_lua.inl
    opengl/debugcontext.cpp
    opengl/framebufferobject.cpp
    opengl/openglstatecache.cpp
    opengl/programobject.cpp
    opengl/programobjectmanager.cpp
    opengl/renderdoc.cpp
    opengl/shaderobject.cpp
    opengl/shaderpreprocessor.cpp
    opengl/texture.cpp
    opengl/textureatlas.cpp
    opengl/texturemanager.cpp
    opengl/textureunit.cpp
    properties/property.cpp
    properties/propertyowner.cpp
    properties/list/doublelistproperty.cpp
    properties/list/intlistproperty.cpp
    properties/list/stringlistproperty.cpp
    properties/matrix/dmat2property.cpp
    properties/matrix/dmat3property.cpp
    properties/matrix/dmat4property.cpp
    properties/matrix/mat2property.cpp
    properties/matrix/mat3property.cpp
    properties/matrix/mat4property.cpp
    properties/misc/optionproperty.cpp
    properties/misc/selectionproperty.cpp
    properties/misc/stringproperty.cpp
    properties/misc/triggerproperty.cpp
    properties/scalar/boolproperty.cpp
    properties/scalar/doubleproperty.cpp
    properties/scalar/floatproperty.cpp
    properties/scalar/intproperty.cpp
    properties/scalar/longproperty.cpp
    properties/scalar/shortproperty.cpp
    properties/scalar/uintproperty.cpp
    properties/scalar/ulongproperty.cpp
    properties/scalar/ushortproperty.cpp
    properties/vector/dvec2property.cpp
    properties/vector/dvec3property.cpp
    properties/vector/dvec4property.cpp
    properties/vector/ivec2property.cpp
    properties/vector/ivec3property.cpp
    properties/vector/ivec4property.cpp
    properties/vector/uvec2property.cpp
    properties/vector/uvec3property.cpp
    properties/vector/uvec4property.cpp
    properties/vector/vec2property.cpp
    properties/vector/vec3property.cpp
    properties/vector/vec4property.cpp
    query/query.cpp
    rendering/colormappingcomponent.cpp
    rendering/dashboard.cpp
    rendering/dashboard_lua.inl
    rendering/dashboarditem.cpp
    rendering/dashboardtextitem.cpp
    rendering/framebufferrenderer.cpp
    rendering/deferredcaster.cpp
    rendering/deferredcastermanager.cpp
    rendering/fadeable.cpp
    rendering/helper.cpp
    rendering/labelscomponent.cpp
    rendering/loadingscreen.cpp
    rendering/luaconsole.cpp
    rendering/raycastermanager.cpp
    rendering/renderable.cpp
    rendering/renderengine.cpp
    rendering/renderengine_lua.inl
    rendering/screenspacerenderableframebuffer.cpp
    rendering/screenspacerenderable.cpp
    rendering/screenspacerenderabletext.cpp
    rendering/shadowmapping.cpp
    rendering/texturecomponent.cpp
    rendering/transferfunction.cpp
    rendering/volumeraycaster.cpp
    scene/asset.cpp
    scene/assetmanager.cpp
    scene/assetmanager_lua.inl
    scene/jasset.cpp
    scene/lightsource.cpp
    scene/profile.cpp
    scene/profile_lua.inl
    scene/rotation.cpp
    scene/scale.cpp
    scene/scene.cpp
    scene/scene_lua.inl
    scene/sceneinitializer.cpp
    scene/scenegraphnode.cpp
    scene/timeframe.cpp
    scene/translation.cpp
    scripting/lualibrary.cpp
    scripting/scriptengine.cpp
    scripting/scriptengine_lua.inl
    scripting/scriptscheduler.cpp
    scripting/scriptscheduler_lua.inl
    scripting/systemcapabilitiesbinding.cpp
    scripting/systemcapabilitiesbinding_lua.inl
    systemcapabilities/generalcapabilitiescomponent.cpp
    systemcapabilities/openglcapabilitiescomponent.cpp
    systemcapabilities/systemcapabilities.cpp
    systemcapabilities/systemcapabilitiescomponent.cpp
    systemcapabilities/version.cpp
    topic/connection.cpp
    topic/connectionpool.cpp
    topic/jsonconverters.cpp
    topic/notificationlog.cpp
    topic/serverinterface.cpp
    topic/server.cpp
    topic/topics/actionkeybindtopic.cpp
    topic/topics/authorizationtopic.cpp
    topic/topics/camerapathtopic.cpp
    topic/topics/cameratopic.cpp
    topic/topics/documentationtopic.cpp
    topic/topics/downloadeventtopic.cpp
    topic/topics/enginemodetopic.cpp
    topic/topics/errorlogtopic.cpp
    topic/topics/eventtopic.cpp
    topic/topics/flightcontrollertopic.cpp
    topic/topics/getpropertytopic.cpp
    topic/topics/luascripttopic.cpp
    topic/topics/missiontopic.cpp
    topic/topics/profiletopic.cpp
    topic/topics/propertytreetopic.cpp
    topic/topics/sessionrecordingtopic.cpp
    topic/topics/setpropertytopic.cpp
    topic/topics/subscriptiontopic.cpp
    topic/topics/timetopic.cpp
    topic/topics/topic.cpp
    topic/topics/triggerpropertytopic.cpp
    topic/topics/versiontopic.cpp
    util/blockplaneintersectiongeometry.cpp
    util/boxgeometry.cpp
    util/collisionhelper.cpp
    util/coordinateconversion.cpp
    util/distanceconversion.cpp
    util/downloadeventengine.cpp
    util/dynamicfilesequencedownloader.cpp
    util/ellipsoid.cpp
    util/factorymanager.cpp
    util/geodetic.cpp
    util/httprequest.cpp
    util/json_helper.cpp
    util/keys.cpp
    util/openspacemodule.cpp
    util/planegeometry.cpp
    util/progressbar.cpp
    util/resourcesynchronization.cpp
    util/screenlog.cpp
    util/sphere.cpp
    util/spicemanager.cpp
    util/spicemanager_lua.inl
    util/syncable.cpp
    util/syncbuffer.cpp
    util/tstring.cpp
    util/histogram.cpp
    util/task.cpp
    util/taskloader.cpp
    util/threadpool.cpp
    util/time.cpp
    util/timeconversion.cpp
    util/timeline.cpp
    util/timemanager.cpp
    util/time_lua.inl
    util/timerange.cpp
    util/touch.cpp
    util/transformationmanager.cpp
    util/universalhelpers.cpp
    util/versionchecker.cpp

    ${OPENSPACE_SHADERS}
)

if (GHOUL_HIGH_DEBUG_MODE)
  target_compile_definitions(openspace-core PUBLIC "GHL_DEBUG")
endif ()

if (GHOUL_LOGGING_ENABLE_TRACE)
  target_compile_definitions(openspace-core PUBLIC "GHOUL_LOGGING_ENABLE_TRACE")
endif ()

if (GHOUL_THROW_ON_ASSERT)
  target_compile_definitions(openspace-core PUBLIC "GHL_THROW_ON_ASSERT")
endif ()

target_include_directories(openspace-core
  PUBLIC
    # In order to be able to include openspace-core files
    ${PROJECT_SOURCE_DIR}/include
    # In order to be able to include the module_registration file
    ${CMAKE_BINARY_DIR}/_generated/include

  PRIVATE
    # In order to be able to include module files.  This is a temporary fix as this
    # introduces a dependency from the opnspace-core onto the modules
    ${PROJECT_SOURCE_DIR}
)

find_package(glm CONFIG REQUIRED)
find_package(lz4 CONFIG REQUIRED)
find_package(scn CONFIG REQUIRED)
find_package(tiny-process-library CONFIG REQUIRED)
find_package(zip CONFIG REQUIRED)
find_package(stbimage CONFIG REQUIRED)
find_package(OpenGL REQUIRED)
find_package(glbinding CONFIG REQUIRED)
find_package(unofficial-lua CONFIG REQUIRED)
find_package(assimp CONFIG REQUIRED)
find_package(Freetype REQUIRED)

target_link_libraries(
  openspace-core
  PUBLIC
    nlohmann_json::nlohmann_json
    "shlwapi"
    "WbemUuid.lib"
    "Ws2_32.lib"
    zip::zip
    glm::glm
    scn::scn
    unofficial::stbimage::stbimage-read-impl
    unofficial::stbimage::stbimage-write-impl
    OpenGL::GL
    glbinding::glbinding
    glbinding::glbinding-aux
    assimp::assimp
    spice::spice
    CURL::libcurl
    date::date
  PRIVATE
    lz4::lz4
    tiny-process-library::tiny-process-library
    unofficial::stbimage::stbimage
    Freetype::Freetype
)
if (TARGET unofficial::lua::lua)
  target_link_libraries(openspace-core PUBLIC unofficial::lua::lua)
else ()
  # Lua 5.4.7 exports an unnamespaced target, later versions do not
  target_link_libraries(openspace-core PUBLIC lua)
endif ()

if (CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
  find_library(INOTIFY_LIBRARIES inotify PATHS "/usr/local/lib")
  target_include_directories(openspace-core SYSTEM PRIVATE "/usr/local/include")
  target_link_libraries(openspace-core PRIVATE ${INOTIFY_LIBRARIES})
endif ()

# WebSocket++ is header only and ships no CMake package, so it is located through its
# include directory
find_path(WEBSOCKETPP_INCLUDE_DIR "websocketpp/version.hpp" REQUIRED)
target_include_directories(openspace-core SYSTEM PUBLIC $<BUILD_INTERFACE:${WEBSOCKETPP_INCLUDE_DIR}>)

# RenderDoc is a single vendored header included by the public ghoul/opengl/renderdoc.h
target_include_directories(
  openspace-core SYSTEM PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/ext/renderdoc>
)

if (TRACY_ENABLE)
  find_package(Tracy CONFIG REQUIRED)
  target_link_libraries(openspace-core PUBLIC Tracy::TracyClient)
  target_compile_definitions(openspace-core PUBLIC "TRACY_ENABLE")
endif ()

if (UNIX)
  # On Arch Linux, if you have the package `oneapi` installed, this influences including
  # the <exception> header and causes the header /usr/include/oneapi/tbb/task_group.h be
  # pulled in, causing a compile problem
  target_compile_definitions(
    openspace-core
      PUBLIC "_GLIBCXX_USE_TBB_PAR_BACKEND=0"
      PUBLIC "_PSTL_PAR_BACKEND_SERIAL"
  )
endif ()

# Place files into source groups
get_target_property(source_files openspace-core SOURCES)
foreach (file ${source_files})
  # Remove prefixes from the files
  set(original_file ${file})
  string(REPLACE "${PROJECT_SOURCE_DIR}/src/" "" file ${file})
  string(REPLACE "${PROJECT_SOURCE_DIR}/include/openspace/" "" file ${file})
  get_filename_component(directory ${file} DIRECTORY)
  if (NOT directory STREQUAL "")
    # Visual Studio wants '\' for the path separator
    string(REPLACE "/" "\\" directory ${directory})

    # It looks nicer if the directory starts with a capital letter
    # First extract the first letter, make it upper case and then reassemble the name
    string(SUBSTRING ${directory} 0 1 FIRST_LETTER)
    string(TOUPPER ${FIRST_LETTER} FIRST_LETTER)
    string(REGEX REPLACE "^.(.*)" "${FIRST_LETTER}\\1" directory "${directory}")

    source_group("${directory}" FILES ${original_file})
  else ()
    source_group("" FILES ${original_file})
  endif ()
endforeach ()


target_precompile_headers(openspace-core PRIVATE
  <openspace/json.h>
  <openspace/properties/numericalproperty.h>
  <openspace/format.h>
  <openspace/glm.h>
  <openspace/lua/lua_helper.h>
  <openspace/misc/assert.h>
  <openspace/misc/boolean.h>
  <openspace/misc/exception.h>
  <openspace/misc/invariants.h>
  <openspace/misc/profiling.h>
  <openspace/opengl/ghoul_gl.h>
  <algorithm>
  <any>
  <array>
  <deque>
  <filesystem>
  <fstream>
  <future>
  <map>
  <memory>
  <string>
  <utility>
  <variant>
  <vector>
)

add_dependencies(openspace-core run_codegen)

configure_file(
  ${PROJECT_SOURCE_DIR}/support/cmake/openspace_header.template
  ${CMAKE_BINARY_DIR}/_generated/include/openspace/openspace.h
  @ONLY
)

configure_file(
  ${PROJECT_SOURCE_DIR}/support/cmake/commit.template
  ${PROJECT_SOURCE_DIR}/COMMIT.md
  @ONLY
)

if (WIN32)
  target_link_libraries(openspace-core PUBLIC "pdh")
endif ()

set_openspace_compile_settings(openspace-core)
