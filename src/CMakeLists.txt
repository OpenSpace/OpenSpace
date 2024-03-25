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

include(${PROJECT_SOURCE_DIR}/support/cmake/set_openspace_compile_settings.cmake)

set(OPENSPACE_SOURCE
  openspace.cpp
  camera/camera.cpp
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
  interaction/actionmanager.cpp
  interaction/actionmanager_lua.inl
  interaction/camerainteractionstates.cpp
  interaction/interactionmonitor.cpp
  interaction/mouseinputstate.cpp
  interaction/joystickinputstate.cpp
  interaction/joystickcamerastates.cpp
  interaction/keybindingmanager.cpp
  interaction/keybindingmanager_lua.inl
  interaction/keyboardinputstate.cpp
  interaction/mousecamerastates.cpp
  interaction/scriptcamerastates.cpp
  interaction/sessionrecording.cpp
  interaction/sessionrecording_lua.inl
  interaction/websocketinputstate.cpp
  interaction/websocketcamerastates.cpp
  interaction/tasks/convertrecfileversiontask.cpp
  interaction/tasks/convertrecformattask.cpp
  mission/mission.cpp
  mission/missionmanager.cpp
  mission/missionmanager_lua.inl
  navigation/pathcurves/avoidcollisioncurve.cpp
  navigation/pathcurves/zoomoutoverviewcurve.cpp
  navigation/keyframenavigator.cpp
  navigation/navigationhandler.cpp
  navigation/navigationhandler_lua.inl
  navigation/navigationstate.cpp
  navigation/orbitalnavigator.cpp
  navigation/orbitalnavigator_lua.inl
  navigation/path.cpp
  navigation/pathcurve.cpp
  navigation/pathnavigator.cpp
  navigation/pathnavigator_lua.inl
  navigation/waypoint.cpp
  network/messagestructureshelper.cpp
  network/parallelconnection.cpp
  network/parallelpeer.cpp
  network/parallelpeer_lua.inl
  properties/optionproperty.cpp
  properties/property.cpp
  properties/propertyowner.cpp
  properties/selectionproperty.cpp
  properties/stringproperty.cpp
  properties/triggerproperty.cpp
  properties/list/doublelistproperty.cpp
  properties/list/intlistproperty.cpp
  properties/list/stringlistproperty.cpp
  properties/matrix/dmat2property.cpp
  properties/matrix/dmat3property.cpp
  properties/matrix/dmat4property.cpp
  properties/matrix/mat2property.cpp
  properties/matrix/mat3property.cpp
  properties/matrix/mat4property.cpp
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
  rendering/screenspacerenderable.cpp
  rendering/texturecomponent.cpp
  rendering/transferfunction.cpp
  rendering/volumeraycaster.cpp
  scene/asset.cpp
  scene/assetmanager.cpp
  scene/assetmanager_lua.inl
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
  util/blockplaneintersectiongeometry.cpp
  util/boxgeometry.cpp
  util/collisionhelper.cpp
  util/coordinateconversion.cpp
  util/distanceconversion.cpp
  util/factorymanager.cpp
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
)

if (APPLE)
  set(OPENSPACE_SOURCE
    ${OPENSPACE_SOURCE}
    interaction/touchbar.mm
  )
  set_source_files_properties(
    interaction/touchbar.mm
    PROPERTIES SKIP_PRECOMPILE_HEADERS ON
  )
endif ()
set(OPENSPACE_HEADER
  ${PROJECT_SOURCE_DIR}/include/openspace/json.h
  ${PROJECT_SOURCE_DIR}/include/openspace/camera/camera.h
  ${PROJECT_SOURCE_DIR}/include/openspace/camera/camerapose.h
  ${PROJECT_SOURCE_DIR}/include/openspace/data/csvloader.h
  ${PROJECT_SOURCE_DIR}/include/openspace/data/dataloader.h
  ${PROJECT_SOURCE_DIR}/include/openspace/data/datamapping.h
  ${PROJECT_SOURCE_DIR}/include/openspace/data/speckloader.h
  ${PROJECT_SOURCE_DIR}/include/openspace/documentation/core_registration.h
  ${PROJECT_SOURCE_DIR}/include/openspace/documentation/documentation.h
  ${PROJECT_SOURCE_DIR}/include/openspace/documentation/documentationengine.h
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
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/action.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/actionmanager.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/delayedvariable.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/delayedvariable.inl
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/camerainteractionstates.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/mouseinputstate.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/interactionmonitor.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/interpolator.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/interpolator.inl
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/joystickinputstate.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/joystickcamerastates.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/keybindingmanager.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/keyboardinputstate.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/mousecamerastates.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/scriptcamerastates.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/sessionrecording.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/sessionrecording.inl
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/websocketinputstate.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/websocketcamerastates.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/tasks/convertrecfileversiontask.h
  ${PROJECT_SOURCE_DIR}/include/openspace/interaction/tasks/convertrecformattask.h
  ${PROJECT_SOURCE_DIR}/include/openspace/mission/mission.h
  ${PROJECT_SOURCE_DIR}/include/openspace/mission/missionmanager.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathcurves/avoidcollisioncurve.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathcurves/zoomoutoverviewcurve.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/keyframenavigator.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/navigationhandler.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/navigationstate.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/orbitalnavigator.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/path.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathcurve.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/pathnavigator.h
  ${PROJECT_SOURCE_DIR}/include/openspace/navigation/waypoint.h
  ${PROJECT_SOURCE_DIR}/include/openspace/network/parallelconnection.h
  ${PROJECT_SOURCE_DIR}/include/openspace/network/parallelpeer.h
  ${PROJECT_SOURCE_DIR}/include/openspace/network/messagestructures.h
  ${PROJECT_SOURCE_DIR}/include/openspace/network/messagestructureshelper.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/listproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/listproperty.inl
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/numericalproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/numericalproperty.inl
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/optionproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/property.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/propertyowner.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/selectionproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/stringproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/templateproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/templateproperty.inl
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/triggerproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/list/doublelistproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/list/intlistproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/list/stringlistproperty.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/dmat2property.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/dmat3property.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/dmat4property.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/mat2property.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/mat3property.h
  ${PROJECT_SOURCE_DIR}/include/openspace/properties/matrix/mat4property.h
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
  ${PROJECT_SOURCE_DIR}/include/openspace/rendering/screenspacerenderable.h
  ${PROJECT_SOURCE_DIR}/include/openspace/rendering/texturecomponent.h
  ${PROJECT_SOURCE_DIR}/include/openspace/rendering/transferfunction.h
  ${PROJECT_SOURCE_DIR}/include/openspace/rendering/volumeraycaster.h
  ${PROJECT_SOURCE_DIR}/include/openspace/scene/asset.h
  ${PROJECT_SOURCE_DIR}/include/openspace/scene/assetmanager.h
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
  ${PROJECT_SOURCE_DIR}/include/openspace/util/factorymanager.h
  ${PROJECT_SOURCE_DIR}/include/openspace/util/factorymanager.inl
  ${PROJECT_SOURCE_DIR}/include/openspace/util/httprequest.h
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
)

if (APPLE)
  set(OPENSPACE_HEADER
    ${OPENSPACE_HEADER}
    ${PROJECT_SOURCE_DIR}/include/openspace/interaction/touchbar.h
  )
endif ()

# Place files into source groups
foreach (file ${OPENSPACE_SOURCE} ${OPENSPACE_HEADER})
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

##########################################################################################
# Create OpenSpace target
##########################################################################################
add_library(openspace-core STATIC ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
target_include_directories(openspace-core SYSTEM
  PUBLIC
    # In order to use the date library
    ${PROJECT_SOURCE_DIR}/ext/date/include
    # In order to use the nlohmann JSON library
    ${PROJECT_SOURCE_DIR}/ext
)

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

target_precompile_headers(openspace-core PRIVATE
  <openspace/json.h>
  <openspace/properties/numericalproperty.h>
  [["ghoul/format.h"]]
  [["ghoul/glm.h"]]
  [["ghoul/misc/assert.h"]]
  [["ghoul/misc/boolean.h"]]
  [["ghoul/misc/exception.h"]]
  [["ghoul/misc/invariants.h"]]
  [["ghoul/misc/profiling.h"]]
  [["ghoul/opengl/ghoul_gl.h"]]
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
  @ONLY IMMEDIATE
)

configure_file(
  ${PROJECT_SOURCE_DIR}/support/cmake/commit.template
  ${PROJECT_SOURCE_DIR}/COMMIT.md
  @ONLY IMMEDIATE
)

if (APPLE)
  target_link_libraries(openspace-core INTERFACE external-system-apple)
endif ()

set_openspace_compile_settings(openspace-core)
target_link_libraries(openspace-core PUBLIC Ghoul spice external-curl)

# Just in case, create the bin directory
add_custom_command(
  TARGET openspace-core
  PRE_BUILD COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)
