#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2015                                                               #
#                                                                                       #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this  #
# software and associated documentation files (the "Software"), to deal in the Software #
# without restriction, including without limitation the rights to use, copy, modify,    #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    #
# permit persons to whom the Software is furnished to do so, subject to the following   #
# conditions:                                                                           #
#                                                                                       #
# The above copyright notice and this permission notice shall be included in all copies #
# or substantial portions of the Software.                                              #
#                                                                                       #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         #
#########################################################################################

set(OPENSPACE_SOURCE
    ${OPENSPACE_BASE_DIR}/src/openspace.cpp
    ${OPENSPACE_BASE_DIR}/src/documentation/documentation.cpp
    ${OPENSPACE_BASE_DIR}/src/documentation/documentationengine.cpp
    ${OPENSPACE_BASE_DIR}/src/documentation/verifier.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/configurationmanager.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/configurationmanager_doc.inl
    ${OPENSPACE_BASE_DIR}/src/engine/downloadmanager.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/logfactory.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/moduleengine.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/moduleengine_lua.inl
    ${OPENSPACE_BASE_DIR}/src/engine/openspaceengine.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/settingsengine.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/wrapper/sgctwindowwrapper.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/wrapper/windowwrapper.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/controller.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/deviceidentifier.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/interactionhandler.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/interactionmode.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/interactionhandler_lua.inl
    ${OPENSPACE_BASE_DIR}/src/interaction/keyboardcontroller.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/luaconsole.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/luaconsole_lua.inl
    ${OPENSPACE_BASE_DIR}/src/interaction/mousecontroller.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/externalcontrol/externalconnectioncontroller.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/externalcontrol/externalcontrol.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/externalcontrol/joystickexternalcontrol.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/externalcontrol/keyboardexternalcontrol.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/externalcontrol/mouseexternalcontrol.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/externalcontrol/pythonexternalcontrol.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/externalcontrol/randomexternalcontrol.cpp
    ${OPENSPACE_BASE_DIR}/src/network/networkengine.cpp
    ${OPENSPACE_BASE_DIR}/src/network/parallelconnection.cpp
    ${OPENSPACE_BASE_DIR}/src/network/parallelconnection_lua.inl
    ${OPENSPACE_BASE_DIR}/src/performance/performancemeasurement.cpp
    ${OPENSPACE_BASE_DIR}/src/performance/performancelayout.cpp
    ${OPENSPACE_BASE_DIR}/src/performance/performancemanager.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/matrixproperty.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/optionproperty.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/property.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/propertyowner.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/scalarproperty.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/selectionproperty.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/stringproperty.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/triggerproperty.cpp
    ${OPENSPACE_BASE_DIR}/src/properties/vectorproperty.cpp
    ${OPENSPACE_BASE_DIR}/src/query/query.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/abufferrenderer.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/framebufferrenderer.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/raycastermanager.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/renderable.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/renderengine.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/renderengine_lua.inl
    ${OPENSPACE_BASE_DIR}/src/rendering/screenspacerenderable.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/transferfunction.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/ephemeris.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/rotation.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/scale.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/scene.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/scene_lua.inl
    ${OPENSPACE_BASE_DIR}/src/scene/scenegraph.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/scenegraphnode.cpp
    ${OPENSPACE_BASE_DIR}/src/scripting/lualibrary.cpp
    ${OPENSPACE_BASE_DIR}/src/scripting/scriptengine.cpp
    ${OPENSPACE_BASE_DIR}/src/scripting/scriptengine_lua.inl
    ${OPENSPACE_BASE_DIR}/src/util/blockplaneintersectiongeometry.cpp
    ${OPENSPACE_BASE_DIR}/src/util/boxgeometry.cpp
    ${OPENSPACE_BASE_DIR}/src/util/camera.cpp
    ${OPENSPACE_BASE_DIR}/src/util/factorymanager.cpp
    ${OPENSPACE_BASE_DIR}/src/util/keys.cpp
    ${OPENSPACE_BASE_DIR}/src/util/openspacemodule.cpp
    ${OPENSPACE_BASE_DIR}/src/util/powerscaledcoordinate.cpp
    ${OPENSPACE_BASE_DIR}/src/util/powerscaledscalar.cpp
    ${OPENSPACE_BASE_DIR}/src/util/powerscaledsphere.cpp
    ${OPENSPACE_BASE_DIR}/src/util/progressbar.cpp
    ${OPENSPACE_BASE_DIR}/src/util/screenlog.cpp
    ${OPENSPACE_BASE_DIR}/src/util/spicemanager.cpp
    ${OPENSPACE_BASE_DIR}/src/util/spicemanager_lua.inl
    ${OPENSPACE_BASE_DIR}/src/util/syncbuffer.cpp
    ${OPENSPACE_BASE_DIR}/src/util/histogram.cpp
    ${OPENSPACE_BASE_DIR}/src/util/time.cpp
    ${OPENSPACE_BASE_DIR}/src/util/time_lua.inl
    ${OPENSPACE_BASE_DIR}/src/util/transformationmanager.cpp
)

set(OPENSPACE_HEADER
    ${OPENSPACE_BASE_DIR}/include/openspace/openspace.h
    ${OPENSPACE_BASE_DIR}/include/openspace/documentation/documentation.h
    ${OPENSPACE_BASE_DIR}/include/openspace/documentation/documentationengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/documentation/verifier.h
    ${OPENSPACE_BASE_DIR}/include/openspace/documentation/verifier.inl
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/configurationmanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/downloadmanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/logfactory.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/moduleengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/openspaceengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/settingsengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/wrapper/sgctwindowwrapper.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/wrapper/windowwrapper.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/controller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/deviceidentifier.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/interactionhandler.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/interactionmode.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/keyboardcontroller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/luaconsole.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/mousecontroller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/externalconnectioncontroller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/externalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/joystickexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/keyboardexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/mouseexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/pythonexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/randomexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/network/networkengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/network/parallelconnection.h
    ${OPENSPACE_BASE_DIR}/include/openspace/network/messagestructures.h
    ${OPENSPACE_BASE_DIR}/include/openspace/performance/performancemeasurement.h
    ${OPENSPACE_BASE_DIR}/include/openspace/performance/performancelayout.h
    ${OPENSPACE_BASE_DIR}/include/openspace/performance/performancemanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/matrixproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/numericalproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/numericalproperty.inl
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/optionproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/property.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/propertydelegate.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/propertydelegate.inl
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/propertyowner.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/scalarproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/selectionproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/stringproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/templateproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/templateproperty.inl
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/triggerproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/properties/vectorproperty.h
    ${OPENSPACE_BASE_DIR}/include/openspace/query/query.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/abufferrenderer.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/framebufferrenderer.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/raycasterlistener.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/raycastermanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/renderable.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/renderer.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/renderengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/volume.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/screenspacerenderable.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/volumeraycaster.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/transferfunction.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/ephemeris.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/rotation.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/scale.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/scene.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/scenegraph.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/scenegraphnode.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scripting/lualibrary.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scripting/script_helper.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scripting/scriptengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/blockplaneintersectiongeometry.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/boxgeometry.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/camera.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/factorymanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/factorymanager.inl
    ${OPENSPACE_BASE_DIR}/include/openspace/util/keys.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/mouse.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/openspacemodule.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/powerscaledcoordinate.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/powerscaledscalar.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/powerscaledsphere.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/progressbar.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/screenlog.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/spicemanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/syncbuffer.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/time.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/updatestructures.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/transformationmanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/histogram.h
)

# Place files into source groups
foreach (file ${OPENSPACE_SOURCE} ${OPENSPACE_HEADER})
    # Remove prefixes from the files
    set(original_file ${file})
    string(REPLACE "${OPENSPACE_BASE_DIR}/src/" "" file ${file})
    string(REPLACE "${OPENSPACE_BASE_DIR}/include/openspace/" "" file ${file})
    get_filename_component(directory ${file} DIRECTORY)
    if (NOT directory STREQUAL "")
        string(REPLACE "/" "\\" directory ${directory})
        string(SUBSTRING ${directory} 0 1 FIRST_LETTER)
        string(TOUPPER ${FIRST_LETTER} FIRST_LETTER)
        string(REGEX REPLACE "^.(.*)" "${FIRST_LETTER}\\1" directory "${directory}")
        source_group("${directory}" FILES ${original_file})
    else ()
        source_group("" FILES ${original_file})
    endif ()
endforeach () 
