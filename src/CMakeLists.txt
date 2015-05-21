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
    ${OPENSPACE_BASE_DIR}/src/abuffer/abuffer.cpp
    ${OPENSPACE_BASE_DIR}/src/abuffer/abufferdynamic.cpp
    ${OPENSPACE_BASE_DIR}/src/abuffer/abufferfixed.cpp
    ${OPENSPACE_BASE_DIR}/src/abuffer/abufferframebuffer.cpp
    ${OPENSPACE_BASE_DIR}/src/abuffer/abuffersinglelinked.cpp
    ${OPENSPACE_BASE_DIR}/src/abuffer/abuffervisualizer.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/configurationmanager.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/logfactory.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/moduleengine.cpp
    ${OPENSPACE_BASE_DIR}/src/engine/openspaceengine.cpp
    ${OPENSPACE_BASE_DIR}/src/gui/gui.cpp
    ${OPENSPACE_BASE_DIR}/src/gui/guicomponent.cpp
    ${OPENSPACE_BASE_DIR}/src/gui/guihelpcomponent.cpp
    ${OPENSPACE_BASE_DIR}/src/gui/guiorigincomponent.cpp
    ${OPENSPACE_BASE_DIR}/src/gui/guiperformancecomponent.cpp
    ${OPENSPACE_BASE_DIR}/src/gui/guipropertycomponent.cpp
    ${OPENSPACE_BASE_DIR}/src/gui/guitimecomponent.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/controller.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/deviceidentifier.cpp
    ${OPENSPACE_BASE_DIR}/src/interaction/interactionhandler.cpp
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
    ${OPENSPACE_BASE_DIR}/src/rendering/renderable.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/renderengine.cpp
    ${OPENSPACE_BASE_DIR}/src/rendering/renderengine_lua.inl
    ${OPENSPACE_BASE_DIR}/src/scene/ephemeris.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/scene.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/scene_lua.inl
    ${OPENSPACE_BASE_DIR}/src/scene/scenegraph.cpp
    ${OPENSPACE_BASE_DIR}/src/scene/scenegraphnode.cpp
    ${OPENSPACE_BASE_DIR}/src/scripting/scriptengine.cpp
    ${OPENSPACE_BASE_DIR}/src/scripting/scriptengine_lua.inl
    ${OPENSPACE_BASE_DIR}/src/util/camera.cpp
    ${OPENSPACE_BASE_DIR}/src/util/factorymanager.cpp
    ${OPENSPACE_BASE_DIR}/src/util/kameleonwrapper.cpp
    ${OPENSPACE_BASE_DIR}/src/util/openspacemodule.cpp
    ${OPENSPACE_BASE_DIR}/src/util/powerscaledcoordinate.cpp
    ${OPENSPACE_BASE_DIR}/src/util/powerscaledscalar.cpp
    ${OPENSPACE_BASE_DIR}/src/util/powerscaledsphere.cpp
    ${OPENSPACE_BASE_DIR}/src/util/progressbar.cpp
    ${OPENSPACE_BASE_DIR}/src/util/screenlog.cpp
    ${OPENSPACE_BASE_DIR}/src/util/spicemanager.cpp
    ${OPENSPACE_BASE_DIR}/src/util/syncbuffer.cpp
    ${OPENSPACE_BASE_DIR}/src/util/time.cpp
    ${OPENSPACE_BASE_DIR}/src/util/time_lua.inl
)

set(OPENSPACE_MAIN
    ${OPENSPACE_BASE_DIR}/src/main.cpp
)

set(OPENSPACE_HEADER
    ${OPENSPACE_BASE_DIR}/include/openspace/abuffer/abuffer.h
    ${OPENSPACE_BASE_DIR}/include/openspace/abuffer/abufferdynamic.h
    ${OPENSPACE_BASE_DIR}/include/openspace/abuffer/abufferfixed.h
    ${OPENSPACE_BASE_DIR}/include/openspace/abuffer/abufferframebuffer.h
    ${OPENSPACE_BASE_DIR}/include/openspace/abuffer/abuffersinglelinked.h
    ${OPENSPACE_BASE_DIR}/include/openspace/abuffer/abuffervisualizer.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/configurationmanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/logfactory.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/moduleengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/engine/openspaceengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/gui/gui.h
    ${OPENSPACE_BASE_DIR}/include/openspace/gui/guicomponent.h
    ${OPENSPACE_BASE_DIR}/include/openspace/gui/guihelpcomponent.h
    ${OPENSPACE_BASE_DIR}/include/openspace/gui/guiorigincomponent.h
    ${OPENSPACE_BASE_DIR}/include/openspace/gui/guiperformancecomponent.h
    ${OPENSPACE_BASE_DIR}/include/openspace/gui/guipropertycomponent.h
    ${OPENSPACE_BASE_DIR}/include/openspace/gui/guitimecomponent.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/controller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/deviceidentifier.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/interactionhandler.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/keyboardcontroller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/luaconsole.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/mouse.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/mousecontroller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/externalconnectioncontroller.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/externalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/joystickexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/keyboardexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/mouseexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/pythonexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/interaction/externalcontrol/randomexternalcontrol.h
    ${OPENSPACE_BASE_DIR}/include/openspace/network/networkengine.h
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
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/renderable.h
    ${OPENSPACE_BASE_DIR}/include/openspace/rendering/renderengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/ephemeris.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/scene.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/scenegraph.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scene/scenegraphnode.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scripting/script_helper.h
    ${OPENSPACE_BASE_DIR}/include/openspace/scripting/scriptengine.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/camera.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/constants.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/factorymanager.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/factorymanager.inl
    ${OPENSPACE_BASE_DIR}/include/openspace/util/kameleonwrapper.h
    ${OPENSPACE_BASE_DIR}/include/openspace/util/keys.h
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
    ${OPENSPACE_BASE_DIR}/include/openspace/version.h
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

# #set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

# set(SOURCE_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
# set(HEADER_ROOT_DIR ${CMAKE_SOURCE_DIR}/include)

# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${HEADER_ROOT_DIR}/openspace/version.h)

# file(GLOB CONFIGURATION_SOURCE ${SOURCE_ROOT_DIR}/configuration/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${CONFIGURATION_SOURCE})
# file(GLOB CONFIGURATION_HEADER ${HEADER_ROOT_DIR}/openspace/configuration/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${CONFIGURATION_HEADER})
# source_group(Configuration FILES ${CONFIGURATION_SOURCE} ${CONFIGURATION_HEADER})

# file(GLOB ABUFFER_SOURCE ${SOURCE_ROOT_DIR}/abuffer/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${ABUFFER_SOURCE})
# file(GLOB ABUFFER_HEADER ${HEADER_ROOT_DIR}/openspace/abuffer/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${ABUFFER_HEADER})
# source_group(ABuffer FILES ${ABUFFER_SOURCE} ${ABUFFER_HEADER})

# file(GLOB ENGINE_SOURCE ${SOURCE_ROOT_DIR}/engine/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${ENGINE_SOURCE})
# file(GLOB ENGINE_HEADER ${HEADER_ROOT_DIR}/openspace/engine/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${ENGINE_HEADER})
# source_group(Engine FILES ${ENGINE_SOURCE} ${ENGINE_HEADER})

# file(GLOB NETWORK_SOURCE ${SOURCE_ROOT_DIR}/network/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${NETWORK_SOURCE})
# file(GLOB NETWORK_HEADER ${HEADER_ROOT_DIR}/openspace/network/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${NETWORK_HEADER})
# source_group(Network FILES ${NETWORK_SOURCE} ${NETWORK_HEADER})

# file(GLOB GUI_SOURCE ${SOURCE_ROOT_DIR}/gui/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${GUI_SOURCE})
# file(GLOB GUI_HEADER ${HEADER_ROOT_DIR}/openspace/gui/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${GUI_HEADER})
# source_group(GUI FILES ${GUI_SOURCE} ${GUI_HEADER})

# file(GLOB INTERACTION_SOURCE ${SOURCE_ROOT_DIR}/interaction/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${INTERACTION_SOURCE})
# file(GLOB INTERACTION_HEADER ${HEADER_ROOT_DIR}/openspace/interaction/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${INTERACTION_HEADER})
# source_group(Interaction FILES ${INTERACTION_SOURCE} ${INTERACTION_HEADER})

# file(GLOB INTERACTION_EXTERNALCONTROL_SOURCE ${SOURCE_ROOT_DIR}/interaction/externalcontrol/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${INTERACTION_EXTERNALCONTROL_SOURCE})
# file(GLOB INTERACTION_EXTERNALCONTROL_HEADER ${HEADER_ROOT_DIR}/openspace/interaction/externalcontrol/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${INTERACTION_EXTERNALCONTROL_HEADER})
# source_group(Interaction\\ExternalControl FILES ${INTERACTION_EXTERNALCONTROL_SOURCE} ${INTERACTION_EXTERNALCONTROL_HEADER})

# file(GLOB PROPERTY_SOURCE_CPP ${SOURCE_ROOT_DIR}/properties/*.cpp)
# file(GLOB PROPERTY_SOURCE_C ${SOURCE_ROOT_DIR}/properties/*.c)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${PROPERTY_SOURCE_CPP} ${PROPERTY_SOURCE_C})
# file(GLOB PROPERTY_HEADER ${HEADER_ROOT_DIR}/openspace/properties/*.h ${HEADER_ROOT_DIR}/openspace/properties/*.inl)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${PROPERTY_HEADER})
# source_group(Properties FILES ${PROPERTY_SOURCE_CPP} ${PROPERTY_SOURCE_C} ${PROPERTY_HEADER})

# file(GLOB QUERY_SOURCE_CPP ${SOURCE_ROOT_DIR}/query/*.cpp)
# file(GLOB QUERY_SOURCE_C ${SOURCE_ROOT_DIR}/query/*.c)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${QUERY_SOURCE_CPP} ${QUERY_SOURCE_C})
# file(GLOB QUERY_HEADER ${HEADER_ROOT_DIR}/openspace/query/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${QUERY_HEADER})
# source_group(Query FILES ${QUERY_SOURCE_CPP} ${QUERY_SOURCE_C} ${QUERY_HEADER})

# file(GLOB RENDERING_SOURCE ${SOURCE_ROOT_DIR}/rendering/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_SOURCE})
# file(GLOB RENDERING_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_HEADER})
# source_group(Rendering FILES ${RENDERING_SOURCE} ${RENDERING_HEADER})

# file(GLOB RENDERING_PLANETS_SOURCE ${SOURCE_ROOT_DIR}/rendering/planets/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_PLANETS_SOURCE})
# file(GLOB RENDERING_PLANETS_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/planets/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_PLANETS_HEADER})
# source_group(Rendering\\Planets FILES ${RENDERING_PLANETS_SOURCE} ${RENDERING_PLANETS_HEADER})

# file(GLOB RENDERING_STARS_SOURCE ${SOURCE_ROOT_DIR}/rendering/stars/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_STARS_SOURCE})
# file(GLOB RENDERING_STARS_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/stars/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_STARS_HEADER})
# source_group(Rendering\\Stars FILES ${RENDERING_STARS_SOURCE} ${RENDERING_STARS_HEADER})

# file(GLOB RENDERING_MODEL_SOURCE ${SOURCE_ROOT_DIR}/rendering/model/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_MODEL_SOURCE})
# file(GLOB RENDERING_MODEL_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/model/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_MODEL_HEADER})
# source_group(Rendering\\Model FILES ${RENDERING_MODEL_SOURCE} ${RENDERING_MODEL_HEADER})

# file(GLOB SCENE_SOURCE ${SOURCE_ROOT_DIR}/scene/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${SCENE_SOURCE})
# file(GLOB SCENE_HEADER ${HEADER_ROOT_DIR}/openspace/scene/*.h ${HEADER_ROOT_DIR}/openspace/scene/*.inl)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${SCENE_HEADER})
# source_group(Scene FILES ${SCENE_SOURCE} ${SCENE_HEADER})

# file(GLOB SCRIPTING_SOURCE ${SOURCE_ROOT_DIR}/scripting/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${SCRIPTING_SOURCE})
# file(GLOB SCRIPTING_HEADER ${HEADER_ROOT_DIR}/openspace/scripting/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${SCRIPTING_HEADER})
# source_group(Scripting FILES ${SCRIPTING_SOURCE} ${SCRIPTING_HEADER})

# file(GLOB UTIL_SOURCE ${SOURCE_ROOT_DIR}/util/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${UTIL_SOURCE})
# file(GLOB UTIL_HEADER ${HEADER_ROOT_DIR}/openspace/util/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${UTIL_HEADER})
# source_group(Util FILES ${UTIL_SOURCE} ${UTIL_HEADER})

# file(GLOB FLARE_SOURCE ${SOURCE_ROOT_DIR}/flare/*.cpp)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${FLARE_SOURCE})
# file(GLOB FLARE_HEADER ${HEADER_ROOT_DIR}/openspace/flare/*.h)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${FLARE_HEADER})
# source_group(Flare FILES ${FLARE_SOURCE} ${FLARE_HEADER})

# include_directories(${HEADER_ROOT_DIR})
# include_directories(${GHOUL_ROOT_DIR}/ext/boost)

# include_directories(${CMAKE_SOURCE_DIR}/ext/imgui)
# set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.h)
# set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.cpp)
# source_group(ext\\imgui FILES ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.h ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.cpp)


# set(DEPENDENT_LIBS ${DEPENDENT_LIBS} openspace-module-base openspace-module-newhorizons openspace-module-volume)

# include_directories("${HEADER_ROOT_DIR}")
# include_directories("${OPENSPACE_BASE_DIR}")

# add_executable(OpenSpace ${SOURCE_ROOT_DIR}/main.cpp ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
# target_link_libraries(OpenSpace ${DEPENDENT_LIBS})

# option(OPENSPACE_HAVE_TESTS "Activate the OpenSpace unit tests" ON)
# if (OPENSPACE_HAVE_TESTS)
#     add_definitions(-DOPENSPACE_HAVE_TESTS)

#     set(OPENSPACE_TEST_DIR ${OPENSPACE_BASE_DIR}/tests)
    
#     include_directories("${GHOUL_ROOT_DIR}/ext/gtest/include")
#     include_directories("${GHOUL_ROOT_DIR}/include")
#     include_directories("${OPENSPACE_TEST_DIR}")
#     file(GLOB_RECURSE OPENSPACE_TEST_FILES ${OPENSPACE_BASE_DIR}/tests/*.inl)
    
#     source_group(Tests FILES ${OPENSPACE_TEST_FILES})
    
#     add_executable(OpenSpaceTest ${OPENSPACE_BASE_DIR}/tests/main.cpp ${OPENSPACE_TEST_FILES} ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
#     target_link_libraries(OpenSpaceTest gtest ${DEPENDENT_LIBS})

# endif (OPENSPACE_HAVE_TESTS)

# #if (NOT UNIX)
#     #cotire(OpenSpace)
# #endif ()
# GhoulCopySharedLibraries(OpenSpace)
