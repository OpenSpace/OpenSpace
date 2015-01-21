#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014                                                                    #
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


#set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

set(SOURCE_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(HEADER_ROOT_DIR ${CMAKE_SOURCE_DIR}/include)

file(GLOB CONFIGURATION_SOURCE ${SOURCE_ROOT_DIR}/configuration/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${CONFIGURATION_SOURCE})
file(GLOB CONFIGURATION_HEADER ${HEADER_ROOT_DIR}/openspace/configuration/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${CONFIGURATION_HEADER})
source_group(Configuration FILES ${CONFIGURATION_SOURCE} ${CONFIGURATION_HEADER})

file(GLOB ABUFFER_SOURCE ${SOURCE_ROOT_DIR}/abuffer/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${ABUFFER_SOURCE})
file(GLOB ABUFFER_HEADER ${HEADER_ROOT_DIR}/openspace/abuffer/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${ABUFFER_HEADER})
source_group(ABuffer FILES ${ABUFFER_SOURCE} ${ABUFFER_HEADER})

file(GLOB ENGINE_SOURCE ${SOURCE_ROOT_DIR}/engine/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${ENGINE_SOURCE})
file(GLOB ENGINE_HEADER ${HEADER_ROOT_DIR}/openspace/engine/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${ENGINE_HEADER})
source_group(Engine FILES ${ENGINE_SOURCE} ${ENGINE_HEADER})

file(GLOB GUI_SOURCE ${SOURCE_ROOT_DIR}/gui/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${GUI_SOURCE})
file(GLOB GUI_HEADER ${HEADER_ROOT_DIR}/openspace/gui/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${GUI_HEADER})
source_group(GUI FILES ${GUI_SOURCE} ${GUI_HEADER})

file(GLOB INTERACTION_SOURCE ${SOURCE_ROOT_DIR}/interaction/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${INTERACTION_SOURCE})
file(GLOB INTERACTION_HEADER ${HEADER_ROOT_DIR}/openspace/interaction/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${INTERACTION_HEADER})
source_group(Interaction FILES ${INTERACTION_SOURCE} ${INTERACTION_HEADER})

file(GLOB INTERACTION_EXTERNALCONTROL_SOURCE ${SOURCE_ROOT_DIR}/interaction/externalcontrol/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${INTERACTION_EXTERNALCONTROL_SOURCE})
file(GLOB INTERACTION_EXTERNALCONTROL_HEADER ${HEADER_ROOT_DIR}/openspace/interaction/externalcontrol/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${INTERACTION_EXTERNALCONTROL_HEADER})
source_group(Interaction\\ExternalControl FILES ${INTERACTION_EXTERNALCONTROL_SOURCE} ${INTERACTION_EXTERNALCONTROL_HEADER})

file(GLOB PROPERTY_SOURCE_CPP ${SOURCE_ROOT_DIR}/properties/*.cpp)
file(GLOB PROPERTY_SOURCE_C ${SOURCE_ROOT_DIR}/properties/*.c)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${PROPERTY_SOURCE_CPP} ${PROPERTY_SOURCE_C})
file(GLOB PROPERTY_HEADER ${HEADER_ROOT_DIR}/openspace/properties/*.h ${HEADER_ROOT_DIR}/openspace/properties/*.inl)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${PROPERTY_HEADER})
source_group(Properties FILES ${PROPERTY_SOURCE_CPP} ${PROPERTY_SOURCE_C} ${PROPERTY_HEADER})

file(GLOB QUERY_SOURCE_CPP ${SOURCE_ROOT_DIR}/query/*.cpp)
file(GLOB QUERY_SOURCE_C ${SOURCE_ROOT_DIR}/query/*.c)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${QUERY_SOURCE_CPP} ${QUERY_SOURCE_C})
file(GLOB QUERY_HEADER ${HEADER_ROOT_DIR}/openspace/query/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${QUERY_HEADER})
source_group(Query FILES ${QUERY_SOURCE_CPP} ${QUERY_SOURCE_C} ${QUERY_HEADER})

file(GLOB RENDERING_SOURCE ${SOURCE_ROOT_DIR}/rendering/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_SOURCE})
file(GLOB RENDERING_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_HEADER})
source_group(Rendering FILES ${RENDERING_SOURCE} ${RENDERING_HEADER})

file(GLOB RENDERING_PLANETS_SOURCE ${SOURCE_ROOT_DIR}/rendering/planets/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_PLANETS_SOURCE})
file(GLOB RENDERING_PLANETS_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/planets/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_PLANETS_HEADER})
source_group(Rendering\\Planets FILES ${RENDERING_PLANETS_SOURCE} ${RENDERING_PLANETS_HEADER})

file(GLOB RENDERING_STARS_SOURCE ${SOURCE_ROOT_DIR}/rendering/stars/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_STARS_SOURCE})
file(GLOB RENDERING_STARS_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/stars/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_STARS_HEADER})
source_group(Rendering\\Stars FILES ${RENDERING_STARS_SOURCE} ${RENDERING_STARS_HEADER})

file(GLOB RENDERING_MODEL_SOURCE ${SOURCE_ROOT_DIR}/rendering/model/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_MODEL_SOURCE})
file(GLOB RENDERING_MODEL_HEADER ${HEADER_ROOT_DIR}/openspace/rendering/model/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_MODEL_HEADER})
source_group(Rendering\\Model FILES ${RENDERING_MODEL_SOURCE} ${RENDERING_MODEL_HEADER})

file(GLOB SCENEGRAPH_SOURCE ${SOURCE_ROOT_DIR}/scenegraph/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${SCENEGRAPH_SOURCE})
file(GLOB SCENEGRAPH_HEADER ${HEADER_ROOT_DIR}/openspace/scenegraph/*.h ${HEADER_ROOT_DIR}/openspace/scenegraph/*.inl)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${SCENEGRAPH_HEADER})
source_group(SceneGraph FILES ${SCENEGRAPH_SOURCE} ${SCENEGRAPH_HEADER})

file(GLOB SCRIPTING_SOURCE ${SOURCE_ROOT_DIR}/scripting/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${SCRIPTING_SOURCE})
file(GLOB SCRIPTING_HEADER ${HEADER_ROOT_DIR}/openspace/scripting/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${SCRIPTING_HEADER})
source_group(Scripting FILES ${SCRIPTING_SOURCE} ${SCRIPTING_HEADER})

file(GLOB UTIL_SOURCE ${SOURCE_ROOT_DIR}/util/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${UTIL_SOURCE})
file(GLOB UTIL_HEADER ${HEADER_ROOT_DIR}/openspace/util/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${UTIL_HEADER})
source_group(Util FILES ${UTIL_SOURCE} ${UTIL_HEADER})

file(GLOB FLARE_SOURCE ${SOURCE_ROOT_DIR}/flare/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${FLARE_SOURCE})
file(GLOB FLARE_HEADER ${HEADER_ROOT_DIR}/openspace/flare/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${FLARE_HEADER})
source_group(Flare FILES ${FLARE_SOURCE} ${FLARE_HEADER})

file(GLOB INTERFACE_SOURCE ${SOURCE_ROOT_DIR}/interface/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${INTERFACE_SOURCE})
file(GLOB INTERFACE_HEADER ${HEADER_ROOT_DIR}/openspace/interface/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${INTERFACE_HEADER})
source_group(Interface FILES ${INTERFACE_SOURCE} ${INTERFACE_HEADER})


include_directories(${HEADER_ROOT_DIR})
include_directories(${GHOUL_ROOT_DIR}/ext/boost)

include_directories(${CMAKE_SOURCE_DIR}/ext/imgui)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.h)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.cpp)
source_group(ext\\imgui FILES ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.h ${CMAKE_SOURCE_DIR}/ext/imgui/imgui.cpp)

if (APPLE)
	add_definitions(-D__APPLE__)
	set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++ ${CMAKE_CXX_FLAGS}")
endif (APPLE)
if (UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -ggdb")
endif (UNIX AND NOT APPLE)


include_directories("${HEADER_ROOT_DIR}")

add_executable(OpenSpace ${SOURCE_ROOT_DIR}/main.cpp ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
target_link_libraries(OpenSpace ${DEPENDENT_LIBS})

if (NOT UNIX)
    cotire(OpenSpace)
endif ()
GhoulCopySharedLibraries(OpenSpace)

add_subdirectory(tests)

