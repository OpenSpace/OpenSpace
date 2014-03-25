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
set(HEADER_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})

file(GLOB MAIN_SOURCE ${SOURCE_ROOT_DIR}/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${MAIN_SOURCE})
file(GLOB MAIN_HEADER ${SOURCE_ROOT_DIR}/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${MAIN_HEADER})
source_group(Main FILES ${MAIN_SOURCE} ${MAIN_HEADER})

file(GLOB CONFIGURATION_SOURCE ${SOURCE_ROOT_DIR}/configuration/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${CONFIGURATION_SOURCE})
file(GLOB CONFIGURATION_HEADER ${HEADER_ROOT_DIR}/configuration/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${CONFIGURATION_HEADER})
source_group(Configuration FILES ${CONFIGURATION_SOURCE} ${CONFIGURATION_HEADER})

file(GLOB ENGINE_SOURCE ${SOURCE_ROOT_DIR}/engine/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${ENGINE_SOURCE})
file(GLOB ENGINE_HEADER ${HEADER_ROOT_DIR}/engine/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${ENGINE_HEADER})
source_group(Engine FILES ${ENGINE_SOURCE} ${ENGINE_HEADER})

file(GLOB INTERACTION_SOURCE ${SOURCE_ROOT_DIR}/interaction/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${INTERACTION_SOURCE})
file(GLOB INTERACTION_HEADER ${HEADER_ROOT_DIR}/interaction/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${INTERACTION_HEADER})
source_group(Interaction FILES ${INTERACTION_SOURCE} ${INTERACTION_HEADER})

file(GLOB INTERACTION_EXTERNALCONTROL_SOURCE ${SOURCE_ROOT_DIR}/interaction/externalcontrol/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${INTERACTION_EXTERNALCONTROL_SOURCE})
file(GLOB INTERACTION_EXTERNALCONTROL_HEADER ${HEADER_ROOT_DIR}/interaction/externalcontrol/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${INTERACTION_EXTERNALCONTROL_HEADER})
source_group(Interaction\\ExternalControl FILES ${INTERACTION_EXTERNALCONTROL_SOURCE} ${INTERACTION_EXTERNALCONTROL_HEADER})

file(GLOB QUERY_SOURCE_CPP ${SOURCE_ROOT_DIR}/query/*.cpp)
file(GLOB QUERY_SOURCE_C ${SOURCE_ROOT_DIR}/query/*.c)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${QUERY_SOURCE_CPP} ${QUERY_SOURCE_C})
file(GLOB QUERY_HEADER ${HEADER_ROOT_DIR}/query/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${QUERY_HEADER})
source_group(Query FILES ${QUERY_SOURCE_CPP} ${QUERY_SOURCE_C} ${QUERY_HEADER})

file(GLOB RENDERING_SOURCE ${SOURCE_ROOT_DIR}/rendering/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${RENDERING_SOURCE})
file(GLOB RENDERING_HEADER ${HEADER_ROOT_DIR}/rendering/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${RENDERING_HEADER})
source_group(Rendering FILES ${RENDERING_SOURCE} ${RENDERING_HEADER})

file(GLOB SCENEGRAPH_SOURCE ${SOURCE_ROOT_DIR}/scenegraph/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${SCENEGRAPH_SOURCE})
file(GLOB SCENEGRAPH_HEADER ${HEADER_ROOT_DIR}/scenegraph/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${SCENEGRAPH_HEADER})
source_group(SceneGraph FILES ${SCENEGRAPH_SOURCE} ${SCENEGRAPH_HEADER})

file(GLOB UTIL_SOURCE ${SOURCE_ROOT_DIR}/util/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${UTIL_SOURCE})
file(GLOB UTIL_HEADER ${HEADER_ROOT_DIR}/util/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${UTIL_HEADER})
source_group(Util FILES ${UTIL_SOURCE} ${UTIL_HEADER})

file(GLOB FLARE_SOURCE ${SOURCE_ROOT_DIR}/flare/*.cpp)
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${FLARE_SOURCE})
file(GLOB FLARE_HEADER ${HEADER_ROOT_DIR}/flare/*.h)
set(OPENSPACE_HEADER ${OPENSPACE_HEADER} ${FLARE_HEADER})
source_group(Flare FILES ${FLARE_SOURCE} ${FLARE_HEADER})

include_directories(${HEADER_ROOT_DIR})

# hack:
set(OPENSPACE_SOURCE ${OPENSPACE_SOURCE} ${OPENSPACE_EXT_DIR}/tinythread.cpp)
include_directories(${GHOUL_ROOT_DIR}/ext/boost)

find_package(OpenCL REQUIRED)
include_directories(${OPENCL_INCLUDE_DIRS})


if (APPLE)
    find_library(FRAMEWORK_IOKit
        NAMES IOKit
        PATHS ${CMAKE_OSX_SYSROOT}/System/Library
        PATH_SUFFIXES Frameworks
        NO_DEFAULT_PATH
    )
    find_library(FRAMEWORK_CoreVideo
        NAMES CoreVideo
        PATHS ${CMAKE_OSX_SYSROOT}/System/Library
        PATH_SUFFIXES Frameworks
        NO_DEFAULT_PATH
    )
    find_library(FRAMEWORK_Cocoa
        NAMES Cocoa
        PATHS ${CMAKE_OSX_SYSROOT}/System/Library
        PATH_SUFFIXES Frameworks
        NO_DEFAULT_PATH
    )
    set(SGCT_DEPENDENCIES ${SGCT_DEPENDENCIES} ${FRAMEWORK_IOKit} ${FRAMEWORK_CoreVideo} ${FRAMEWORK_Cocoa})
endif (APPLE)

if (APPLE)
	add_definitions(-D__APPLE__)
	set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++ ${CMAKE_CXX_FLAGS}")
    if(NOT CMAKE_GENERATOR STREQUAL Xcode)
        message(WARNING "You are not using Xcode, OpenSpace assumes that working directory path to top level is \"../../..\"")
    endif(NOT CMAKE_GENERATOR STREQUAL Xcode)
endif (APPLE)
if (UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -ggdb")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/bin/")
endif (UNIX AND NOT APPLE)


add_executable(OpenSpace ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
target_link_libraries(OpenSpace ${DEPENDENT_LIBS})
