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


#########################################################################################
# General Settings
#########################################################################################

cmake_minimum_required (VERSION 2.8) 
project (OpenSpace)

if (WIN32)
    SET(CMAKE_EXE_LINKER_FLAGS /NODEFAULTLIB:\"LIBCMTD.lib\")
endif ()

set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_EXT_DIR})

include(cotire)

# Make sure a build type is set. Default is Debug.
if(NOT CMAKE_BUILD_TYPE)
	set( CMAKE_BUILD_TYPE Debug CACHE STRING
       "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel." 
	   FORCE )
endif(NOT CMAKE_BUILD_TYPE)

if (APPLE )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -stdlib=libc++")
endif ()

#########################################################################################
# External Third-party software
#########################################################################################

# Ghoul
set(GHOUL_ROOT_DIR "${OPENSPACE_EXT_DIR}/ghoul")
include_directories("${GHOUL_ROOT_DIR}/include")
set(BOOST_ROOT "${GHOUL_ROOT_DIR}/ext/boost")
add_subdirectory(${GHOUL_ROOT_DIR})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} Ghoul)

# Add ghoul ext
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${GHOUL_ROOT_DIR}/ext)

# Boost
include_directories(${GHOUL_ROOT_DIR}/ext/boost)

# SGCT
find_package(SGCT REQUIRED)
include_directories(${SGCT_INCLUDE_DIRECTORIES})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${SGCT_LIBRARIES})

# GLM
set(GLM_ROOT_DIR "${GHOUL_ROOT_DIR}/ext/glm")
find_package(GLM REQUIRED)
add_definitions(-DGLM_SWIZZLE)
include_directories(${GLM_INCLUDE_DIRS})

# GLEW
find_package(GLEW REQUIRED)
include_directories(${GLEW_INCLUDE_DIRECTORIES})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${GLEW_LIBRARIES})

# Lua
set(LUA_ROOT_DIR "${GHOUL_ROOT_DIR}/ext/lua")
include_directories("${LUA_ROOT_DIR}/include")

# Spice
set(SPICE_ROOT_DIR "${OPENSPACE_EXT_DIR}/spice")
include_directories("${SPICE_ROOT_DIR}/include")
add_subdirectory(${SPICE_ROOT_DIR})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} Spice)

# OpenCL
find_package(OpenCL)
if(OPENCL_FOUND)
    add_definitions(-DGHL_OPENCL)
    include_directories(${OPENCL_INCLUDE_DIRS})
    set(GHOUL_DEPENDENCIES ${GHOUL_DEPENDENCIES} ${OPENCL_LIBRARIES})
endif(OPENCL_FOUND)

# Kameleon
option(KAMELEON_LIBRARY_ONLY "Build with Kameleon as library only" ON)
if(WIN32)
    option(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
else(WIN32)
    option(BUILD_SHARED_LIBS "Build Shared Libraries" ON)
endif(WIN32)
option(KAMELEON_USE_HDF5 "Kameleon use HDF5" OFF)
set(KAMELEON_ROOT_DIR ${OPENSPACE_EXT_DIR}/kameleon)
set(KAMELEON_INCLUDES ${KAMELEON_ROOT_DIR}/src)
add_subdirectory(${KAMELEON_ROOT_DIR})
include_directories(${KAMELEON_INCLUDES})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ccmc)

if (APPLE)
    include_directories(/Developer/Headers/FlatCarbon)
    find_library(CARBON_LIBRARY Carbon)
    find_library(COCOA_LIBRARY Cocoa)
    find_library(APP_SERVICES_LIBRARY ApplicationServices)
    mark_as_advanced(CARBON_LIBRARY COCOA_LIBRARY APP_SERVICES_LIBRARY)
    set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${CARBON_LIBRARY} ${COCOA_LIBRARY} ${APP_SERVICES_LIBRARY})
endif ()

#########################################################################################
# Executable
#########################################################################################

add_subdirectory(src)

#add_subdirectory(gui)