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

set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_EXT_DIR})

if (APPLE )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -stdlib=libc++")
endif ()

#########################################################################################
# External Third-party software
#########################################################################################

# Ghoul
add_definitions(-DGHL_DEBUG)
set(GHOUL_ROOT_DIR "${OPENSPACE_EXT_DIR}/ghoul")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${GHOUL_ROOT_DIR}/ext)
include_directories("${GHOUL_ROOT_DIR}/include")
add_subdirectory(${GHOUL_ROOT_DIR})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} Ghoul)

# SGCT (TODO: write cmake script)
#set(SGCT_ROOT_DIR "$ENV{SGCT_ROOT_DIR}")
#set(SGCT_ROOT_DIR "C:/Program Files/SGCT/SGCT_2.0.7")
#set(SGCT_ROOT_DIR "C:/Program Files (x86)/SGCT/SGCT_2.0.5_x86")
#set(SGCT_ROOT_DIR "C:/Program Files/SGCT/SGCT_2.0.7")

#add_definitions(-D__WIN32__)
add_definitions(-DGLEW_STATIC)
include_directories("${OPENSPACE_EXT_DIR}/sgct/include")
#set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} ${SGCT_ROOT_DOR}/bin/lib/msvc11)
set(DEPENDENT_LIBS
    ${DEPENDENT_LIBS}
#    "Ws2_32.lib"
#    optimized "${SGCT_ROOT_DIR}/lib/msvc11_x64/sgct.lib"
#    debug "${SGCT_ROOT_DIR}/lib/msvc11_x64/sgctd.lib"
	${OPENSPACE_EXT_DIR}/sgct/lib/libsgct_cpp11.a
   )

#add_subdirectory(${SGCT_ROOT_DIR})
#set(DEPENDENT_LIBS ${DEPENDENT_LIBS} SGCT)

# GLM
set(GLM_ROOT_DIR "${GHOUL_ROOT_DIR}/ext/glm")
find_package(GLM REQUIRED)
include_directories(${GLM_INCLUDE_DIRS})

# OpenGL
#find_package(OpenGL REQUIRED)
#include_directories(${OPENGL_INCLUDE_DIRS})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${OPENGL_LIBRARIES})

# GLEW
#find_package(GLEW REQUIRED)
#include_directories(${GLEW_INCLUDE_DIRECTORIES})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${GLEW_LIBRARIES})

# Lua
set(LUA_ROOT_DIR "${GHOUL_ROOT_DIR}/ext/lua")
include_directories("${LUA_ROOT_DIR}/include")
#add_subdirectory(${LUA_ROOT_DIR})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} Lua)

# Spice
set(SPICE_ROOT_DIR "${OPENSPACE_EXT_DIR}/spice")
include_directories("${SPICE_ROOT_DIR}/include")
add_subdirectory(${SPICE_ROOT_DIR})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} Spice)

# Threads
find_library(PTHREADS_LIB pthread)
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${PTHREADS_LIB})

if (APPLE)
    include_directories(/Developer/Headers/FlatCarbon)
    find_library(CARBON_LIBRARY Carbon)
    find_library(COCOA_LIBRARY Cocoa)
    find_library(APP_SERVICES_LIBRARY ApplicationServices)
    find_library(APPKIT_LIBRARY AppKit)
    find_library(IOKIT_LIBRARY IOKit)
    mark_as_advanced(CARBON_LIBRARY COCOA_LIBRARY APP_SERVICES_LIBRARY IOKIT_LIBRARY)
    set(DEPENDENT_LIBS
    	${DEPENDENT_LIBS}
    	${CARBON_LIBRARY}
    	${APPKIT_LIBRARY}
    	${COCOA_LIBRARY}
    	${APP_SERVICES_LIBRARY}
    	${IOKIT_LIBRARY}
    	)
endif ()

#########################################################################################
# Executable
#########################################################################################

add_subdirectory(src)
