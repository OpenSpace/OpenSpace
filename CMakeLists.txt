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
    SET(CMAKE_EXE_LINKER_FLAGS /NODEFAULTLIB:\"LIBCMTD.lib;LIBCMT.lib\")
endif ()

set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")
set(OPENSPACE_CMAKE_EXT_DIR "${OPENSPACE_BASE_DIR}/support/cmake")
set(OPENSPACE_BINARY_DIR ${OPENSPACE_BASE_DIR}/bin/openspace)
set(OPENSPACE_LIBRARY_DIR ${OPENSPACE_BASE_DIR}/bin/lib)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_CMAKE_EXT_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${OPENSPACE_LIBRARY_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${OPENSPACE_LIBRARY_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OPENSPACE_BINARY_DIR})


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
if (GHOUL_USE_FREEIMAGE)
    add_definitions(-DGHOUL_USE_FREEIMAGE)
endif ()
if (GHOUL_USE_DEVIL)
    add_definitions(-DGHOUL_USE_DEVIL)
endif ()

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
find_package(Spice REQUIRED)
include_directories(${SPICE_INCLUDE_DIRS})
set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${SPICE_LIBRARIES})

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

#########################################################################################
# File Fetch
#########################################################################################
option(DOWNLOAD_FILES "Download large OpenSpace data on configure" OFF)
if(DOWNLOAD_FILES)
    function(DownloadFile FILE_PATH FILE_URL)
        set(FILE_MD5 ${ARGV2})
        if(NOT EXISTS "${FILE_PATH}")
            file(DOWNLOAD ${FILE_URL} ${FILE_PATH} INACTIVITY_TIMEOUT 10 SHOW_PROGRESS)
        endif()
        file(MD5 ${FILE_PATH} MD5_RESULT)
        if( NOT "${FILE_MD5}" STREQUAL "" )
            string(COMPARE EQUAL ${MD5_RESULT} ${FILE_MD5} SUCCESS)
            if(NOT ${SUCCESS})
                message(WARNING "${FILE_PATH} not matching MD5")
            endif()
        endif()
    endfunction(DownloadFile)

    function(NewHorizonDownload FILE_PATH )
        set(MD5 "${ARGV1}")
        DownloadFile("${OPENSPACE_BASE_DIR}/openspace-data/spice/JupiterNhKernels/${FILE_PATH}"
                     "http://naif.jpl.nasa.gov/pub/naif/pds/data/nh-j_p_ss-spice-6-v1.0/nhsp_1000/data/${FILE_PATH}" ${MD5})
    endfunction(NewHorizonDownload)

    NewHorizonDownload("ck/merged_nhpc_2006_v011.bc")
    NewHorizonDownload("ck/merged_nhpc_2007_v006.bc")
    NewHorizonDownload("fk/nh_v200.tf")
    NewHorizonDownload("ik/nh_lorri_v100.ti")
    NewHorizonDownload("sclk/new_horizons_413.tsc" "6f7a87c21cb3e37835261ed745f34d4a")
    NewHorizonDownload("spk/de413.bsp")
    NewHorizonDownload("spk/jup260.bsp")
    NewHorizonDownload("spk/nh_nep_ura_000.bsp")
    NewHorizonDownload("spk/nh_recon_e2j_v1.bsp")
    NewHorizonDownload("spk/nh_recon_j2sep07_prelimv1.bsp")
    NewHorizonDownload("spk/sb_2002jf56_2.bsp")
endif(DOWNLOAD_FILES)
