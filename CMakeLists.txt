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

cmake_minimum_required (VERSION 3.0)

# Remove MinSizeRel build option
set(CMAKE_CONFIGURATION_TYPES Debug Release RelWithDebInfo CACHE TYPE INTERNAL FORCE)
mark_as_advanced(CMAKE_CONFIGURATION_TYPES)
mark_as_advanced(CMAKE_INSTALL_PREFIX)

project (OpenSpace)
message(STATUS "Generating OpenSpace project")

#########################################################################################
# General Settings
#########################################################################################

set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")
set(OPENSPACE_CMAKE_EXT_DIR "${OPENSPACE_BASE_DIR}/support/cmake")
set(OPENSPACE_BINARY_DIR ${OPENSPACE_BASE_DIR}/bin/openspace)
set(OPENSPACE_LIBRARY_DIR ${OPENSPACE_BASE_DIR}/bin/lib)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_CMAKE_EXT_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${OPENSPACE_LIBRARY_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${OPENSPACE_LIBRARY_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OPENSPACE_BINARY_DIR})

#############################
# Including the source files (with source groups already defined)
#############################
include(src/CMakeLists.txt)

#############################
# Declare the application
#############################
add_library(libOpenSpace STATIC ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_BASE_DIR}/include)

add_executable(OpenSpace ${OPENSPACE_MAIN})
target_include_directories(OpenSpace PUBLIC ${OPENSPACE_BASE_DIR}/include)
target_link_libraries(OpenSpace libOpenSpace)

#############################
# Compile settings
#############################

if (MSVC)
    target_compile_options(libOpenSpace PUBLIC "/MP" "/wd4201" "/wd4127")
    set_target_properties(OpenSpace PROPERTIES LINK_FLAGS "/NODEFAULTLIB:\"LIBCMTD.lib;LIBCMT.lib\"")
elseif (APPLE)
    target_compile_definitions(libOpenSpace PUBLIC "__APPLE__")
    target_compile_options(libOpenSpace PUBLIC "-std=c++11" "-stdlib=libc++")

    target_include_directories(libOpenSpace "/Developer/Headers/FlatCarbon")
    find_library(COREFOUNDATION_LIBRARY CoreFoundation)
    find_library(CARBON_LIBRARY Carbon)
    find_library(COCOA_LIBRARY Carbon)
    find_library(APP_SERVICES_LIBRARY ApplicationServices)
    mark_as_advanced(CARBON_LIBRARY COCOA_LIBRARY APP_SERVICES_LIBRARY)
    target_link_libraries(libOpenSpace
        ${CARBON_LIBRARY}
        ${COREFOUNDATION_LIBRARY}
        ${COCOA_LIBRARY}
        ${APP_SERVICES_LIBRARY}
    )
elseif (UNIX)
    target_compile_definitions(libOpenSpace PUBLIC "-std=c++0x -ggdb")
endif ()

#############################
# Dependencies
#############################

# Ghoul
add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul)
target_link_libraries(libOpenSpace Ghoul)
get_property(GHOUL_INCLUDE_DIR TARGET Ghoul PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
target_include_directories(libOpenSpace PUBLIC ${GHOUL_INCLUDE_DIR})
get_property(GHOUL_DEFINITIONS TARGET Ghoul PROPERTY INTERFACE_COMPILE_DEFINITIONS)
target_compile_definitions(libOpenSpace PUBLIC ${GHOUL_DEFINITIONS})

# # SGCT
find_package(SGCT REQUIRED)
target_include_directories(libOpenSpace SYSTEM PUBLIC ${SGCT_INCLUDE_DIRECTORIES})
target_link_libraries(libOpenSpace ${SGCT_LIBRARIES})
if (UNIX AND (NOT APPLE))
    target_link_libraries(libOpenSpace Xcursor Xinerama)
endif ()

# Spice
set(SPICE_ROOT_DIR "${OPENSPACE_EXT_DIR}/spice")
find_package(Spice REQUIRED)
target_include_directories(libOpenSpace SYSTEM PUBLIC ${SPICE_INCLUDE_DIRS})
target_link_libraries(libOpenSpace ${SPICE_LIBRARIES})

# # Kameleon
option(KAMELEON_LIBRARY_ONLY "Build with Kameleon as library only" ON)
if (WIN32)
    option(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
else ()
    option(BUILD_SHARED_LIBS "Build Shared Libraries" ON)
endif ()
mark_as_advanced(BUILD_SHARED_LIBS) # Change to set instead of option
option(KAMELEON_USE_HDF5 "Kameleon use HDF5" OFF)
set(KAMELEON_ROOT_DIR ${OPENSPACE_EXT_DIR}/kameleon)
set(KAMELEON_INCLUDES ${KAMELEON_ROOT_DIR}/src)
set(BOOST_ROOT "${OPENSPACE_EXT_DIR}/ghoul/ext/boost")
add_subdirectory(${KAMELEON_ROOT_DIR})
target_include_directories(libOpenSpace SYSTEM PUBLIC ${KAMELEON_INCLUDES})
target_link_libraries(libOpenSpace ccmc)

# Imgui
add_subdirectory(${OPENSPACE_EXT_DIR}/imgui)
get_property(IMGUI_INCLUDE_DIR TARGET Imgui PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
target_link_libraries(libOpenSpace Imgui)
target_include_directories(libOpenSpace PUBLIC ${IMGUI_INCLUDE_DIR})

#############################
# Other applications
#############################
option(BUILD_GUI_APPLICATIONS "Build GUI Applications" OFF)
if (BUILD_GUI_APPLICATIONS)
    add_subdirectory(gui)
endif ()

option(OPENSPACE_HAVE_TESTS "Activate the OpenSpace unit tests" ON)
if (OPENSPACE_HAVE_TESTS)
    file(GLOB_RECURSE OPENSPACE_TEST_FILES ${OPENSPACE_BASE_DIR}/tests/*.inl)

    add_executable(OpenSpaceTest ${OPENSPACE_BASE_DIR}/tests/main.cpp ${OPENSPACE_TEST_FILES})
    target_include_directories(OpenSpaceTest PUBLIC "${OPENSPACE_BASE_DIR}/include")
    target_link_libraries(OpenSpaceTest gtest libOpenSpace)


    # set(OPENSPACE_TEST_DIR ${OPENSPACE_BASE_DIR}/tests)
    
    # include_directories("${GHOUL_ROOT_DIR}/ext/gtest/include")
    # include_directories("${GHOUL_ROOT_DIR}/include")
    # include_directories("${OPENSPACE_TEST_DIR}")
    # file(GLOB_RECURSE OPENSPACE_TEST_FILES ${OPENSPACE_BASE_DIR}/tests/*.inl)
    
    # source_group(Tests FILES ${OPENSPACE_TEST_FILES})
    

endif (OPENSPACE_HAVE_TESTS)

#############################
# Modules
#############################
add_subdirectory(modules/base)
add_subdirectory(modules/newhorizons)
add_subdirectory(modules/volume)

target_link_libraries(libOpenSpace openspace-module-base openspace-module-newhorizons openspace-module-volume)



# set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_EXT_DIR}/ghoul/ext)

# #set(GHOUL_ROOT_DIR "${OPENSPACE_EXT_DIR}/ghoul")
# include_directories("${GHOUL_ROOT_DIR}/include")
# include_directories("${GHOUL_ROOT_DIR}/ext/tinyobjloader")
# set(BOOST_ROOT "${GHOUL_ROOT_DIR}/ext/boost")
# add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul)
# set(DEPENDENT_LIBS ${DEPENDENT_LIBS} Ghoul)
# if (GHOUL_USE_FREEIMAGE)
#     add_definitions(-DGHOUL_USE_FREEIMAGE)
# endif ()
# if (GHOUL_USE_DEVIL)
#     add_definitions(-DGHOUL_USE_DEVIL)
# endif ()

# # Add ghoul ext
# set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${GHOUL_ROOT_DIR}/ext)

# # Boost
# include_directories(${GHOUL_ROOT_DIR}/ext/boost)

# # SGCT
# find_package(SGCT REQUIRED)
# include_directories(${SGCT_INCLUDE_DIRECTORIES})
# set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${SGCT_LIBRARIES})
# if (UNIX AND (NOT APPLE))
#     set(DEPENDENT_LIBS ${DEPENDENT_LIBS} Xcursor Xinerama)
# endif ()

# # GLM
# set(GLM_ROOT_DIR "${GHOUL_ROOT_DIR}/ext/glm")
# find_package(GLM REQUIRED)
# add_definitions(-DGLM_SWIZZLE)
# include_directories(${GLM_INCLUDE_DIRS})

# # GLEW
# find_package(GLEW REQUIRED)
# include_directories(${GLEW_INCLUDE_DIRECTORIES})
# set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${GLEW_LIBRARIES})

# # Lua
# set(LUA_ROOT_DIR "${GHOUL_ROOT_DIR}/ext/lua")
# include_directories("${LUA_ROOT_DIR}/src")

# # Spice
# set(SPICE_ROOT_DIR "${OPENSPACE_EXT_DIR}/spice")
# find_package(Spice REQUIRED)
# include_directories(${SPICE_INCLUDE_DIRS})
# set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${SPICE_LIBRARIES})

# # Kameleon
# option(KAMELEON_LIBRARY_ONLY "Build with Kameleon as library only" ON)
# if(WIN32)
#     option(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
# else(WIN32)
#     option(BUILD_SHARED_LIBS "Build Shared Libraries" ON)
# endif(WIN32)
# option(KAMELEON_USE_HDF5 "Kameleon use HDF5" OFF)
# set(KAMELEON_ROOT_DIR ${OPENSPACE_EXT_DIR}/kameleon)
# set(KAMELEON_INCLUDES ${KAMELEON_ROOT_DIR}/src)
# add_subdirectory(${KAMELEON_ROOT_DIR})
# include_directories(${KAMELEON_INCLUDES})
# set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ccmc)

# if (APPLE)
#     include_directories(/Developer/Headers/FlatCarbon)
#     find_library(CARBON_LIBRARY Carbon)
#     find_library(COCOA_LIBRARY Cocoa)
#     find_library(APP_SERVICES_LIBRARY ApplicationServices)
#     mark_as_advanced(CARBON_LIBRARY COCOA_LIBRARY APP_SERVICES_LIBRARY)
#     set(DEPENDENT_LIBS ${DEPENDENT_LIBS} ${CARBON_LIBRARY} ${COCOA_LIBRARY} ${APP_SERVICES_LIBRARY})
# endif ()

# #########################################################################################
# # Executable
# #########################################################################################

# add_subdirectory(src)
# add_subdirectory(modules/base)
# add_subdirectory(modules/newhorizons)
# add_subdirectory(modules/volume)

# option(BUILD_GUI_APPLICATIONS "Build GUI Applications" OFF)
# if (BUILD_GUI_APPLICATIONS)
#     add_subdirectory(gui)
# endif ()

# #########################################################################################
# # File Fetch
# #########################################################################################
# option(DOWNLOAD_FILES "Download large OpenSpace data on configure" OFF)
# if(DOWNLOAD_FILES)
#     function(DownloadFile FILE_PATH FILE_URL)
#         set(FILE_MD5 ${ARGV2})
#         if(NOT EXISTS "${FILE_PATH}")
#             file(DOWNLOAD ${FILE_URL} ${FILE_PATH} INACTIVITY_TIMEOUT 10 SHOW_PROGRESS)
#         endif()
#         file(MD5 ${FILE_PATH} MD5_RESULT)
#         if( NOT "${FILE_MD5}" STREQUAL "" )
#             string(COMPARE EQUAL ${MD5_RESULT} ${FILE_MD5} SUCCESS)
#             if(NOT ${SUCCESS})
#                 message(WARNING "${FILE_PATH} not matching MD5")
#             endif()
#         endif()
#     endfunction(DownloadFile)

#     function(NewHorizonDownload FILE_PATH )
#         set(MD5 "${ARGV1}")
#         DownloadFile("${OPENSPACE_BASE_DIR}/openspace-data/spice/JupiterNhKernels/${FILE_PATH}"
#                      "http://naif.jpl.nasa.gov/pub/naif/pds/data/nh-j_p_ss-spice-6-v1.0/nhsp_1000/data/${FILE_PATH}" ${MD5})
#     endfunction(NewHorizonDownload)

#     NewHorizonDownload("ck/merged_nhpc_2006_v011.bc")
#     NewHorizonDownload("ck/merged_nhpc_2007_v006.bc")
#     NewHorizonDownload("fk/nh_v200.tf")
#     NewHorizonDownload("ik/nh_lorri_v100.ti")
#     NewHorizonDownload("sclk/new_horizons_413.tsc" "6f7a87c21cb3e37835261ed745f34d4a")
#     NewHorizonDownload("spk/de413.bsp")
#     NewHorizonDownload("spk/jup260.bsp")
#     NewHorizonDownload("spk/nh_nep_ura_000.bsp")
#     NewHorizonDownload("spk/nh_recon_e2j_v1.bsp")
#     NewHorizonDownload("spk/nh_recon_j2sep07_prelimv1.bsp")
#     NewHorizonDownload("spk/sb_2002jf56_2.bsp")
# endif(DOWNLOAD_FILES)
