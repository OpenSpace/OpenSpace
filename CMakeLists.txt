##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2018                                                                #
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

cmake_minimum_required(VERSION 3.8 FATAL_ERROR)

project(OpenSpace)
message(STATUS "Generating OpenSpace project")

set(OPENSPACE_VERSION_MAJOR 0)
set(OPENSPACE_VERSION_MINOR 12)
set(OPENSPACE_VERSION_PATCH 0)
set(OPENSPACE_VERSION_STRING "Beta-2")

set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_APPS_DIR "${OPENSPACE_BASE_DIR}/apps")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")
set(OPENSPACE_CMAKE_EXT_DIR "${OPENSPACE_BASE_DIR}/support/cmake")
set(GHOUL_BASE_DIR "${OPENSPACE_BASE_DIR}/ext/ghoul")

include(${OPENSPACE_CMAKE_EXT_DIR}/module_common.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/global_variables.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/copy_shared_libraries.cmake) 
include(${OPENSPACE_CMAKE_EXT_DIR}/handle_applications.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/handle_modules.cmake)

##########################################################################################
# Cleanup project                                                                        #
##########################################################################################
set_property(GLOBAL PROPERTY USE_FOLDERS On)
set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER CMake)

# Test compiler
if (MSVC AND (MSVC_VERSION LESS 1910))
    message(FATAL_ERROR "OpenSpace requires at least Visual Studio 2017.2")
endif ()

# Set build output directories
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_CMAKE_EXT_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OPENSPACE_BASE_DIR}/bin)

##########################################################################################
# Main                                                                                   #
##########################################################################################

# Get the current working branch
execute_process(
  COMMAND git rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE OPENSPACE_GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the latest abbreviated commit hash of the working branch
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE OPENSPACE_GIT_COMMIT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# See if working directory is clean or not
execute_process(
  COMMAND git diff-index --quiet HEAD --
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE OPENSPACE_GIT_STATUS_RETURN
)
if (NOT OPENSPACE_GIT_STATUS_RETURN EQUAL 0) 
    set(OPENSPACE_GIT_STATUS "uncomitted changes")
else()
    set(OPENSPACE_GIT_STATUS "")
endif()

option(OPENSPACE_WARNINGS_AS_ERRORS "Treat warnings as errors" OFF)

include(src/CMakeLists.txt)

##########################################################################################
# Add external dependencies                                                              #
##########################################################################################
# System libraries
if (APPLE)
    target_include_directories(libOpenSpace PUBLIC "/Developer/Headers/FlatCarbon")
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
endif()

# Ghoul
add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul)
target_link_libraries(libOpenSpace Ghoul)
set_property(TARGET Lua PROPERTY FOLDER "External")
set_property(TARGET lz4 PROPERTY FOLDER "External")
link_directories("${GHOUL_LIBRARY_DIRS}")

# SGCT
set(SGCT_TEXT OFF CACHE BOOL "" FORCE)
set(SGCT_BUILD_CSHARP_PROJECTS OFF CACHE BOOL "" FORCE)
set(SGCT_LIGHT_ONLY ON CACHE BOOL "" FORCE)
set(SGCT_CUSTOMOUTPUTDIRS OFF CACHE BOOL "" FORCE)
set(JPEG_TURBO_WITH_SIMD OFF CACHE BOOL "" FORCE)

add_subdirectory(${OPENSPACE_EXT_DIR}/sgct)
target_include_directories(libOpenSpace SYSTEM PUBLIC ${OPENSPACE_EXT_DIR}/sgct/include)
target_link_libraries(
    libOpenSpace
    sgct_light glew glfw png16_static quat tinyxml2static turbojpeg-static
    vrpn
    ${GLFW_LIBRARIES}
)

if (TARGET sgct_light)
    set_property(TARGET sgct_light PROPERTY FOLDER "External")
endif ()
if (TARGET glew)
    set_property(TARGET glew PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET glfw)
    set_property(TARGET glfw PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET png16_static)
    set_property(TARGET png16_static PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET quat)
    set_property(TARGET quat PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET simd)
    set_property(TARGET simd PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET tinyxml2static)
    set_property(TARGET tinyxml2static PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET turbojpeg-static)
    set_property(TARGET turbojpeg-static PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET vrpn)
    set_property(TARGET vrpn PROPERTY FOLDER "External/SGCT")
endif ()
if (TARGET zlibstatic)
    set_property(TARGET zlibstatic PROPERTY FOLDER "External/SGCT")
endif ()

if (UNIX AND (NOT APPLE))
    target_link_libraries(libOpenSpace Xcursor Xinerama X11)
endif ()

# Spice
add_subdirectory(${OPENSPACE_EXT_DIR}/spice)
target_link_libraries(libOpenSpace Spice)
set_property(TARGET Spice PROPERTY FOLDER "External")

# Curl
if (WIN32)
    set(CURL_ROOT_DIR "${OPENSPACE_EXT_DIR}/curl")
    target_include_directories(libOpenSpace SYSTEM PUBLIC ${CURL_ROOT_DIR}/include)
    target_link_libraries(libOpenSpace ${CURL_ROOT_DIR}/lib/libcurl_imp.lib)
    target_compile_definitions(libOpenSpace PUBLIC "OPENSPACE_CURL_ENABLED" "CURL_STATICLIB")
else ()
    find_package(CURL)
    if (CURL_FOUND)
        target_include_directories(libOpenSpace SYSTEM PUBLIC ${CURL_INCLUDE_DIRS})
        target_link_libraries(libOpenSpace ${CURL_LIBRARIES})
        target_compile_definitions(libOpenSpace PUBLIC "OPENSPACE_CURL_ENABLED")
    endif ()
endif()

# Qt
# Unfortunately, we have to set this value manually; sigh
# In the future, if the Qt version is updated, just add to this variable ---abock
if (APPLE)
    set(CMAKE_PREFIX_PATH
        "~/Qt/5.6/clang_64/lib/cmake"
        "~/Qt/5.7/clang_64/lib/cmake"
        "~/Qt/5.8/clang_64/lib/cmake"
        PARENT_SCOPE
    )
endif ()

if (MSVC)
    option(OPENSPACE_ENABLE_VLD "Enable the Visual Leak Detector" OFF)
    if (OPENSPACE_ENABLE_VLD)
        target_compile_definitions(libOpenSpace PUBLIC "OPENSPACE_ENABLE_VLD")
        target_link_libraries(libOpenSpace ${OPENSPACE_EXT_DIR}/vld/lib/vld.lib)
        target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_EXT_DIR}/vld)
    endif ()
endif ()


##########################################################################################
# Tests                                                                                  #
##########################################################################################
option(OPENSPACE_HAVE_TESTS "Activate the OpenSpace unit tests" ON)
if (OPENSPACE_HAVE_TESTS)
    if (NOT TARGET gtest)
        set(BUILD_GTEST ON CACHE BOOL "")
        set(BUILD_GMOCK OFF CACHE BOOL "")
        set(gtest_force_shared_crt ON CACHE BOOL "")
        add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul/ext/googletest)
        set_property(TARGET gtest PROPERTY FOLDER "External")
        set_property(TARGET gtest_main PROPERTY FOLDER "External")
    endif ()

    file(GLOB_RECURSE OPENSPACE_TEST_FILES ${OPENSPACE_BASE_DIR}/tests/*.inl)

    add_executable(OpenSpaceTest ${OPENSPACE_BASE_DIR}/tests/main.cpp ${OPENSPACE_TEST_FILES})
    target_include_directories(OpenSpaceTest PUBLIC
        "${OPENSPACE_BASE_DIR}/include"
        "${OPENSPACE_BASE_DIR}/tests"
        "${OPENSPACE_EXT_DIR}/ghoul/ext/googletest/googletest/include"
    )
    target_compile_definitions(OpenSpaceTest PUBLIC
        "GHL_THROW_ON_ASSERT"
        "GTEST_HAS_TR1_TUPLE=0"
    )
    target_link_libraries(OpenSpaceTest gtest libOpenSpace)

    set_property(TARGET OpenSpaceTest PROPERTY FOLDER "Unit Tests")

    if (MSVC)
        set_target_properties(OpenSpaceTest PROPERTIES LINK_FLAGS
            "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
        )
    endif ()
    set_openspace_compile_settings(OpenSpaceTest)
endif (OPENSPACE_HAVE_TESTS)
if (TARGET GhoulTest)
    if (NOT TARGET gtest)
        set(BUILD_GTEST ON CACHE BOOL "")
        set(BUILD_GMOCK OFF CACHE BOOL "")
        set(gtest_force_shared_crt ON CACHE BOOL "")
        add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul/ext/googletest)
    endif ()

    set_property(TARGET gtest PROPERTY FOLDER "External")
    set_property(TARGET GhoulTest PROPERTY FOLDER "Unit Tests")
endif ()

set(OPENSPACE_EXTERNAL_MODULES_PATHS "" CACHE STRING "List of external modules")
handle_modules("${OPENSPACE_BASE_DIR}/modules" "${OPENSPACE_EXTERNAL_MODULES_PATHS}")

handle_applications()

##########################################################################################
# Misc settings                                                                          #
##########################################################################################
option(OPENSPACE_WITH_ABUFFER_RENDERER "Compile ABuffer Renderer" OFF)
if (OPENSPACE_WITH_ABUFFER_RENDERER)
    target_compile_definitions(libOpenSpace PUBLIC "OPENSPACE_WITH_ABUFFER_RENDERER")
endif ()


# Just in case, create the bin directory
add_custom_command(
    TARGET libOpenSpace
    PRE_BUILD COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)

# Manage the CPack packaging
include(${OPENSPACE_CMAKE_EXT_DIR}/packaging.cmake)
