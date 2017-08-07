##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2017                                                                #
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

cmake_minimum_required (VERSION 3.8 FATAL_ERROR)

project(OpenSpace)
message(STATUS "Generating OpenSpace project")

set(OPENSPACE_VERSION_MAJOR 0)
set(OPENSPACE_VERSION_MINOR 9)
set(OPENSPACE_VERSION_PATCH 0)
set(OPENSPACE_VERSION_STRING "prerelease-14 (Astronomy Live)")

set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_APPS_DIR "${OPENSPACE_BASE_DIR}/apps")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")
set(OPENSPACE_MODULE_DIR "${OPENSPACE_BASE_DIR}/modules")
set(OPENSPACE_CMAKE_EXT_DIR "${OPENSPACE_BASE_DIR}/support/cmake")
set(GHOUL_BASE_DIR "${OPENSPACE_BASE_DIR}/ext/ghoul")

include(${OPENSPACE_CMAKE_EXT_DIR}/support_macros.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/module_common.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/copy_shared_libraries.cmake) 

# Cleanup project
set_property(GLOBAL PROPERTY USE_FOLDERS On)
set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER CMake)

# Test compiler
if (MSVC AND (MSVC_VERSION LESS 1910))
    message(FATAL_ERROR "OpenSpace requires at least Visual Studio 2017.2")
endif ()

# Set build output directories
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_CMAKE_EXT_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OPENSPACE_BASE_DIR}/bin)

option(OPENSPACE_WARNINGS_AS_ERRORS "Treat warnings as errors" OFF)

include(src/CMakeLists.txt)

create_openspace_target()
add_external_dependencies()
handle_applications()

if (MSVC)
    option(OPENSPACE_ENABLE_VLD "Enable the Visual Leak Detector" OFF)
    handle_option_vld()
endif ()

option(OPENSPACE_HAVE_TESTS "Activate the OpenSpace unit tests" ON)
handle_option_tests()

handle_internal_modules()
copy_dynamic_libraries()

include(${OPENSPACE_CMAKE_EXT_DIR}/packaging.cmake)
