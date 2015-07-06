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

cmake_minimum_required (VERSION 3.0 FATAL_ERROR)

project (OpenSpace)
message(STATUS "Generating OpenSpace project")

set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_APPS_DIR "${OPENSPACE_BASE_DIR}/apps")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")
set(OPENSPACE_MODULE_DIR "${OPENSPACE_BASE_DIR}/modules")
set(OPENSPACE_CMAKE_EXT_DIR "${OPENSPACE_BASE_DIR}/support/cmake")

include(${OPENSPACE_CMAKE_EXT_DIR}/support_macros.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/module_common.cmake)
include(${OPENSPACE_EXT_DIR}/ghoul/ext/CopySharedLibraries.cmake) 

test_compiler_compatibility()
cleanup_project()
set_build_output_directories()
configure_openspace_version(0 1 1 "prerelease-6")

option(OPENSPACE_WARNINGS_AS_ERRORS "Treat warnings as errors" OFF)
option(OPENSPACE_DISABLE_EXTERNAL_WARNINGS "Disable warnings in external libraries" ON)

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
