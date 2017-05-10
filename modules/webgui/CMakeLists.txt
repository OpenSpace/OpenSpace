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

include(${OPENSPACE_CMAKE_EXT_DIR}/module_definition.cmake)

# Copyright (c) 2016 The Chromium Embedded Framework Authors. All rights
# reserved. Use of this source code is governed by a BSD-style license that
# can be found in the LICENSE file.
cmake_minimum_required(VERSION 2.8.12.1)

# Only generate Debug and Release configuration types.
set(CMAKE_CONFIGURATION_TYPES Debug Release)

# Project name.
# project(cef)

# Use folders in the resulting project files.
set_property(GLOBAL PROPERTY OS_FOLDERS ON)

# Specify the CEF distribution version.
set(CEF_VERSION "3.2987.1601.gf035232")

# Determine the platform.
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set(CEF_PLATFORM "macosx64")
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    if(CMAKE_SIZEOF_VOID_P MATCHES 8)
        set(CEF_PLATFORM "linux64")
    else()
        set(CEF_PLATFORM "linux32")
    endif()
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    if(CMAKE_SIZEOF_VOID_P MATCHES 8)
        set(CEF_PLATFORM "windows64")
    else()
        set(CEF_PLATFORM "windows32")
    endif()
endif()

# Add this project's cmake/ directory to the module path.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Download and extract the CEF binary distribution (executes DownloadCEF.cmake).
include(DownloadCEF)
DownloadCEF("${CEF_PLATFORM}" "${CEF_VERSION}" "${CMAKE_CURRENT_SOURCE_DIR}/ext/cef")

# Add the CEF binary distribution's cmake/ directory to the module path.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CEF_ROOT}/cmake")

# Load the CEF configuration (executes FindCEF.cmake).
find_package(CEF REQUIRED)

# Include the libcef_dll_wrapper target (executes libcef_dll/CMakeLists.txt).
add_subdirectory(${CEF_LIBCEF_DLL_WRAPPER_PATH} libcef_dll_wrapper)

# Include sample application targets (executes <target>/CMakeLists.txt).
# add_subdirectory(${CEF_ROOT}/tests/cefclient)
# add_subdirectory(${CEF_ROOT}/tests/cefsimple)
# add_subdirectory(${CEF_ROOT}/tests/gtest)
# add_subdirectory(${CEF_ROOT}/tests/ceftests)


# Create OpenSpace module.
set(HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/webguimodule.h
)
source_group("Header Files" FILES ${HEADER_FILES})
set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/webguimodule.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "WebGUI"
    server_module
    ${HEADER_FILES} ${SOURCE_FILES}
)

# Display configuration settings.
PRINT_CEF_CONFIG()
