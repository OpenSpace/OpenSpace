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
set(WEBGUI_MODULE_NAME WebGUI)

# wanted by CEF
set(PROJECT_ARCH "x86_64")
set(CMAKE_BUILD_TYPE Debug)

##########################################################################################
# Download CEF

# Copyright (c) 2016 The Chromium Embedded Framework Authors. All rights
# reserved. Use of this source code is governed by a BSD-style license that
# can be found in the LICENSE file.

cmake_minimum_required(VERSION 2.8.12.1)

# Only generate Debug and Release configuration types.
set(CMAKE_CONFIGURATION_TYPES Debug Release)

# Use folders in the resulting project files.
set_property(GLOBAL PROPERTY OS_FOLDERS ON)

# Specify the CEF distribution version.
set(CEF_VERSION "3.3029.1617.gaf831b6")

# Add this project's cmake/ directory to the module path.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(cef_support)

# determine the current build platform
set_current_cef_build_platform()

# Download and extract the CEF binary distribution (executes DownloadCEF.cmake).
DownloadCEF("${CEF_PLATFORM}" "${CEF_VERSION}" "${CMAKE_CURRENT_SOURCE_DIR}/ext/cef")

# Add the CEF binary distribution's cmake/ directory to the module path.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CEF_ROOT}/cmake")

# Load the CEF configuration (executes FindCEF.cmake).
find_package(CEF REQUIRED)

# Include the libcef_dll_wrapper target (executes libcef_dll/CMakeLists.txt).
add_subdirectory(${CEF_LIBCEF_DLL_WRAPPER_PATH} libcef_dll_wrapper)


##########################################################################################
# Add CEF client files



##########################################################################################
# Create OpenSpace module.
set(OPENSPACE_HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/webguimodule.h
)
source_group("Header Files" FILES ${OPENSPACE_HEADER_FILES})

set(OPENSPACE_SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/webguimodule.cpp
)
source_group("Source Files" FILES ${OPENSPACE_SOURCE_FILES})

create_new_module(
    ${WEBGUI_MODULE_NAME}
    webgui_module
    ${OPENSPACE_HEADER_FILES} ${OPENSPACE_SOURCE_FILES}
)

# Display CEF configuration settings.
PRINT_CEF_CONFIG()
