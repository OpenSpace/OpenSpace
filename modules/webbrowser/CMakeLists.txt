##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2025                                                                #
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

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)
include(cmake/webbrowser_helpers.cmake)

set(WEBBROWSER_MODULE_NAME WebBrowser)
set(WEBBROWSER_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "WEBBROWSER_MODULE_PATH")

# wanted by CEF
if (CMAKE_SYSTEM_NAME STREQUAL "Darwin" AND CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64")
  set(PROJECT_ARCH "arm64") # For macOS on ARM64
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux" AND CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64")
  set(PROJECT_ARCH "armv8-a") # For Ubuntu/Linux on ARM64 - or should it be aarch64? Builds OK on Ubuntu with armv8-a
else ()
  set(PROJECT_ARCH "x86_64") # Default to x86_64
endif ()

##########################################################################################
# Download CEF

# Copyright (c) 2016 The Chromium Embedded Framework Authors. All rights
# reserved. Use of this source code is governed by a BSD-style license that
# can be found in the LICENSE file.

cmake_minimum_required(VERSION 3.10)

# Use folders in the resulting project files.
set_property(GLOBAL PROPERTY OS_FOLDERS ON)

# Use <PackageName>_ROOT variables
# https://cmake.org/cmake/help/git-stage/policy/CMP0074.html
cmake_policy(SET CMP0074 NEW)

# Specify the CEF distribution version.
set(CEF_VERSION "127.3.5+g114ea2a+chromium-127.0.6533.120")

# CEF Sandbox is not working with the latest Visual Studio, so we disable it for now.
if (WIN32 OR UNIX)
  option(USE_SANDBOX OFF)
endif ()
# Add this project's cmake/ directory to the module path.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(cef_support)

# determine the current build platform
set_current_cef_build_platform()

# Download and extract the CEF binary distribution (executes DownloadCEF.cmake).
download_cef("${CEF_PLATFORM}" "${CEF_VERSION}" "${CMAKE_CURRENT_BINARY_DIR}/ext/cef")

##########################################################################################
# Apply cmake patch

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/cmake/patch/CMakeLists.txt DESTINATION ${CEF_ROOT})

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/cmake/patch/cmake/cef_variables.cmake DESTINATION ${CEF_ROOT}/cmake)

# CEF does not provide RelWithDebInfo libs, so we use the ones without Debug information
file(COPY ${CEF_ROOT}/Release/
  DESTINATION ${CEF_ROOT}/RelWithDebInfo
  FILES_MATCHING PATTERN *
)

##########################################################################################

# Add the CEF binary distribution's cmake/ directory to the module path.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CEF_ROOT}/cmake")

# Load the CEF configuration (executes FindCEF.cmake).
find_package(CEF REQUIRED)

# Include the libcef_dll_wrapper target (executes libcef_dll/CMakeLists.txt).
add_subdirectory(${CEF_LIBCEF_DLL_WRAPPER_PATH} libcef_dll_wrapper SYSTEM)

mark_as_advanced(CEF_DEBUG_INFO_FLAG USE_ATL USE_OFFICIAL_BUILD_SANDBOX USE_SANDBOX)

if (WIN32)
  target_compile_options(libcef_dll_wrapper PRIVATE "/W0")
elseif (UNIX)
  target_compile_options(libcef_dll_wrapper PRIVATE "-w")
endif ()

target_precompile_headers(libcef_dll_wrapper PRIVATE
  [["include/cef_client.h"]]
  [["include/capi/cef_browser_capi.h"]]
  [["include/capi/cef_client_capi.h"]]
  [["include/cef_client.h"]]
  [["include/capi/cef_base_capi.h"]]
  [["include/cef_render_handler.h"]]
  <string>
  <sstream>
  <istream>
  <ostream>
)

if (WIN32)
  target_precompile_headers(libcef_dll_wrapper PRIVATE
    <Windows.h>
  )
endif ()

##########################################################################################
# Add CEF client files

set(WEBBROWSER_SOURCES
  webbrowsermodule.cpp
  src/browserclient.cpp
  src/eventhandler.cpp
  src/webbrowserapp.cpp
  src/defaultbrowserlauncher.cpp
)
set(WEBBROWSER_SOURCES_WINDOWS
  OpenSpace.exe.manifest
  cef_webgui.rc
  resource.h
  simple_handler_win.cc
)
append_platform_sources(WEBBROWSER_SOURCES)
append_platform_sources(WEBBROWSER_RESOURCES_SOURCES)

set(WEBBROWSER_SOURCES ${WEBBROWSER_SOURCES} ${WEBBROWSER_RESOURCES_SOURCES})

# CEF helper sources
set(WEBBROWSER_HELPER_SOURCES src/webbrowserapp.cpp)
if (OS_MACOSX)
  list(APPEND WEBBROWSER_HELPER_SOURCES src/processhelpermac.cpp)
endif ()
set(WEBBROWSER_HELPER_SOURCES_WINDOWS src/processhelperwindows.cpp)
if (OS_LINUX)
  list(APPEND WEBBROWSER_HELPER_SOURCES src/processhelperlinux.cpp)
  set(WEBBROWSER_HELPER_SOURCES_LINUX src/processhelperlinux.cpp)
endif ()
append_platform_sources(WEBBROWSER_HELPER_SOURCES)

set(WEBBROWSER_RESOURCES_MAC_SOURCES_MACOSX
  mac/Info.plist
)
append_platform_sources(WEBBROWSER_RESOURCES_MAC_SOURCES)

set(WEBBROWSER_RESOURCES_MAC_ENGLISH_LPROJ_SRCS_MACOSX
  mac/English.lproj/InfoPlist.strings
  mac/English.lproj/MainMenu.xib
)
append_platform_sources(WEBBROWSER_RESOURCES_MAC_ENGLISH_LPROJ_SRCS)

set(WEBBROWSER_RESOURCES_SRCS
  ${WEBBROWSER_RESOURCES_MAC_SOURCES}
  ${WEBBROWSER_RESOURCES_MAC_ENGLISH_LPROJ_SRCS}
)

# Place Helper in separate executable
# The naming style "<ApplicationName> Helper" is required by Chromium.
set(CEF_HELPER_TARGET "OpenSpace_Helper" CACHE INTERNAL "CEF_HELPER_TARGET")
set(CEF_HELPER_TARGET_OUTPUT_NAME "OpenSpace Helper" CACHE INTERNAL "")

set(CEF_HELPER_TARGET_GPU "OpenSpace_Helper_GPU" CACHE INTERNAL "CEF_HELPER_TARGET_GPU")
set(CEF_HELPER_TARGET_GPU_OUTPUT_NAME "OpenSpace Helper (GPU)" CACHE INTERNAL "")

set(CEF_HELPER_TARGET_RENDERER "OpenSpace_Helper_Renderer" CACHE INTERNAL "CEF_HELPER_TARGET_RENDERER")
set(CEF_HELPER_TARGET_RENDERER_OUTPUT_NAME "OpenSpace Helper (Renderer)" CACHE INTERNAL "")

#
# CEF platform-specific config
#

list(APPEND Targets ${CEF_HELPER_TARGET} ${CEF_HELPER_TARGET_GPU} ${CEF_HELPER_TARGET_RENDERER})


if (OS_MACOSX)
  add_logical_target("cef_sandbox_lib" "${CEF_SANDBOX_LIB_DEBUG}" "${CEF_SANDBOX_LIB_RELEASE}")
  foreach(target IN LISTS Targets)
    # Helper executable target.
    add_executable(${target} MACOSX_BUNDLE ${WEBBROWSER_HELPER_SOURCES})
    set_executable_target_properties(${target})
    # add_cef_logical_target("libcef_lib" "${CEF_LIB_DEBUG}" "${CEF_LIB_RELEASE}")
    add_dependencies(${target} libcef_dll_wrapper)
    target_link_libraries(${target} libcef_dll_wrapper ${CEF_STANDARD_LIBS})
    set_target_properties(${target} PROPERTIES
      MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/mac/helper-Info.plist
      OUTPUT_NAME "${target}_OUTPUT_NAME"
    )

    target_compile_options(${target} PRIVATE -Wno-deprecated-declarations)
    target_compile_options(libcef_dll_wrapper PRIVATE -Wno-deprecated-declarations)

    if (USE_SANDBOX)
      # Logical target used to link the cef_sandbox library.
      target_link_libraries(${target} cef_sandbox_lib)
    endif ()
  endforeach()

  set_target_properties(${CEF_HELPER_TARGET_GPU} PROPERTIES FOLDER "Helper")
  set_target_properties(${CEF_HELPER_TARGET_RENDERER} PROPERTIES FOLDER "Helper")
else()
  message(STATUS "Setting up WebBrowser CEF helper executable: ${CEF_HELPER_TARGET}")
  set_openspace_cef_target_out_dir()
  add_executable(${CEF_HELPER_TARGET} ${WEBBROWSER_HELPER_SOURCES})

  if (WIN32)
    target_compile_options(${CEF_HELPER_TARGET} PRIVATE "/W0")
  elseif (UNIX)
    target_compile_options(${CEF_HELPER_TARGET} PRIVATE "-w")
  endif ()

  set_executable_target_properties(${CEF_HELPER_TARGET})
  add_dependencies(${CEF_HELPER_TARGET} libcef_dll_wrapper)
  # Logical target used to link the libcef library.
  add_cef_logical_target("libcef_lib" "${CEF_LIB_DEBUG}" "${CEF_LIB_RELEASE}")
  target_link_libraries(${CEF_HELPER_TARGET} libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
  if (OS_WINDOWS)
    # Add the custom manifest files to the executable.
    add_windows_cef_manifest("${CEF_TARGET_OUT_DIR}" "${WEBBROWSER_MODULE_PATH}" "${CEF_HELPER_TARGET}" "exe")
  endif (OS_WINDOWS)
endif ()

set_target_properties(${CEF_HELPER_TARGET} PROPERTIES FOLDER "Helper")
set_openspace_compile_settings(${CEF_HELPER_TARGET})

##########################################################################################
# Create OpenSpace module.
set(HEADER_FILES
  webbrowsermodule.h
  include/webrenderhandler.h
  include/webkeyboardhandler.h
  include/browserclient.h
  include/screenspacebrowser.h
  include/browserinstance.h
  include/eventhandler.h
  include/webbrowserapp.h
  include/defaultbrowserlauncher.h
  include/cefhost.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  webbrowsermodule.cpp
  src/webrenderhandler.cpp
  src/webkeyboardhandler.cpp
  src/browserclient.cpp
  src/screenspacebrowser.cpp
  src/browserinstance.cpp
  src/eventhandler.cpp
  src/webbrowserapp.cpp
  src/defaultbrowserlauncher.cpp
  src/cefhost.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

create_new_module(
  ${WEBBROWSER_MODULE_NAME}
  webbrowser_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)

target_precompile_headers(${webbrowser_module} PRIVATE
  <include/cef_browser.h>
  <include/cef_client.h>
  <include/wrapper/cef_helpers.h>
)

set_target_properties(libcef_dll_wrapper PROPERTIES FOLDER "Helper")
set_openspace_compile_settings(libcef_dll_wrapper)

# Display CEF configuration settings.
# PRINT_CEF_CONFIG()

target_include_directories(${webbrowser_module} SYSTEM PUBLIC ${CEF_ROOT})

set_modules_dependency_on_cef_libraries(${webbrowser_module})

set(deps "")
foreach (i ${CEF_BINARY_FILES})
  list(APPEND deps "${CEF_BINARY_DIR}/${i}")
endforeach()
foreach (j ${CEF_RESOURCE_FILES})
  list(APPEND deps "${CEF_RESOURCE_DIR}/${j}")
endforeach()
add_external_library_dependencies("${deps}")

if (UNIX AND NOT (${CMAKE_SYSTEM_NAME} MATCHES "Darwin"))
  # Linux needs to have copies of CEF files in Resources/ in its build-type dir to avoid
  # the 'Couldn't mmap icu data file' runtime error
  file(COPY ${CEF_ROOT}/Resources/
    DESTINATION ${CEF_ROOT}/${CMAKE_BUILD_TYPE}/
    FILES_MATCHING PATTERN *
  )
endif ()
