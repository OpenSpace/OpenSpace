##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2020                                                                #
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

include(${GHOUL_BASE_DIR}/support/cmake/copy_shared_libraries.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/message_macros.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/application_definition.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/global_variables.cmake)

#####
# OpenVR
#####
option(OPENSPACE_OPENVR_SUPPORT "Build OpenSpace application with OpenVR support" OFF)

if (OPENSPACE_OPENVR_SUPPORT)
  begin_header("Dependency: OpenVR")
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${OPENSPACE_EXT_DIR}/sgct/cmake/modules/")

  find_package(OpenVR REQUIRED)

  set(SGCT_OPENVR_DEFINITIONS OPENVR_SUPPORT)
  if (NOT SGCT_OPENVR_INCLUDE_DIRECTORY)
    if (WIN32)
      find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
        NAMES SGCTOpenVR.h
        PATHS ${OPENSPACE_EXT_DIR}/sgct/additional_includes/openvr NO_DEFAULT_PATH
        REQUIRED
      )
    else ()
      find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
        NAMES SGCTOpenVR.h
        PATH_SUFFIXES SGCTOpenVR
        PATHS ${OPENSPACE_EXT_DIR}/sgct/additional_includes/openvr
        REQUIRED
      )
    endif ()
  else ()
    set(SGCT_OPENVR_FILES
      ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.h
      ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.cpp
    )
  endif ()
  end_header("Dependency: OpenVR")
endif ()

#####
# Spout
#####
if (SGCT_SPOUT_SUPPORT AND NOT OPENSPACE_MODULE_SPOUT)
  message(WARNING "Spout support from SGCT was requested, but OpenSpace Spout module was disabled")
  message(STATUS "Enabling module as dependency")
  set(OPENSPACE_MODULE_SPOUT ON CACHE BOOL "Build OPENSPACE_MODULE_SPOUTModule" FORCE)
endif ()


#####
# macos
#####
if (APPLE)
  set(MACOSX_BUNDLE_ICON_FILE openspace.icns)
  set(CMAKE_OSX_DEPLOYMENT_TARGET "10.14" CACHE STRING "Minimum OS X deployment version" FORCE)
  set(CMAKE_XCODE_ATTRIBUTE_OTHER_CODE_SIGN_FLAGS "--deep" CACHE STRING "Other Code Signing Flags" FORCE)
endif ()

create_new_application(OpenSpace
  ${SGCT_OPENVR_FILES}
  ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.rc
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.icns
)

set_source_files_properties(
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.icns
  PROPERTIES MACOSX_PACKAGE_LOCATION "Resources"
)

target_include_directories(
  OpenSpace PRIVATE
  ${OPENVR_INCLUDE_DIRS}
  ${SGCT_OPENVR_INCLUDE_DIRECTORY}
  ${SPOUT_INCLUDE_DIRS}
)

target_link_libraries(OpenSpace openspace-core ${OPENVR_LIBRARY} ${SPOUT_LIBRARY})

target_compile_definitions(OpenSpace PRIVATE
  ${SGCT_OPENVR_DEFINITIONS}
  ${SPOUT_DEFINITIONS}
)

begin_header("Dependency: SGCT")

set(SGCT_TEXT OFF CACHE BOOL "" FORCE)
set(SGCT_DEP_INCLUDE_FREETYPE OFF CACHE BOOL "" FORCE)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ext/sgct)
target_include_directories(OpenSpace SYSTEM PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/ext/sgct/include)
target_link_libraries(OpenSpace sgct)

set_folder_location(sgct "External")
set_folder_location(glfw "External/SGCT")
set_folder_location(miniziplibstatic "External/SGCT")
set_folder_location(png16_static "External/SGCT")
set_folder_location(quat "External/SGCT")
set_folder_location(tinyxml2static "External/SGCT")
set_folder_location(vrpn "External/SGCT")
set_folder_location(zlibstatic "External/SGCT")

if (UNIX AND (NOT APPLE))
  target_link_libraries(OpenSpace Xcursor Xinerama X11)
endif ()

end_header("Dependency: SGCT")


# Web Browser and Web gui
# Why not put these in the module's path? Because they do not have access to the
# target as of July 2017, which is needed.
if (OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  # wanted by CEF
  set(PROJECT_ARCH "x86_64")

  if (WIN32)
    set(RESOURCE_FILE ${OPENSPACE_APPS_DIR}/OpenSpace/openspace.rc)
  endif ()

  # Add the CEF binary distribution's cmake/ directory to the module path and
  # find CEF to initialize it properly.
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBBROWSER_MODULE_PATH}/cmake")
  include(webbrowser_helpers)

  set_cef_targets("${CEF_ROOT}" OpenSpace)
  run_cef_platform_config("${CEF_ROOT}" "${CEF_TARGET}" "${WEBBROWSER_MODULE_PATH}")
elseif (OPENSPACE_MODULE_WEBBROWSER)
  message(WARNING "Web configured to be included, but no CEF_ROOT was found, please try configuring CMake again.")
endif ()

if (MSVC)
  begin_header("Dependency: Dbghelp")
  # This library is used for being able to output the callstack if an exception escapes
  target_link_libraries(OpenSpace Dbghelp.lib)
  end_header()
endif ()

if (OPENSPACE_NVTOOLS_ENABLED)
  target_link_libraries(OpenSpace "${OPENSPACE_NVTOOLS_PATH}/lib/x64/nvToolsExt64_1.lib")
endif ()
