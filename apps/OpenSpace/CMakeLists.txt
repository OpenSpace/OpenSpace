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

include(${GHOUL_BASE_DIR}/support/cmake/CopySharedLibraries.cmake)

set(APPLICATION_NAME OpenSpace)
set(APPLICATION_LINK_TO_OPENSPACE ON)

# wanted by CEF
set(CMAKE_BUILD_TYPE Debug CACHE INTERNAL "CMAKE_BUILD_TYPE")

if (WIN32)
    set(RESOURCE_FILE ${OPENSPACE_APPS_DIR}/OpenSpace/openspace.rc)
endif ()


########################
# OpenVR section start #
########################
option(OPENSPACE_OPENVR_SUPPORT "Build OpenSpace application with OpenVR support" OFF)

if (OPENSPACE_OPENVR_SUPPORT)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${OPENSPACE_EXT_DIR}/sgct/cmake/modules/")

    find_package(OpenVR REQUIRED)

    set(SGCT_OPENVR_DEFINITIONS OPENVR_SUPPORT)

    if(NOT SGCT_OPENVR_INCLUDE_DIRECTORY)
        if( WIN32 )
            find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
                NAMES SGCTOpenVR.h
                PATHS $ENV{SGCT_ROOT_DIR}/additional_includes/openvr ${OPENSPACE_EXT_DIR}/sgct/additional_includes/openvr NO_DEFAULT_PATH
                REQUIRED)
        else()
            find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
                NAMES SGCTOpenVR.h
                PATH_SUFFIXES SGCTOpenVR
                PATHS $ENV{SGCT_ROOT_DIR}/additional_includes/openvr ${OPENSPACE_EXT_DIR}/sgct/additional_includes/openvr
                REQUIRED)
        endif()
    else()
        set(SGCT_OPENVR_FILES ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.h ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.cpp)
    endif()
else()
    set(OPENVR_INCLUDE_DIRS "")
    set(SGCT_OPENVR_INCLUDE_DIRECTORY "")
    set(OPENVR_LIBRARY "")
    set(SGCT_OPENVR_DEFINITIONS "")
endif()
########################
#  OpenVR section end  #
########################

########################
# Spout section  start #
########################
if (WIN32)
    option(OPENSPACE_SPOUT_SUPPORT "Build OpenSpace application with Spout support" OFF)
endif ()

set(SPOUT_INCLUDE_DIRS "")
set(SPOUT_LIBRARY "")
set(SPOUT_DEFINITIONS "")
if (OPENSPACE_SPOUT_SUPPORT)
    set(SPOUT_INCLUDE_DIRS ${OPENSPACE_APPS_DIR}/OpenSpace/ext/spout)
    set(SPOUT_LIBRARY ${OPENSPACE_APPS_DIR}/OpenSpace/ext/spout/SpoutLibrary.lib)
    set(SPOUT_DEFINITIONS "OPENSPACE_HAS_SPOUT")

    set(SGCT_SPOUT_SUPPORT ON CACHE BOOL "" FORCE)

endif ()

########################
#  Spout section end   #
########################

# Building on MacOS? Bundle int into an .app!
if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    add_executable(${APPLICATION_NAME} MACOSX_BUNDLE
        ${SGCT_OPENVR_FILES}
        ${OPENSPACE_APPS_DIR}/OpenSpace/main.cpp
        ${RESOURCE_FILE}
    )
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    add_executable(${APPLICATION_NAME}
        ${SGCT_OPENVR_FILES}
        ${OPENSPACE_APPS_DIR}/OpenSpace/main.cpp
        ${RESOURCE_FILE}
    )
else ()
    add_executable(${APPLICATION_NAME}
        ${SGCT_OPENVR_FILES}
        ${OPENSPACE_APPS_DIR}/OpenSpace/main.cpp
        ${RESOURCE_FILE}
    )
endif ()

if (OPENSPACE_SPOUT_SUPPORT)
    ghl_copy_files(
        ${APPLICATION_NAME}
        ${OPENSPACE_APPS_DIR}/OpenSpace/ext/spout/SpoutLibrary.dll
    )
endif ()

target_include_directories(
    ${APPLICATION_NAME} PUBLIC
    ${OPENSPACE_BASE_DIR}/include
    ${OPENVR_INCLUDE_DIRS}
    ${SGCT_OPENVR_INCLUDE_DIRECTORY}
    ${SPOUT_INCLUDE_DIRS}
)

target_link_libraries(${APPLICATION_NAME}
    libOpenSpace
    ${OPENVR_LIBRARY}
    ${SPOUT_LIBRARY}
)

# Web Browser and Web gui
# Why not put these in the module's path? Because they do not have access to the
# target as of July 2017, which is needed.
if (OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
    # Add the CEF binary distribution's cmake/ directory to the module path and
    # find CEF to initialize it properly.
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBBROWSER_MODULE_PATH}/cmake")
    include(webbrowser_helpers)

    set_cef_targets("${CEF_ROOT}" "${APPLICATION_NAME}")
    run_cef_platform_config("${CEF_ROOT}" "${CEF_TARGET}" "${WEBBROWSER_MODULE_PATH}")
elseif (OPENSPACE_MODULE_WEBBROWSER)
    message(WARNING "Web configured to be included, but no CEF_ROOT was found, please try configuring CMake again.")
endif ()

if (OPENSPACE_MODULE_WEBGUI AND WEBGUI_MODULE_PATH)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBGUI_MODULE_PATH}/cmake")
    include(webgui_helpers)
    build_webgui_source("${APPLICATION_NAME}")
elseif(OPENSPACE_MODULE_WEBGUI)
    message(WARNING "WebGui is configured to be included, but the web source could not be found. Try configuring CMake again.")
endif()



target_compile_definitions(${APPLICATION_NAME} PUBLIC
    ${SGCT_OPENVR_DEFINITIONS}
    ${SPOUT_DEFINITIONS}
)

if (MSVC)
    target_link_libraries(${APPLICATION_NAME} Dbghelp.lib)
    set_target_properties(${APPLICATION_NAME} PROPERTIES LINK_FLAGS
        "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
    )
endif()
