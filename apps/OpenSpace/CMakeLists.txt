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
endif ()

########################
#  Spout section  end  #
########################

add_executable(${APPLICATION_NAME}
    ${SGCT_OPENVR_FILES}
    ${OPENSPACE_APPS_DIR}/OpenSpace/main.cpp
    ${RESOURCE_FILE}
)
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
