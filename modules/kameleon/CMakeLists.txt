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

set(HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/include/kameleonwrapper.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/src/kameleonwrapper.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "Kameleon"
    kameleon_module
    ${HEADER_FILES} ${SOURCE_FILES}
)

    option(KAMELEON_LIBRARY_ONLY "Build with Kameleon as library only" ON)
    if (WIN32)
        option(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
    else ()
        option(BUILD_SHARED_LIBS "Build Shared Libraries" ON)
    endif ()
    mark_as_advanced(BUILD_SHARED_LIBS) # Change to set instead of option
    option(KAMELEON_USE_HDF5 "Kameleon use HDF5" OFF)
    set(KAMELEON_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ext/kameleon)
    set(KAMELEON_INCLUDES ${KAMELEON_ROOT_DIR}/src)    
    add_subdirectory(${KAMELEON_ROOT_DIR})
    target_include_directories(${kameleon_module} SYSTEM PUBLIC ${KAMELEON_INCLUDES})
    target_link_libraries(${kameleon_module} ccmc)
    if (OPENSPACE_DISABLE_EXTERNAL_WARNINGS)
        if (MSVC)
            target_compile_options(ccmc PRIVATE "/W0" "/MP")
        else ()
            target_compile_options(ccmc PRIVATE "-w")
        endif ()
        target_compile_definitions(ccmc PUBLIC "_SCL_SECURE_NO_WARNINGS")
    endif ()
    set_property(TARGET ccmc PROPERTY FOLDER "External")
    if (TARGET cdf)
        if (GHOUL_DISABLE_EXTERNAL_WARNINGS)
            if (MSVC)
                target_compile_options(cdf PRIVATE "/W0" "/MP")
            else ()
                target_compile_options(cdf PRIVATE "-w")
            endif ()
        endif ()
        set_property(TARGET cdf PROPERTY FOLDER "External")
    endif ()
# Boost
find_package(Boost REQUIRED)
target_include_directories(openspace-module-kameleon SYSTEM PUBLIC ${Boost_INCLUDE_DIRS})
#include_external_library(${onscreengui_module} Imgui ${CMAKE_CURRENT_SOURCE_DIR}/ext/kameleon)
