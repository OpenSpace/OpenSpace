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

include(${OPENSPACE_CMAKE_EXT_DIR}/module_definition.cmake)

set(HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/dataloadermodule.h
    ${CMAKE_CURRENT_SOURCE_DIR}/helpers.h
    ${CMAKE_CURRENT_SOURCE_DIR}/operators/operator.h
    ${CMAKE_CURRENT_SOURCE_DIR}/operators/reader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/operators/loader.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/dataloadermodule.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/dataloadermodule_lua.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/helpers.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/operators/operator.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/operators/reader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/operators/loader.cpp
)

option(OPENSPACE_MODULE_DATALOADER "Use Data Loader Module" ON)

source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "DataLoader"
    dataloader_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} 
)

# nativefiledialog
# Dep is a git submodule but does not use CMake
# Handling files here in favor of easy updates as a git submodule
# as opposed to a custom file structure with a CMakeLists.txt for it
set (NFD_SOURCE_FILES ${CMAKE_CURRENT_SOURCE_DIR}/ext/nativefiledialog/src/nfd_common.c)
set (NFD_EXTRA)

if (WIN32)
    set (NFD_SOURCE_FILES ${NFD_SOURCE_FILES} ${CMAKE_CURRENT_SOURCE_DIR}/ext/nativefiledialog/src/nfd_win.cpp)
endif(WIN32)

if (APPLE)
    set (NFD_SOURCE_FILES ${NFD_SOURCE_FILES} ${CMAKE_CURRENT_SOURCE_DIR}/ext/nativefiledialog/src/nfd_cocoa.m)
endif( APPLE)

if (UNIX AND (NOT APPLE))
    FIND_PACKAGE(PkgConfig REQUIRED)
    PKG_CHECK_MODULES(GTK3 REQUIRED gtk+-3.0) # Ubuntu: libgtk-3-dev

    set (NFD_SOURCE_FILES ${NFD_SOURCE_FILES} ${CMAKE_CURRENT_SOURCE_DIR}/ext/nativefiledialog/src/nfd_gtk.c)
    include_directories(${GTK3_INCLUDE_DIRS})
    link_directories(${GTK3_LIBRARY_DIRS})
    add_definitions(${GTK3_CFLAGS_OTHER})
    set (NFD_EXTRA ${NFD_EXTRA} ${GTK3_LIBRARIES})
endif()

add_library(nativefiledialog ${NFD_SOURCE_FILES})

target_include_directories(nativefiledialog 
    SYSTEM PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/nativefiledialog/src/include
)

target_link_libraries(openspace-module-dataloader 
    nativefiledialog 
    ${NFD_EXTRA}
)
