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
    ${CMAKE_CURRENT_SOURCE_DIR}/syncmodule.h
    ${CMAKE_CURRENT_SOURCE_DIR}/torrentclient.h
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/httpsynchronization.h
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/torrentsynchronization.h
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/urlsynchronization.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tasks/syncassettask.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/syncmodule.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/torrentclient.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/httpsynchronization.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/torrentsynchronization.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/urlsynchronization.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tasks/syncassettask.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "Sync"
    sync_module
    ${HEADER_FILES} ${SOURCE_FILES}
)

#####
# Libtorrent
#####
option(OPENSPACE_MODULE_SYNC_USE_LIBTORRENT "Use libtorrent" OFF)

if (OPENSPACE_MODULE_SYNC_USE_LIBTORRENT)
    set(Boost_USE_STATIC_LIBS ON)
    set(Boost_USE_MULTITHREADED ON)
    SET(LIBTORRENT_encryption OFF CACHE BOOL "Use OpenSSL Encryption" FORCE)
    SET(LIBTORRENT_shared OFF CACHE BOOL "Use Libtorrent as shared library" FORCE)

    include_external_library(
        ${sync_module}
        torrent-rasterbar
        ${CMAKE_CURRENT_SOURCE_DIR}/ext/libtorrent
    )
    target_include_directories(
        ${sync_module}
        SYSTEM PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/ext/libtorrent/include
    )

    target_compile_definitions(openspace-module-sync PUBLIC SYNC_USE_LIBTORRENT)
endif () # OPENSPACE_MODULE_SYNC_USE_LIBTORRENT