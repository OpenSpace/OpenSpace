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
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/httpsynchronization.h
    # ${CMAKE_CURRENT_SOURCE_DIR}/syncs/resourcesynchronization.h
    # ${CMAKE_CURRENT_SOURCE_DIR}/syncs/torrentsynchronization.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/syncs/httpsynchronization.cpp
    # ${CMAKE_CURRENT_SOURCE_DIR}/syncs/resourcesynchronization.cpp
    # ${CMAKE_CURRENT_SOURCE_DIR}/syncs/torrentsynchronization.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

# # Libtorrent
# target_include_directories(
#     openspace-module-sync
#     SYSTEM PUBLIC
#     ${CMAKE_CURRENT_SOURCE_DIR}/ext/libtorrent/include
# )
# target_link_libraries(
#     openspace-module-sync
#      ${CMAKE_CURRENT_SOURCE_DIR}/ext/libtorrent
# )

create_new_module(
    "Sync"
    sync_module
    ${HEADER_FILES} ${SOURCE_FILES}
)
