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
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

set(HEADER_FILES
    include/connectionpool.h
    include/connection.h
    include/jsonconverters.h
    include/topic.h
    include/authenticationtopic.h
    include/getpropertytopic.h
    include/luascripttopic.h
    include/subscriptiontopic.h
    include/setpropertytopic.h
    include/triggerpropertytopic.h
    ${CMAKE_CURRENT_SOURCE_DIR}/servermodule.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    src/connectionpool.cpp
    src/connection.cpp
    src/jsonconverters.cpp
    src/topics/topic.cpp
    src/topics/authenticationtopic.cpp
    src/topics/getpropertytopic.cpp
    src/topics/luascripttopic.cpp
    src/topics/subscriptiontopic.cpp
    src/topics/setpropertytopic.cpp
    src/topics/triggerpropertytopic.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/servermodule.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "Server"
    server_module
    ${HEADER_FILES} ${SOURCE_FILES}
)
