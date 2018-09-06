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
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

set(HEADER_FILES
    servermodule.h
    include/connection.h
    include/connectionpool.h
    include/jsonconverters.h
    include/topics/authorizationtopic.h
    include/topics/bouncetopic.h
    include/topics/getpropertytopic.h
    include/topics/luascripttopic.h
    include/topics/setpropertytopic.h
    include/topics/subscriptiontopic.h
    include/topics/timetopic.h
    include/topics/topic.h
    include/topics/triggerpropertytopic.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    servermodule.cpp
    src/connection.cpp
    src/connectionpool.cpp
    src/jsonconverters.cpp
    src/topics/authorizationtopic.cpp
    src/topics/bouncetopic.cpp
    src/topics/getpropertytopic.cpp
    src/topics/luascripttopic.cpp
    src/topics/setpropertytopic.cpp
    src/topics/subscriptiontopic.cpp
    src/topics/timetopic.cpp
    src/topics/topic.cpp
    src/topics/triggerpropertytopic.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "Server"
    server_module
    ${HEADER_FILES} ${SOURCE_FILES}
)
