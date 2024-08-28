##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2024                                                                #
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
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

set(HEADER_FILES
  servermodule.h
  include/connection.h
  include/connectionpool.h
  include/jsonconverters.h
  include/serverinterface.h
  include/topics/authorizationtopic.h
  include/topics/bouncetopic.h
  include/topics/camerapathtopic.h
  include/topics/cameratopic.h
  include/topics/documentationtopic.h
  include/topics/enginemodetopic.h
  include/topics/eventtopic.h
  include/topics/flightcontrollertopic.h
  include/topics/getpropertytopic.h
  include/topics/luascripttopic.h
  include/topics/missiontopic.h
  include/topics/sessionrecordingtopic.h
  include/topics/setpropertytopic.h
  include/topics/shortcuttopic.h
  include/topics/subscriptiontopic.h
  include/topics/timetopic.h
  include/topics/skybrowsertopic.h
  include/topics/topic.h
  include/topics/triggerpropertytopic.h
  include/topics/versiontopic.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  servermodule.cpp
  src/connection.cpp
  src/connectionpool.cpp
  src/jsonconverters.cpp
  src/serverinterface.cpp
  src/topics/authorizationtopic.cpp
  src/topics/bouncetopic.cpp
  src/topics/camerapathtopic.cpp
  src/topics/cameratopic.cpp
  src/topics/documentationtopic.cpp
  src/topics/enginemodetopic.cpp
  src/topics/eventtopic.cpp
  src/topics/flightcontrollertopic.cpp
  src/topics/getpropertytopic.cpp
  src/topics/luascripttopic.cpp
  src/topics/missiontopic.cpp
  src/topics/sessionrecordingtopic.cpp
  src/topics/setpropertytopic.cpp
  src/topics/shortcuttopic.cpp
  src/topics/subscriptiontopic.cpp
  src/topics/timetopic.cpp
  src/topics/skybrowsertopic.cpp
  src/topics/topic.cpp
  src/topics/triggerpropertytopic.cpp
  src/topics/versiontopic.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Server"
  server_module
  ${HEADER_FILES} ${SOURCE_FILES}
)

target_precompile_headers(${server_module} PRIVATE
  <modules/server/include/connection.h>
  <modules/server/include/topics/topic.h>
  <openspace/json.h>
  <ghoul/misc/templatefactory.h>
)
