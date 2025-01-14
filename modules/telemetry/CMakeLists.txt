##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2025                                                                #
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

set(HEADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/include/general/anglemodetelemetry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/general/cameratelemetry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/general/focustelemetry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/general/nodestelemetry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/general/timetelemetry.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/specific/planetscomparesonification.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/specific/planetsoverviewsonification.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/specific/planetssonification.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/telemetrybase.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/util.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/src/general/anglemodetelemetry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/general/cameratelemetry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/general/focustelemetry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/general/nodestelemetry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/general/nodestelemetry_lua.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/src/general/timetelemetry.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/specific/planetscomparesonification.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/specific/planetsoverviewsonification.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/specific/planetssonification.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/specific/planetssonification_lua.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/src/telemetrybase.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/util.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Telemetry"
  telemetry_module
  STATIC
  ${HEADER_FILES}
  ${SOURCE_FILES}
)
