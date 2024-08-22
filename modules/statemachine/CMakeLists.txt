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

set(HEADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/include/state.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/transition.h
  ${CMAKE_CURRENT_SOURCE_DIR}/include/statemachine.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/statemachinemodule_lua.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/src/state.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/transition.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/statemachine.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "StateMachine"
  statemachine_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)
target_precompile_headers(${statemachine_module} PRIVATE
  <ghoul/glm.h>
  <ghoul/lua/ghoul_lua.h>
  <chrono>
  <filesystem>
  <string>
)
