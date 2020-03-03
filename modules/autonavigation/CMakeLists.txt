##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2019                                                                #
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
  ${CMAKE_CURRENT_SOURCE_DIR}/atnodenavigator.h
  ${CMAKE_CURRENT_SOURCE_DIR}/autonavigationmodule.h
  ${CMAKE_CURRENT_SOURCE_DIR}/autonavigationhandler.h
  ${CMAKE_CURRENT_SOURCE_DIR}/avoidcollisioncurve.h
  ${CMAKE_CURRENT_SOURCE_DIR}/helperfunctions.h
  ${CMAKE_CURRENT_SOURCE_DIR}/instruction.h
  ${CMAKE_CURRENT_SOURCE_DIR}/pathspecification.h
  ${CMAKE_CURRENT_SOURCE_DIR}/pathsegment.h
  ${CMAKE_CURRENT_SOURCE_DIR}/pathcurves.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rotationinterpolator.h
  ${CMAKE_CURRENT_SOURCE_DIR}/speedfunction.h
  ${CMAKE_CURRENT_SOURCE_DIR}/waypoint.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/atnodenavigator.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/autonavigationmodule.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/autonavigationmodule_lua.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/autonavigationhandler.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/avoidcollisioncurve.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/helperfunctions.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/instruction.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/pathspecification.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/pathsegment.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/pathcurves.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rotationinterpolator.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/speedfunction.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/waypoint.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "AutoNavigation"
  autonavigation_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)
