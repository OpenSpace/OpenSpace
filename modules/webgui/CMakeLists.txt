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
include(../webbrowser/cmake/webbrowser_helpers.cmake)

set(WEBGUI_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "WEBGUI_MODULE_PATH")

set(OPENSPACE_HEADER_FILES
  webguimodule.h
)
source_group("Header Files" FILES ${OPENSPACE_HEADER_FILES})

set(OPENSPACE_SOURCE_FILES
  webguimodule.cpp
)
source_group("Source Files" FILES ${OPENSPACE_SOURCE_FILES})

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Specify the NodeJs distribution version.
set(NODEJS_VERSION "8.11.4")

include(nodejs_support)
DownloadNodeJs("${NODEJS_VERSION}" "${CMAKE_CURRENT_SOURCE_DIR}/ext/nodejs")

create_new_module(
  WebGui
  webgui_module
  STATIC
  ${OPENSPACE_HEADER_FILES} ${OPENSPACE_SOURCE_FILES}
)

set_modules_dependency_on_cef_libraries(${webgui_module})
