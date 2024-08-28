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

set(CEFWEBGUI_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "CEFWEBGUI_MODULE_PATH")
set(CEFWEBGUI_SOURCE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/web CACHE INTERNAL "CEFWEBGUI_MODULE_PATH")

set(OPENSPACE_HEADER_FILES
  cefwebguimodule.h
  include/guirenderhandler.h
  include/guikeyboardhandler.h
)
source_group("Header Files" FILES ${OPENSPACE_HEADER_FILES})

set(OPENSPACE_SOURCE_FILES
  cefwebguimodule.cpp
  src/guirenderhandler.cpp
  src/guikeyboardhandler.cpp
)
source_group("Source Files" FILES ${OPENSPACE_SOURCE_FILES})

create_new_module(
  CefWebGui
  cefwebgui_module
  ${OPENSPACE_HEADER_FILES} ${OPENSPACE_SOURCE_FILES}
)

target_precompile_headers(${cefwebgui_module} PRIVATE
  [["include/capi/cef_base_capi.h"]]
  [["include/cef_render_handler.h"]]
  <string>
  <sstream>
  <istream>
  <ostream>
)
if (WIN32)
  target_precompile_headers(${cefwebgui_module} PRIVATE
    <Windows.h>
  )
endif ()

set_modules_dependency_on_cef_libraries(${cefwebgui_module})
