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
  skybrowsermodule.h
  include/renderableskytarget.h
  include/wwtdatahandler.h
  include/utility.h
  include/targetbrowserpair.h
  include/wwtcommunicator.h
  include/browser.h
  include/screenspaceskybrowser.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  skybrowsermodule.cpp
  skybrowsermodule_lua.inl
  src/renderableskytarget.cpp
  src/wwtdatahandler.cpp
  src/utility.cpp
  src/targetbrowserpair.cpp
  src/wwtcommunicator.cpp
  src/browser.cpp
  src/screenspaceskybrowser.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    shaders/target_fs.glsl
    shaders/target_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "SkyBrowser"
  skybrowser_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
target_precompile_headers(${skybrowser_module} PRIVATE
  [["include/cef_accessibility_handler.h"]]
  [["include/cef_render_handler.h"]]
)

# This introduces a dependency into SGCT, but the way it is handled is not great right now
# This will break when we remove tinyxml2 from SGCT and then we can move the submodule
# over into this ext folder and fix it well
target_link_libraries(${skybrowser_module} PRIVATE tinyxml2)
