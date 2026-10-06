##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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
  rendering/renderablesolarimagery.h
  rendering/renderablesolarimageryprojection.h
  tasks/helioviewerdownloadtask.h
  util/asyncimagedecoder.h
  util/format_defs.h
  util/j2kcodec.h
  util/solarbrowsinghelper.h
  util/structs.h
)

source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  rendering/renderablesolarimagery.cpp
  rendering/renderablesolarimageryprojection.cpp
  tasks/helioviewerdownloadtask.cpp
  util/asyncimagedecoder.cpp
  util/j2kcodec.cpp
  util/solarbrowsinghelper.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/spacecraftimagefrustum_fs.glsl
  shaders/spacecraftimagefrustum_vs.glsl
  shaders/spacecraftimageplane_fs.glsl
  shaders/spacecraftimageplane_vs.glsl
  shaders/spacecraftimageprojection_fs.glsl
  shaders/spacecraftimageprojection_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "SolarBrowsing"
  solarbrowsing_module
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

begin_dependency("OpenJPEG")
find_package(OpenJPEG CONFIG REQUIRED)
target_link_libraries(openspace-module-solarbrowsing PRIVATE openjp2)
end_dependency("OpenJPEG")
