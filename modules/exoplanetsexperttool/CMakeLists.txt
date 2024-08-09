##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2022                                                                #
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
    columnfilter.h
    datahelper.h
    dataloader.h
    datastructures.h
    dataviewer.h
    exoplanetsexperttoolmodule.h
    gui.h
    rendering/renderableexoplanetglyphcloud.h
    views/colormappingview.h
    views/columnselectionview.h
    views/viewhelper.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    columnfilter.cpp
    datahelper.cpp
    dataloader.cpp
    dataviewer.cpp
    exoplanetsexperttoolmodule.cpp
    gui.cpp
    rendering/renderableexoplanetglyphcloud.cpp
    views/colormappingview.cpp
    views/columnselectionview.cpp
    views/viewhelper.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/glyphs_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/glyphs_gs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/glyphs_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gui_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gui_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/points_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/points_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "ExoplanetsExpertTool"
    exoplanets_expert_tool_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

add_subdirectory(ext/implot SYSTEM)
target_link_libraries(${exoplanets_expert_tool_module} PRIVATE Implot)
set_target_properties(Implot PROPERTIES FOLDER "External")

