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

set(HEADER_FILES


    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemarsrover.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspacedashboard.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceframebuffer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableheightmap.h>

    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/fixedrotation.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/luarotation.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/staticrotation.h

    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/advancedspicerotation.h

    ${CMAKE_CURRENT_SOURCE_DIR}/heighthandler/extractheightmap.h

    ${CMAKE_CURRENT_SOURCE_DIR}/scale/luascale.h
    ${CMAKE_CURRENT_SOURCE_DIR}/scale/staticscale.h

)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemarsrover.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspacedashboard.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceframebuffer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableheightmap.cpp>

    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/fixedrotation.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/luarotation.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/staticrotation.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/advancedspicerotation.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/heighthandler/extractheightmap.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/scale/luascale.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/scale/staticscale.cpp

)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/heightmap_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/heightmap_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "Marsrover"
    marsrover_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
