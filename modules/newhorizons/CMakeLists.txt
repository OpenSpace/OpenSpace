#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2015                                                               #
#                                                                                       #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this  #
# software and associated documentation files (the "Software"), to deal in the Software #
# without restriction, including without limitation the rights to use, copy, modify,    #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    #
# permit persons to whom the Software is furnished to do so, subject to the following   #
# conditions:                                                                           #
#                                                                                       #
# The above copyright notice and this permission notice shall be included in all copies #
# or substantial portions of the Software.                                              #
#                                                                                       #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         #
#########################################################################################

include(${OPENSPACE_CMAKE_EXT_DIR}/module_definition.cmake)

set(HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometryprojection.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecrawlingline.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefov.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneprojection.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanetprojection.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableshadowcylinder.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometryprojection.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/writeToTexture.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/decoder.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/hongkangparser.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer2.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/instrumentdecoder.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/labelparser.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/scannerdecoder.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/sequenceparser.h
    ${CMAKE_CURRENT_SOURCE_DIR}/util/targetdecoder.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometryprojection.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecrawlingline.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefov.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneprojection.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanetprojection.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableshadowcylinder.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometryprojection.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/decoder.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/hongkangparser.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer2.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/instrumentdecoder.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/labelparser.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/scannerdecoder.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/sequenceparser.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util/targetdecoder.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/crawlingline_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/crawlingline_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/fov_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/fov_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/projectiveTexture_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/projectiveTexture_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/terminatorshadow_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/terminatorshadow_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "NewHorizons"
    newhorizons_module
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
