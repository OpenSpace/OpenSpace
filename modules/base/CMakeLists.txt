#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2016                                                               #
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
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemodel.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablepath.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplane.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanet.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphere.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphericalgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablestars.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrail.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometry.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/wavefrontgeometry.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimage.h
    ${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/dynamicephemeris.h
    ${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/spiceephemeris.h
    ${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/staticephemeris.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemodel.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablepath.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanet.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphere.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphericalgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablestars.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrail.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometry.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/wavefrontgeometry.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimage.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/dynamicephemeris.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/spiceephemeris.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/staticephemeris.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/constellationbounds_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/constellationbounds_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/ephemeris_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/ephemeris_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/imageplane_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/imageplane_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/model_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/model_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/path_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/path_gs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/path_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/plane_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/plane_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/star_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/star_ge.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/star_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "Base"
    base_module
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
