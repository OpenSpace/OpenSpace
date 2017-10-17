##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2017                                                                #
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
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/multimodelgeometry.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemodel.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplane.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphere.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphericalgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrail.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailorbit.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailtrajectory.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceframebuffer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimagelocal.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimageonline.h
    ${CMAKE_CURRENT_SOURCE_DIR}/translation/statictranslation.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/staticrotation.h
    ${CMAKE_CURRENT_SOURCE_DIR}/scale/staticscale.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/multimodelgeometry.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablemodel.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphere.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesphericalgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrail.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailorbit.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabletrailtrajectory.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceframebuffer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimagelocal.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/screenspaceimageonline.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/translation/statictranslation.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rotation/staticrotation.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/scale/staticscale.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/imageplane_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/imageplane_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/model_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/model_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/path_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/path_gs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/path_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/plane_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/plane_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/pscstandard_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/pscstandard_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderabletrail_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderabletrail_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/screenspace_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/screenspace_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/sphere_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "Base"
    base_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
