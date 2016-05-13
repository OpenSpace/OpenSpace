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

    ${CMAKE_CURRENT_SOURCE_DIR}/globes/renderableglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/globemesh.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/clipmapglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/clipmappyramid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkedlodglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunknode.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkindex.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunk.h

    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/clipmapgrid.h

    ${CMAKE_CURRENT_SOURCE_DIR}/geodetics/geodetic2.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geodetics/angle.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geodetics/ellipsoid.h

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/patchrenderer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/culling.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/aabb.h

    ${CMAKE_CURRENT_SOURCE_DIR}/other/distanceswitch.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/texturetileset.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/patchcoverageprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/tileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/gdaldataconverter.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lrucache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentjobmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentqueue.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/tileprovidermanager.h

)

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/renderableglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/globemesh.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/clipmapglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/clipmappyramid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkedlodglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunknode.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkindex.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunk.cpp


    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/clipmapgrid.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/geodetics/geodetic2.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geodetics/angle.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/geodetics/ellipsoid.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/patchrenderer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/culling.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/aabb.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/other/distanceswitch.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/texturetileset.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/patchcoverageprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/tileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/gdaldataconverter.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lrucache.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentjobmanager.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/tileprovidermanager.cpp
   
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/ellipsoid.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/texturetile.hglsl

    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalchunkedlodpatch_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalchunkedlodpatch_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalclipmappatch_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalclipmappatch_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/localclipmappatch_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/simple_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/simple_fs.glsl
)

source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "GlobeBrowsing"
    globebrowsing_module
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

target_include_directories(
    openspace-module-globebrowsing
    SYSTEM PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/include
)

target_link_libraries(
    openspace-module-globebrowsing
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal_i.lib
)
