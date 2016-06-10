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

    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunkedlodglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunknode.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunkindex.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunk.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunkrenderer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling.h


    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/skirtedgrid.h

    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodetic2.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/angle.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/ellipsoid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/aabb.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/convexhull.h

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/temporaltileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledataset.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/asynctilereader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovidermanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/layeredtextureshaderprovider.h

    ${CMAKE_CURRENT_SOURCE_DIR}/other/distanceswitch.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lrucache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentjobmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/threadpool.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentqueue.h
    

)

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/renderableglobe.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunkedlodglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunknode.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunkindex.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunk.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunkrenderer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling.cpp


    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/skirtedgrid.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodetic2.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/angle.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/ellipsoid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/aabb.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/convexhull.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/temporaltileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledataset.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/asynctilereader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovidermanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/layeredtextureshaderprovider.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/other/distanceswitch.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lrucache.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentjobmanager.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/threadpool.cpp
    
   
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/blending.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/ellipsoid.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tile.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/texturetilemapping.hglsl

    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalchunkedlodpatch_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalchunkedlodpatch_fs.glsl

    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/localchunkedlodpatch_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/localchunkedlodpatch_fs.glsl
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
