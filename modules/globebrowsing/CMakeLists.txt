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
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunk.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunknode.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling.h

    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/aabb.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/angle.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/angle.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/convexhull.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/ellipsoid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodetic2.h

    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkedlodglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/pointglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/renderableglobe.h

    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/skirtedgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.h

    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentjobmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentjobmanager.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentqueue.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/concurrentqueue.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/distanceswitch.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lrucache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lrucache.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/statscollector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/statscollector.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/threadpool.h

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/chunkrenderer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpulayermanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layermanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layershadermanager.h

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/asynctilereader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/chunktile.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/pixelregion.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tile.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledataset.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledatatype.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledepthtransform.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tilediskcache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileindex.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileselector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/cachingtileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/singleimageprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/temporaltileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/texttileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbyindex.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbylevel.h
)

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunk.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunknode.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/aabb.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/convexhull.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/ellipsoid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodetic2.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkedlodglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/pointglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/renderableglobe.cpp
    
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/skirtedgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/other/distanceswitch.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/statscollector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/threadpool.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/chunkrenderer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpulayermanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layershadermanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layermanager.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/asynctilereader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/pixelregion.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tile.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledataset.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledatatype.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tilediskcache.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileindex.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileselector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/cachingtileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/singleimageprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/temporaltileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/texttileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbyindex.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbylevel.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/blending.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/ellipsoid.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalchunkedlodpatch_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalchunkedlodpatch_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/localchunkedlodpatch_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/localchunkedlodpatch_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/pointglobe_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/pointglobe_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/texturetilemapping.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tile.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tilefragcolor.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tilevertexheight.hglsl
)

source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "GlobeBrowsing"
    globebrowsing_module
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

if (WIN32)
    target_include_directories(
        openspace-module-globebrowsing
        SYSTEM PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/include
    )

    target_link_libraries(
        openspace-module-globebrowsing
        ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal_i.lib
    )

    set(EXTERNAL_LIBRARY "${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal201.dll" PARENT_SCOPE)

else (WIN32)
    find_package(GDAL REQUIRED)

    target_include_directories(
        openspace-module-globebrowsing
        SYSTEM PUBLIC
        ${GDAL_INCLUDE_DIR}
    )

    target_link_libraries(
        openspace-module-globebrowsing
        ${GDAL_LIBRARY}
    )
endif ()
