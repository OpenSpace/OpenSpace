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
    ${CMAKE_CURRENT_SOURCE_DIR}/globebrowsingmodule.h
    
    ${CMAKE_CURRENT_SOURCE_DIR}/cache/lrucache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/cache/lrucache.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/cache/memoryawaretilecache.h
    ${CMAKE_CURRENT_SOURCE_DIR}/cache/texturecontainer.h

    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunk.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunknode.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator/chunklevelevaluator.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator/availabletiledataevaluator.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator/distanceevaluator.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator/projectedareaevaluator.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling/chunkculler.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling/frustumculler.h
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling/horizonculler.h

    ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemglobelocation.h

    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/aabb.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/angle.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/angle.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/ellipsoid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodetic2.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodetic3.h
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodeticpatch.h

    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkedlodglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/pointglobe.h
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/renderableglobe.h

    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/skirtedgrid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.h

    ${CMAKE_CURRENT_SOURCE_DIR}/other/prioritizingconcurrentjobmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/prioritizingconcurrentjobmanager.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/pixelbuffer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/pixelbuffercontainer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/pixelbuffercontainer.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/statscollector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/templatedstatscollector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/templatedstatscollector.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/other/timequantizer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lruthreadpool.h
    ${CMAKE_CURRENT_SOURCE_DIR}/other/lruthreadpool.inl

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/chunkrenderer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layershadermanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpuchunktile.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpuchunktilepile.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpuheightlayer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayeradjustment.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayergroup.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayermanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayerrendersettings.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gputiledepthtransform.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gputileuvtransform.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layer.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layeradjustment.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layergroup.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layergroupid.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layermanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layerrendersettings.h

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/asynctiledataprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/chunktile.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/pixelregion.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtile.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/textureformat.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tile.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiledepthtransform.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileindex.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tilemetadata.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileselector.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileuvtransform.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileloadjob.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/defaulttileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/singleimageprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/sizereferencetileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/temporaltileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/texttileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileindextileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileprovider.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbyindex.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbylevel.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiletextureinitdata.h

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/rawtiledatareader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/gdalrawtiledatareader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/simplerawtiledatareader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/gdalwrapper.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/iodescription.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/tiledatatype.h
)

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/globebrowsingmodule.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globebrowsingmodule_lua.inl

    ${CMAKE_CURRENT_SOURCE_DIR}/cache/memoryawaretilecache.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/cache/texturecontainer.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunk.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunknode.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator/availabletiledataevaluator.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator/distanceevaluator.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/chunklevelevaluator/projectedareaevaluator.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling/frustumculler.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/chunk/culling/horizonculler.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/dashboard/dashboarditemglobelocation.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/aabb.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/ellipsoid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodetic2.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/geometry/geodeticpatch.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/globes/chunkedlodglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/pointglobe.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/globes/renderableglobe.cpp
    
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/basicgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/grid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/skirtedgrid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/meshes/trianglesoup.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/other/pixelbuffer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/statscollector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/other/timequantizer.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/chunkrenderer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layershadermanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpuchunktile.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpuchunktilepile.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpuheightlayer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayeradjustment.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayergroup.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayermanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gpulayerrendersettings.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gputiledepthtransform.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/gpu/gputileuvtransform.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layeradjustment.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layergroup.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layergroupid.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layermanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/layer/layerrendersettings.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/asynctiledataprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/pixelregion.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtile.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tile.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileindex.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tilemetadata.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileselector.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileloadjob.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/defaulttileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/singleimageprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/sizereferencetileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/temporaltileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/texttileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileindextileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileprovider.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbyindex.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tileprovider/tileproviderbylevel.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/tiletextureinitdata.cpp

    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/rawtiledatareader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/gdalrawtiledatareader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/simplerawtiledatareader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/gdalwrapper.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/iodescription.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tile/rawtiledatareader/tiledatatype.cpp
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
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tilefragment.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tileheight.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tilevertexskirt.hglsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globeshading.hglsl
)

source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "GlobeBrowsing"
    globebrowsing_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

option(OPENSPACE_MODULE_GLOBEBROWSING_USE_GDAL "Use GDAL" ON)

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/gdal_data DESTINATION modules/globebrowsing)

if (OPENSPACE_MODULE_GLOBEBROWSING_USE_GDAL)
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
        register_external_libraries("${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal202.dll")
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
    endif () # WIN32
    target_compile_definitions(openspace-module-globebrowsing PUBLIC GLOBEBROWSING_USE_GDAL)
endif () # OPENSPACE_MODULE_GLOBEBROWSING_USE_GDAL
