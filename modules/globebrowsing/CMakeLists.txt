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

include(${OPENSPACE_CMAKE_EXT_DIR}/module_definition.cmake)

set(HEADER_FILES
  globebrowsingmodule.h
  src/asynctiledataprovider.h
  src/basictypes.h
  src/dashboarditemglobelocation.h
  src/ellipsoid.h
  src/gdalwrapper.h
  src/geodeticpatch.h
  src/globelabelscomponent.h
  src/globetranslation.h
  src/globerotation.h
  src/gpulayergroup.h
  src/layer.h
  src/layeradjustment.h
  src/layergroup.h
  src/layergroupid.h
  src/layermanager.h
  src/layerrendersettings.h
  src/lrucache.h
  src/lrucache.inl
  src/lruthreadpool.h
  src/lruthreadpool.inl
  src/memoryawaretilecache.h
  src/prioritizingconcurrentjobmanager.h
  src/prioritizingconcurrentjobmanager.inl
  src/rawtile.h
  src/rawtiledatareader.h
  src/renderableglobe.h
  src/ringscomponent.h
  src/shadowcomponent.h
  src/skirtedgrid.h
  src/tileindex.h
  src/tileloadjob.h
  src/tiletextureinitdata.h
  src/timequantizer.h
  src/tileprovider/defaulttileprovider.h
  src/tileprovider/videotileprovider.h
  src/tileprovider/imagesequencetileprovider.h
  src/tileprovider/singleimagetileprovider.h
  src/tileprovider/sizereferencetileprovider.h
  src/tileprovider/spoutimageprovider.h
  src/tileprovider/temporaltileprovider.h
  src/tileprovider/texttileprovider.h
  src/tileprovider/tileindextileprovider.h
  src/tileprovider/tileprovider.h
  src/tileprovider/tileproviderbyindex.h
  src/tileprovider/tileproviderbylevel.h
)

set(SOURCE_FILES
  globebrowsingmodule.cpp
  globebrowsingmodule_lua.inl
  src/asynctiledataprovider.cpp
  src/dashboarditemglobelocation.cpp
  src/ellipsoid.cpp
  src/gdalwrapper.cpp
  src/geodeticpatch.cpp
  src/globelabelscomponent.cpp
  src/globetranslation.cpp
  src/globerotation.cpp
  src/gpulayergroup.cpp
  src/layer.cpp
  src/layeradjustment.cpp
  src/layergroup.cpp
  src/layergroupid.cpp
  src/layermanager.cpp
  src/layerrendersettings.cpp
  src/memoryawaretilecache.cpp
  src/rawtiledatareader.cpp
  src/renderableglobe.cpp
  src/ringscomponent.cpp
  src/shadowcomponent.cpp
  src/skirtedgrid.cpp
  src/tileindex.cpp
  src/tileloadjob.cpp
  src/tiletextureinitdata.cpp
  src/timequantizer.cpp
  src/tileprovider/defaulttileprovider.cpp
  src/tileprovider/videotileprovider.cpp
  src/tileprovider/imagesequencetileprovider.cpp
  src/tileprovider/singleimagetileprovider.cpp
  src/tileprovider/sizereferencetileprovider.cpp
  src/tileprovider/spoutimageprovider.cpp
  src/tileprovider/temporaltileprovider.cpp
  src/tileprovider/texttileprovider.cpp
  src/tileprovider/tileindextileprovider.cpp
  src/tileprovider/tileprovider.cpp
  src/tileprovider/tileproviderbyindex.cpp
  src/tileprovider/tileproviderbylevel.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/advanced_rings_vs.glsl
  shaders/advanced_rings_fs.glsl
  shaders/blending.glsl
  shaders/globalrenderer_vs.glsl
  shaders/localrenderer_vs.glsl
  shaders/renderer_fs.glsl
  shaders/rings_vs.glsl
  shaders/rings_fs.glsl
  shaders/rings_geom_vs.glsl
  shaders/rings_geom_fs.glsl
  shaders/texturetilemapping.glsl
  shaders/tile.glsl
  shaders/tileheight.glsl
  shaders/tilevertexskirt.glsl
)

source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "GlobeBrowsing"
  globebrowsing_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
target_precompile_headers(${globebrowsing_module} PRIVATE
  <modules/globebrowsing/src/tileprovider/tileprovider.h>
  <modules/globebrowsing/globebrowsingmodule.h>
  <future>
  <map>
)

# Ffmpeg
if(WIN32)
  add_library(libmpv SHARED IMPORTED)
  set_target_properties(libmpv PROPERTIES
    IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/lib/mpv-2.dll"
    IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/lib/mpv.lib"
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/include"
  )

  target_link_libraries(openspace-module-globebrowsing PUBLIC libmpv)

  add_library(avcodec SHARED IMPORTED)
  set_target_properties(avcodec PROPERTIES
    IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/avcodec-59.dll"
    IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/avcodec.lib"
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/include"
  )

  target_link_libraries(openspace-module-globebrowsing PUBLIC avcodec)

  add_library(avformat SHARED IMPORTED)
  set_target_properties(avformat PROPERTIES
    IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/avformat-59.dll"
    IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/avformat.lib"
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/include"
  )

  target_link_libraries(openspace-module-globebrowsing PUBLIC avformat)

  add_library(avutil SHARED IMPORTED)
  set_target_properties(avutil PROPERTIES
    IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/avutil-57.dll"
    IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/avutil.lib"
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/include"
  )

  target_link_libraries(openspace-module-globebrowsing PUBLIC avutil)

  add_library(swscale SHARED IMPORTED)
  set_target_properties(swscale PROPERTIES
    IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/swscale-6.dll"
    IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/lib/swscale.lib"
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/ext/ffmpeg/include"
  )

  target_link_libraries(openspace-module-globebrowsing PUBLIC swscale)

else(WIN32)
  find_package(FFMPEG REQUIRED)

  target_include_directories(openspace-module-globebrowsing SYSTEM PRIVATE ${FFMPEG_INCLUDE_DIR})
  target_link_libraries(openspace-module-globebrowsing PRIVATE ${FFMPEG_LIBRARY})
  mark_as_advanced(FFMPEG_CONFIG FFMPEG_INCLUDE_DIR FFMPEG_LIBRARY)
endif () # WIN32

# Gdal
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/gdal_data DESTINATION modules/globebrowsing)

if (WIN32)
  add_library(gdal SHARED IMPORTED)
  target_include_directories(gdal SYSTEM INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/include)
  set_target_properties(gdal PROPERTIES IMPORTED_IMPLIB ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal_i.lib)
  set_target_properties(gdal PROPERTIES IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal241.dll)
  target_link_libraries(openspace-module-globebrowsing PRIVATE gdal)
else (WIN32)
  find_package(GDAL REQUIRED)

  target_include_directories(openspace-module-globebrowsing SYSTEM PRIVATE ${GDAL_INCLUDE_DIR})
  target_link_libraries(openspace-module-globebrowsing PRIVATE ${GDAL_LIBRARY})
  mark_as_advanced(GDAL_CONFIG GDAL_INCLUDE_DIR GDAL_LIBRARY)
endif () # WIN32
