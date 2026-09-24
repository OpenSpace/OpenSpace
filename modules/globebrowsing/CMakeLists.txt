##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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
  globebrowsingmodule.h
  src/asynctiledataprovider.h
  src/basictypes.h
  src/dashboarditemglobelocation.h
  src/gdalwrapper.h
  src/geodeticpatch.h
  src/globelabelscomponent.h
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
  src/tilecacheproperties.h
  src/timequantizer.h
  src/geojson/geojsoncomponent.h
  src/geojson/geojsonmanager.h
  src/geojson/geojsonproperties.h
  src/geojson/globegeometryfeature.h
  src/geojson/globegeometryhelper.h
  src/tileprovider/defaulttileprovider.h
  src/tileprovider/imagesequencetileprovider.h
  src/tileprovider/singleimagetileprovider.h
  src/tileprovider/sizereferencetileprovider.h
  src/tileprovider/spoutimageprovider.h
  src/tileprovider/temporaltileprovider.h
  src/tileprovider/texttileprovider.h
  src/tileprovider/tileindextileprovider.h
  src/tileprovider/tileprovider.h
  src/tileprovider/tileproviderbydate.h
  src/tileprovider/tileproviderbyindex.h
  src/tileprovider/tileproviderbylevel.h
)

set(SOURCE_FILES
  globebrowsingmodule.cpp
  globebrowsingmodule_lua.inl
  src/asynctiledataprovider.cpp
  src/dashboarditemglobelocation.cpp
  src/gdalwrapper.cpp
  src/geodeticpatch.cpp
  src/globelabelscomponent.cpp
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
  src/geojson/geojsoncomponent.cpp
  src/geojson/geojsonmanager.cpp
  src/geojson/geojsonproperties.cpp
  src/geojson/globegeometryfeature.cpp
  src/geojson/globegeometryhelper.cpp
  src/tileprovider/defaulttileprovider.cpp
  src/tileprovider/imagesequencetileprovider.cpp
  src/tileprovider/singleimagetileprovider.cpp
  src/tileprovider/sizereferencetileprovider.cpp
  src/tileprovider/spoutimageprovider.cpp
  src/tileprovider/temporaltileprovider.cpp
  src/tileprovider/texttileprovider.cpp
  src/tileprovider/tileindextileprovider.cpp
  src/tileprovider/tileprovider.cpp
  src/tileprovider/tileproviderbydate.cpp
  src/tileprovider/tileproviderbyindex.cpp
  src/tileprovider/tileproviderbylevel.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/advanced_rings_vs.glsl
  shaders/advanced_rings_fs.glsl
  shaders/blending.glsl
  shaders/ellipsoid.glsl
  shaders/geojson_fs.glsl
  shaders/geojson_points_fs.glsl
  shaders/geojson_points_gs.glsl
  shaders/geojson_points_vs.glsl
  shaders/geojson_vs.glsl
  shaders/globalrenderer_vs.glsl
  shaders/interpolate_fs.glsl
  shaders/interpolate_vs.glsl
  shaders/localrenderer_vs.glsl
  shaders/renderer_fs.glsl
  shaders/rings_fs.glsl
  shaders/rings_geom_fs.glsl
  shaders/rings_geom_vs.glsl
  shaders/rings_vs.glsl
  shaders/texturetilemapping.glsl
  shaders/tile.glsl
  shaders/tileheight.glsl
  shaders/tilevertexskirt.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "GlobeBrowsing"
  globebrowsing_module
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
target_precompile_headers(${globebrowsing_module} PRIVATE
  <modules/globebrowsing/src/tileprovider/tileprovider.h>
  <modules/globebrowsing/globebrowsingmodule.h>
  <future>
  <map>
)

# Gdal
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/gdal_data DESTINATION modules/globebrowsing)

begin_dependency("GDAL")
find_package(GDAL CONFIG REQUIRED)
if (MSVC AND TARGET LERC::LERC)
  # GDAL's vendored FindLERC.cmake module (used to locate its optional Lerc dependency) is
  # a plain find_library call with no debug/release awareness, so it caches a single
  # IMPORTED_LOCATION shared by every configuration in this multi-config generator,
  # causing LNK2038 CRT/runtime-library mismatches whenever it happens to resolve to
  # the "wrong" variant for the config being linked. Give it proper per-config locations
  # instead, matching how the other vcpkg CONFIG packages (e.g. GEOS) already behave.
  set_target_properties(LERC::LERC PROPERTIES
    IMPORTED_LOCATION_DEBUG "${VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/debug/lib/Lerc.lib"
    IMPORTED_LOCATION_RELEASE "${VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/Lerc.lib"
    IMPORTED_CONFIGURATIONS "DEBUG;RELEASE"
  )
endif ()
if (MSVC AND TARGET PkgConfig::PC_SPATIALITE)
  # GDAL's SPATIALITE dependency is resolved through pkg-config (FindSPATIALITE.cmake),
  # which produces one flat, non-config-aware list of absolute library paths. Depending
  # on which PKG_CONFIG_PATH happened to be active at configure time, that list can
  # contain the debug variant of geos/geos_c even when linking Release/RelWithDebInfo
  # (or vice versa), causing LNK2038 CRT mismatches. We already link the proper,
  # per-config-aware GEOS::geos/GEOS::geos_c targets below, so drop pkg-config's raw
  # geos/geos_c entries here instead of keeping two conflicting copies in sync.
  get_target_property(_globebrowsing_spatialite_libs PkgConfig::PC_SPATIALITE INTERFACE_LINK_LIBRARIES)
  list(FILTER _globebrowsing_spatialite_libs EXCLUDE REGEX "/geos(_c)?\\.lib$")
  set_target_properties(PkgConfig::PC_SPATIALITE PROPERTIES
    INTERFACE_LINK_LIBRARIES "${_globebrowsing_spatialite_libs}"
  )
  unset(_globebrowsing_spatialite_libs)
endif ()
target_link_libraries(openspace-module-globebrowsing PRIVATE GDAL::GDAL)
end_dependency("GDAL")

begin_dependency("GEOS")
find_package(GEOS CONFIG REQUIRED)
target_link_libraries(openspace-module-globebrowsing PRIVATE GEOS::geos)
if (MSVC AND TARGET GEOS::geos_c)
  # Re-add geos_c (stripped from spatialite's pkg-config link line above) via the
  # proper per-config-aware target so its symbols are still resolved correctly.
  target_link_libraries(openspace-module-globebrowsing PRIVATE GEOS::geos_c)
endif ()
end_dependency("GEOS")
