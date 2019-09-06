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
  ${CMAKE_CURRENT_SOURCE_DIR}/src/asynctiledataprovider.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/basictypes.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/dashboarditemglobelocation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/ellipsoid.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/gdalwrapper.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/geodeticpatch.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/globelabelscomponent.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/globetranslation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/gpulayergroup.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layer.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layeradjustment.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layergroup.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layergroupid.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layermanager.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layerrendersettings.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/lrucache.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/lrucache.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/src/lruthreadpool.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/lruthreadpool.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/src/memoryawaretilecache.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/prioritizingconcurrentjobmanager.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/prioritizingconcurrentjobmanager.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/src/rawtile.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/rawtiledatareader.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/renderableglobe.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/skirtedgrid.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tileindex.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tileloadjob.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tileprovider.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tiletextureinitdata.h
  ${CMAKE_CURRENT_SOURCE_DIR}/src/timequantizer.h
)

set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/globebrowsingmodule.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/globebrowsingmodule_lua.inl
  ${CMAKE_CURRENT_SOURCE_DIR}/src/asynctiledataprovider.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/dashboarditemglobelocation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/ellipsoid.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/gdalwrapper.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/geodeticpatch.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/globelabelscomponent.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/globetranslation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/gpulayergroup.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layer.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layeradjustment.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layergroup.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layergroupid.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layermanager.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/layerrendersettings.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/memoryawaretilecache.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/rawtile.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/rawtiledatareader.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/renderableglobe.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/skirtedgrid.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tileindex.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tileloadjob.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tileprovider.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/tiletextureinitdata.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/timequantizer.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/blending.hglsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/globalrenderer_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/localrenderer_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderer_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/texturetilemapping.hglsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tile.hglsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tileheight.hglsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/tilevertexskirt.hglsl
)

source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "GlobeBrowsing"
  globebrowsing_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

option(OPENSPACE_MODULE_GLOBEBROWSING_INSTRUMENTATION "Instrumentation for GlobeBrowsing Performance" OFF)
if (OPENSPACE_MODULE_GLOBEBROWSING_INSTRUMENTATION)
  target_compile_definitions(openspace-module-globebrowsing INTERFACE "OPENSPACE_MODULE_GLOBEBROWSING_INSTRUMENTATION")
endif ()

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/gdal_data DESTINATION modules/globebrowsing)

if (WIN32)
  target_include_directories(
    openspace-module-globebrowsing
    SYSTEM PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/include
  )

  target_link_libraries(
    openspace-module-globebrowsing
    PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal_i.lib
  )
  register_external_libraries("${CMAKE_CURRENT_SOURCE_DIR}/ext/gdal/lib/gdal241.dll")
else (WIN32)
  find_package(GDAL REQUIRED)

  target_include_directories(
    openspace-module-globebrowsing
    SYSTEM PUBLIC
    ${GDAL_INCLUDE_DIR}
  )

  target_link_libraries(openspace-module-globebrowsing PRIVATE ${GDAL_LIBRARY})

  mark_as_advanced(GDAL_CONFIG GDAL_INCLUDE_DIR GDAL_LIBRARY)
endif () # WIN32
