set(HEADER_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefieldlines.h
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
	${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/dynamicephemeris.h
	${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/spiceephemeris.h
	${CMAKE_CURRENT_SOURCE_DIR}/ephemeris/staticephemeris.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefieldlines.cpp
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
	${CMAKE_CURRENT_SOURCE_DIR}/shaders/fieldline_fs.glsl
	${CMAKE_CURRENT_SOURCE_DIR}/shaders/fieldline_gs.glsl
	${CMAKE_CURRENT_SOURCE_DIR}/shaders/fieldline_vs.glsl
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

set(MODULE_CLASS_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/basemodule.h
	${CMAKE_CURRENT_SOURCE_DIR}/basemodule.cpp
)

include_directories(${OPENSPACE_BASE_DIR}/include ${OPENSPACE_BASE_DIR})

add_library(openspace-module-base ${HEADER_FILES} ${SOURCE_FILES} ${MODULE_CLASS_FILES})
