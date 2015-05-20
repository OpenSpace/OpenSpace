set(HEADER_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecrawlingline.h
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
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/modelgeometry.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometry.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableconstellationbounds.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecrawlingline.cpp
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
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(MODULE_CLASS_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/basemodule.h
	${CMAKE_CURRENT_SOURCE_DIR}/basemodule.cpp
)

include_directories(${OPENSPACE_BASE_DIR}/include ${OPENSPACE_BASE_DIR})

add_library(openspace-module-base ${HEADER_FILES} ${SOURCE_FILES} ${MODULE_CLASS_FILES})
