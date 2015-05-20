set(HEADER_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometryprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefov.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanetprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometryprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/writeToTexture.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometryprojection.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefov.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneprojection.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanetprojection.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometryprojection.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(MODULE_CLASS_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/newhorizonsmodule.h
	${CMAKE_CURRENT_SOURCE_DIR}/newhorizonsmodule.cpp
)

include_directories(${OPENSPACE_BASE_DIR}/include ${OPENSPACE_BASE_DIR})

add_library(openspace-module-newhorizons ${HEADER_FILES} ${SOURCE_FILES} ${MODULE_CLASS_FILES})
