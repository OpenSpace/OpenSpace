set(HEADER_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometryprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecrawlingline.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefov.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanetprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometryprojection.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/writeToTexture.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/decoder.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/hongkangparser.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer2.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/instrumentdecoder.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/labelparser.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/scannerdecoder.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/sequenceparser.h
	${CMAKE_CURRENT_SOURCE_DIR}/util/targetdecoder.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/planetgeometryprojection.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecrawlingline.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablefov.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplaneprojection.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableplanetprojection.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/simplespheregeometryprojection.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/decoder.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/hongkangparser.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/imagesequencer2.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/instrumentdecoder.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/labelparser.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/scannerdecoder.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/sequenceparser.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/util/targetdecoder.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(MODULE_CLASS_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/newhorizonsmodule.h
	${CMAKE_CURRENT_SOURCE_DIR}/newhorizonsmodule.cpp
)

include_directories(${OPENSPACE_BASE_DIR}/include ${OPENSPACE_BASE_DIR})

add_library(openspace-module-newhorizons ${HEADER_FILES} ${SOURCE_FILES} ${MODULE_CLASS_FILES})
