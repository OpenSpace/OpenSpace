set(HEADER_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablevolume.h
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablevolumegl.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablevolume.cpp
	${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablevolumegl.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(MODULE_CLASS_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/volumemodule.h
	${CMAKE_CURRENT_SOURCE_DIR}/volumemodule.cpp
)

include_directories(${OPENSPACE_BASE_DIR}/include ${OPENSPACE_BASE_DIR})

add_library(openspace-module-volume ${HEADER_FILES} ${SOURCE_FILES} ${MODULE_CLASS_FILES})
