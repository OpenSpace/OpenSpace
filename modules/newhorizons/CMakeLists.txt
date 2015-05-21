#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2015                                                               #
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

add_library(openspace-module-newhorizons ${HEADER_FILES} ${SOURCE_FILES} ${MODULE_CLASS_FILES})
target_include_directories(openspace-module-newhorizons PUBLIC "${OPENSPACE_BASE_DIR}/include" "${OPENSPACE_BASE_DIR}")

get_property(OPENSPACE_INCLUDE_DIR TARGET libOpenSpace PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
target_include_directories(openspace-module-newhorizons PUBLIC ${OPENSPACE_INCLUDE_DIR})
get_property(OPENSPACE_DEFINITIONS TARGET libOpenSpace PROPERTY INTERFACE_COMPILE_DEFINITIONS)
target_compile_definitions(openspace-module-newhorizons PUBLIC ${OPENSPACE_DEFINITIONS})
