##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2024                                                                #
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
	${CMAKE_CURRENT_SOURCE_DIR}/include/oscconnection.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/src/oscconnection.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
	"Osc"
	osc_module
	STATIC
	${HEADER_FILES}
	${SOURCE_FILES}
)

add_subdirectory(ext/osc SYSTEM)
set_target_properties(oscpack PROPERTIES FOLDER "External")

if (MSVC)
  target_compile_options(oscpack PRIVATE "/W0")
  target_compile_definitions(oscpack PRIVATE "_SCL_SECURE_NO_WARNINGS")
else ()
  target_compile_options(oscpack PRIVATE "-w")
endif ()

target_include_directories(openspace-module-osc SYSTEM PRIVATE ext ext/osc)
target_link_libraries(openspace-module-osc PRIVATE oscpack)
