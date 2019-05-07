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
    ${CMAKE_CURRENT_SOURCE_DIR}/roverterrainmodule.h
    ${CMAKE_CURRENT_SOURCE_DIR}/tasks/roverterraintask.h
    ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/binaryreader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/txtreader.h
    ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/meshwriter.h
    ${CMAKE_CURRENT_SOURCE_DIR}/generation/collapsedmeshgeneration.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/roverterrainmodule.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tasks/roverterraintask.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/binaryreader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/txtreader.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/filehandler/meshwriter.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/generation/collapsedmeshgeneration.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
    "RoverTerrain"
    rover_terrain_module
    ${HEADER_FILES} ${SOURCE_FILES}
)


option(OPENSPACE_MODULE_ROVERTERRAIN_USE_PCL "Use PCL" ON)


if (OPENSPACE_MODULE_ROVERTERRAIN_USE_PCL)
    # Find Packages
    find_package(PCL 1.8 REQUIRED)

    target_compile_definitions(openspace-module-roverterrain PRIVATE GLOBEBROWSING_USE_PCL)

    # Additional Include Directories
    # [C/C++]>[General]>[Additional Include Directories]
    include_directories( ${PCL_INCLUDE_DIRS})

    # Preprocessor Definitions
    # [C/C++]>[Preprocessor]>[Preprocessor Definitions]
    add_definitions( ${PCL_DEFINITIONS})
    #add_definitions( -DPCL_NO_PRECOMPILE )

    # Additional Library Directories
    # [Linker]>[General]>[Additional Library Directories]
    link_directories( ${PCL_LIBRARY_DIRS})

    target_include_directories(openspace-module-roverterrain SYSTEM PUBLIC ${PCL_INCLUDE_DIRS})

    # Additional Dependencies
    # [Linker]>[Input]>[Additional Dependencies]
    target_link_libraries(openspace-module-roverterrain PRIVATE ${PCL_LIBRARIES})
endif()
