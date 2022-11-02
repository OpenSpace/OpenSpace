##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2022                                                                #
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
include(${GHOUL_BASE_DIR}/support/cmake/handle_external_library.cmake)

set(HEADER_FILES
  moleculemodule.h
  moleculemanager.h
  rendering/renderablemolecule.h
  rendering/renderablesimulationbox.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  moleculemodule.cpp
  moleculemanager.cpp
  rendering/renderablemolecule.cpp
  rendering/renderablesimulationbox.cpp
  rendering/viamd/coloring.cpp
  #rendering/viamd/color_utils.cpp
  rendering/viamd/loader.cpp
  rendering/viamd/gfx/gl_utils.cpp
  rendering/viamd/gfx/immediate_draw_utils.cpp
  rendering/viamd/gfx/conetracing_utils.cpp
  rendering/viamd/gfx/postprocessing_utils.cpp
  rendering/billboard.cpp
)

source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Molecule"
  molecule_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)

option(MD_USE_RELATIVE_RESOURCE_PATH OFF)

include_external_library(${molecule_module} PRIVATE mdlib ${CMAKE_CURRENT_SOURCE_DIR}/ext/mold)
disable_external_warnings(mdlib)

target_compile_definitions(${molecule_module} PRIVATE VIAMD_SHADER_DIR="${CMAKE_CURRENT_SOURCE_DIR}/rendering/viamd/gfx/shaders")
  
# disable annoying warnings from mold included header files
# TODO: Cmake people help me!! How do you do that properly??
# I hate Cmake
if (MSVC)
else ()
  target_compile_options(${molecule_module} PRIVATE -w)
endif ()
