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

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

set(HEADER_FILES
  moleculemodule.h
  renderablemolecule.h
  renderablesimulationbox.h
  mol/def.h
  mol/util.h
  mol/cache.h
  mol/viamd/coloring.h
  mol/viamd/loader.h
  mol/viamd/postprocessing.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  moleculemodule.cpp
  renderablemolecule.cpp
  renderablesimulationbox.cpp
  billboard.cpp
  mol/util.cpp
  mol/cache.cpp
  mol/viamd/coloring.cpp
  mol/viamd/loader.cpp
  mol/viamd/postprocessing.cpp
  mol/viamd/postprocessing_shaders.inl
)

source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Molecule"
  molecule_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)

option(MD_USE_RELATIVE_RESOURCE_PATH OFF)
option(MD_UNITTEST OFF)
option(MD_BENCHMARK OFF)

add_subdirectory(ext/mold SYSTEM)
target_link_libraries(${molecule_module} PRIVATE mdlib)
set_target_properties(mdlib PROPERTIES FOLDER "External")
