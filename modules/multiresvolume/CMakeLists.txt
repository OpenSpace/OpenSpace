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
  rendering/atlasmanager.h
  rendering/brickmanager.h
  rendering/brickselector.h
  rendering/brickcover.h
  rendering/brickselection.h
  rendering/multiresvolumeraycaster.h
  rendering/shenbrickselector.h
  rendering/tfbrickselector.h
  rendering/localtfbrickselector.h
  rendering/simpletfbrickselector.h
  rendering/renderablemultiresvolume.h
  rendering/tsp.h
  rendering/histogrammanager.h
  rendering/errorhistogrammanager.h
  rendering/localerrorhistogrammanager.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  rendering/atlasmanager.cpp
  rendering/brickcover.cpp
  rendering/brickmanager.cpp
  rendering/brickselection.cpp
  rendering/multiresvolumeraycaster.cpp
  rendering/shenbrickselector.cpp
  rendering/tfbrickselector.cpp
  rendering/localtfbrickselector.cpp
  rendering/simpletfbrickselector.cpp
  rendering/renderablemultiresvolume.cpp
  rendering/tsp.cpp
  rendering/histogrammanager.cpp
  rendering/errorhistogrammanager.cpp
  rendering/localerrorhistogrammanager.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "MultiresVolume"
  multiresvolume_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)
