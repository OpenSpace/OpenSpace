##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2023                                                                #
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
  fitsfilereadermodule.h
  include/fitsfilereader.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES 
  fitsfilereadermodule.cpp
  src/fitsfilereader.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "FitsFileReader"
  fitsfilereader
  ${HEADER_FILES}
  ${SOURCE_FILES}
)

# Set root directories for external libraries.
set(CFITSIO_ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ext/cfitsio/")
set(CCFITS_ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ext/CCfits/")
set(INCLUDES_FOR_TARGET ${CCFITS_ROOT_DIR} "${CCFITS_ROOT_DIR}/../" ${CFITSIO_ROOT_DIR})
set(MODULE_NAME openspace-module-fitsfilereader)

# CCfits is dependent on cfitsio, let it handle the internal linking
add_subdirectory(${CFITSIO_ROOT_DIR})
set_folder_location(cfitsio "External")
set(cfitsio_BUILD_SHARED_LIBS OFF)

add_subdirectory(${CCFITS_ROOT_DIR})
set_folder_location(CCfits "External")
set(CCfits_BUILD_SHARED_LIBS OFF)

disable_external_warnings(cfitsio)
disable_external_warnings(CCfits)

target_include_directories(openspace-module-fitsfilereader SYSTEM PRIVATE ${INCLUDES_FOR_TARGET})
target_link_libraries(openspace-module-fitsfilereader PRIVATE cfitsio CCfits)

target_precompile_headers(CCfits PRIVATE
  <istream>
  <ostream>
)
