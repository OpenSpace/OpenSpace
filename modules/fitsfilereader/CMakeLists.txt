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

# CCfits is dependent on cfitsio, let it handle the internal linking
add_subdirectory(ext/cfitsio SYSTEM)
set_target_properties(cfitsio PROPERTIES FOLDER "External")
set(cfitsio_BUILD_SHARED_LIBS OFF)

add_subdirectory(ext/CCfits SYSTEM)
set_target_properties(CCfits PROPERTIES FOLDER "External")
set(CCfits_BUILD_SHARED_LIBS OFF)

if (MSVC)
  target_compile_options(cfitsio PRIVATE "/W0")
  target_compile_definitions(cfitsio PRIVATE "_SCL_SECURE_NO_WARNINGS")

  target_compile_options(CCfits PRIVATE "/W0")
  target_compile_definitions(CCfits PRIVATE "_SCL_SECURE_NO_WARNINGS")
else ()
  if (APPLE)
    target_compile_options(cfitsio PRIVATE "-Wno-implicit-function-declaration")
  endif ()
  target_compile_options(cfitsio PRIVATE "-w")
  target_compile_options(CCfits PRIVATE "-w")
endif ()


target_include_directories(openspace-module-fitsfilereader SYSTEM PRIVATE ext ext/CCfits ext/cfitsio)
target_link_libraries(openspace-module-fitsfilereader PRIVATE cfitsio CCfits)

target_precompile_headers(CCfits PRIVATE
  <istream>
  <ostream>
)
