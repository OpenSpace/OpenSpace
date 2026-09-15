##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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
  include/renderabletimevaryingfitssphere.h
  include/wsafitshelper.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  fitsfilereadermodule.cpp
  src/fitsfilereader.cpp
  src/renderabletimevaryingfitssphere.cpp
  src/wsafitshelper.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "FitsFileReader"
  fitsfilereader
  ${HEADER_FILES} ${SOURCE_FILES}
)

begin_dependency("CCfits")
find_package(ccfits CONFIG REQUIRED)
target_link_libraries(openspace-module-fitsfilereader PRIVATE CCfits::CCfits)
# vcpkg's CCfits::CCfits target does not propagate CCFITS_STATIC_DEFINE, which its generated
# ccfits_export.h needs to avoid declaring header-only methods as __declspec(dllimport) when
# CCfits is built statically (as it is under this triplet). Without it, MSVC never emits those
# methods anywhere, causing unresolved externals for e.g. FITS::extension() and
# FITS::setVerboseMode().
target_compile_definitions(openspace-module-fitsfilereader PRIVATE CCFITS_STATIC_DEFINE)
end_dependency("CCfits")
