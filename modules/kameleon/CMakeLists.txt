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

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

set(HEADER_FILES
  include/kameleonwrapper.h
  include/kameleonhelper.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  src/kameleonwrapper.cpp
  src/kameleonhelper.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Kameleon"
  kameleon_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)

option(KAMELEON_LIBRARY_ONLY "Build with Kameleon as library only" ON)
if (WIN32)
  option(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
else ()
  option(BUILD_SHARED_LIBS "Build Shared Libraries" ON)
endif ()
mark_as_advanced(BUILD_SHARED_LIBS) # Change to set instead of option
option(KAMELEON_USE_HDF5 "Kameleon use HDF5" OFF)
set(KAMELEON_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ext/kameleon)
set(KAMELEON_INCLUDES ${KAMELEON_ROOT_DIR}/src)
add_subdirectory(${KAMELEON_ROOT_DIR} SYSTEM)
target_include_directories(${kameleon_module} SYSTEM PUBLIC ${KAMELEON_INCLUDES})
target_link_libraries(${kameleon_module} PRIVATE ccmc)
mark_as_advanced(CDF_BUILD_ZLIB CDF_INCLUDES CDF_LIBRARY CDF_USE_STATIC_LIBS
  CDF_USE_ZLIB HDF5_DIR HDF5_USE_STATIC_LIBRARIES KAMELEON_LIBRARY_ONLY KAMELEON_USE_HDF5
)
set_target_properties(ccmc PROPERTIES FOLDER "External")
if (TARGET cdf)
  if (MSVC)
    target_compile_options(cdf PRIVATE "/W0")
    target_compile_definitions(cdf PRIVATE "_SCL_SECURE_NO_WARNINGS")
  else ()
    target_compile_options(cdf PRIVATE "-w")
  endif ()

  set_target_properties(cdf PROPERTIES FOLDER "External")
endif ()

target_precompile_headers(cdf PRIVATE
  [["stdio.h"]]
  [["stdlib.h"]]
  [["string.h"]]
)

target_precompile_headers(ccmc PRIVATE
  "$<$<COMPILE_LANGUAGE:CXX>:iostream>"
  "$<$<COMPILE_LANGUAGE:CXX>:map>"
  "$<$<COMPILE_LANGUAGE:CXX>:unordered_map>"
  "$<$<COMPILE_LANGUAGE:CXX>:vector>"
)
