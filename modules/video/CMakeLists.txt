# #########################################################################################
# #
# OpenSpace                                                                              #
# #
# Copyright (c) 2014-2023                                                                #
# #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
# #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
# #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
# #########################################################################################

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

set(HEADER_FILES
  videomodule.h
  include/videotileprovider.h
  include/videoplayer.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  videomodule.cpp
  videomodule_lua.inl
  src/videotileprovider.cpp
  src/videoplayer.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

# Libmpv
if(WIN32)
  add_library(libmpv SHARED IMPORTED)
  target_include_directories(libmpv SYSTEM INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/include)
  set_target_properties(libmpv PROPERTIES
    IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/bin/mpv-2.dll"
    IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/lib/mpv.lib"
  )

  add_library(libopenh264 SHARED IMPORTED)
  set_target_properties(libopenh264 PROPERTIES
    IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/bin/libopenh264.dll"
    IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/ext/libmpv/lib/mpv.lib"
  )

  target_link_libraries(openspace-module-globebrowsing PUBLIC libmpv libopenh264)
else(WIN32)
  find_package(LIBMPV REQUIRED)

  target_include_directories(openspace-module-globebrowsing SYSTEM PRIVATE ${LIBMPV_INCLUDE_DIR})
  target_link_libraries(openspace-module-globebrowsing PRIVATE ${LIBMPV_LIBRARY})
  mark_as_advanced(LIBMPV_CONFIG LIBMPV_INCLUDE_DIR LIBMPV_LIBRARY)
endif() # WIN32

create_new_module(
  "Video"
  video_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)