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
  videomodule.h
  include/videotileprovider.h
  include/videoplayer.h
  include/screenspacevideo.h
  include/renderablevideosphere.h
  include/renderablevideoplane.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  videomodule.cpp
  src/videotileprovider.cpp
  src/videoplayer.cpp
  src/screenspacevideo.cpp
  src/renderablevideosphere.cpp
  src/renderablevideoplane.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Video"
  video_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES}
)

# Libmpv
if (WIN32)
  set(LIBMPV_ROOT "${CMAKE_CURRENT_BINARY_DIR}/ext/libmpv" CACHE INTERNAL "LIBMPV_ROOT")
  if (NOT IS_DIRECTORY "${LIBMPV_ROOT}")
    message(STATUS "Downloading LibMPV...")

    # First download the SHA1 hash of the file
    file(
      DOWNLOAD
      "http://data.openspaceproject.com/ext/libmpv/2.0/libmpv.zip.sha1"
      "${LIBMPV_ROOT}/libmpv.zip.sha1"
    )
    file(READ "${LIBMPV_ROOT}/libmpv.zip.sha1" LIBMPV_SHA1)

    # For some reason the SHA1 file contains extra linebreaks
    string(STRIP ${LIBMPV_SHA1} LIBMPV_SHA1)

    # Then download the file itself
    file(
      DOWNLOAD
      "http://data.openspaceproject.com/ext/libmpv/2.0/libmpv.zip"
      "${LIBMPV_ROOT}/libmpv.zip"
      EXPECTED_HASH SHA1=${LIBMPV_SHA1}
      SHOW_PROGRESS
    )

    execute_process(
      COMMAND ${CMAKE_COMMAND} -E tar xzf "${LIBMPV_ROOT}/libmpv.zip"
      WORKING_DIRECTORY ${LIBMPV_ROOT}
    )
  endif ()

  add_library(libmpv SHARED IMPORTED)
  target_include_directories(libmpv SYSTEM INTERFACE ${LIBMPV_ROOT}/libmpv/include)
  set_target_properties(libmpv PROPERTIES
    IMPORTED_LOCATION "${LIBMPV_ROOT}/libmpv/bin/mpv-2.dll"
    IMPORTED_IMPLIB "${LIBMPV_ROOT}/libmpv/lib/mpv.lib"
  )

  add_library(libopenh264 SHARED IMPORTED)
  set_target_properties(libopenh264 PROPERTIES
    IMPORTED_LOCATION "${LIBMPV_ROOT}/libmpv/bin/libopenh264.dll"
    IMPORTED_IMPLIB "${LIBMPV_ROOT}/libmpv/lib/mpv.lib"
  )

  target_link_libraries(openspace-module-video PUBLIC libmpv libopenh264)
else (WIN32)
  find_package(Libmpv REQUIRED)
  # For some reason libmpv on Ubuntu doesn't link if Vulkan is not included too
  find_package(Vulkan REQUIRED)

  target_include_directories(openspace-module-video SYSTEM PRIVATE ${Libmpv_INCLUDE_DIRS})
  target_link_libraries(openspace-module-video PRIVATE ${Libmpv_LIBRARIES} Vulkan::Vulkan)
  mark_as_advanced(LIBMPV_CONFIG Libmpv_INCLUDE_DIRS Libmpv_LIBRARIES)
endif () # WIN32
