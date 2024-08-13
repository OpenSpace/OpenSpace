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

# This file consists of snippets taken from various CEF projects.

# Copyright (c) 2016 The Chromium Embedded Framework Authors. All rights
# reserved. Use of this source code is governed by a BSD-style license that
# can be found in the LICENSE file.

# Sets |CEF_PLATFORM| of parent (caller) scope. Determines the current build
# platform.

function(set_current_cef_build_platform)
  if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set(CEF_PLATFORM "macosx64" PARENT_SCOPE)
  elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    if (CMAKE_SIZEOF_VOID_P MATCHES 8)
      set(CEF_PLATFORM "linux64" PARENT_SCOPE)
    else ()
      set(CEF_PLATFORM "linux32" PARENT_SCOPE)
    endif ()
  elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    if (CMAKE_SIZEOF_VOID_P MATCHES 8)
      set(CEF_PLATFORM "windows64" PARENT_SCOPE)
    else()
      set(CEF_PLATFORM "windows32" PARENT_SCOPE)
    endif ()
  endif ()
endfunction ()

# Download the CEF binary distribution for |platform| and |version| to
# |download_dir|. The |CEF_ROOT| variable will be set in global scope pointing
# to the extracted location.
# Visit http://opensource.spotify.com/cefbuilds/index.html for the list of
# supported platforms and versions.

function(download_cef platform version download_dir)
  # Specify the binary distribution type and download directory.
  set(CEF_DISTRIBUTION "cef_binary_${version}_${platform}")
  set(CEF_DOWNLOAD_DIR "${download_dir}")

  # The location where we expect the extracted binary distribution.
  set(CEF_ROOT "${CEF_DOWNLOAD_DIR}/${CEF_DISTRIBUTION}" CACHE INTERNAL "CEF_ROOT")

  # Download and/or extract the binary distribution if necessary.
  if (NOT IS_DIRECTORY "${CEF_ROOT}")
    set(CEF_DOWNLOAD_FILENAME "${CEF_DISTRIBUTION}.tar.bz2")
    set(CEF_DOWNLOAD_PATH "${CEF_DOWNLOAD_DIR}/${CEF_DOWNLOAD_FILENAME}")
    if (NOT EXISTS "${CEF_DOWNLOAD_PATH}")
      string(REPLACE "+" "%2B" CEF_DOWNLOAD_URL "http://cef-builds.spotifycdn.com/${CEF_DOWNLOAD_FILENAME}")

      # Download the SHA1 hash for the binary distribution.
      message(STATUS "Downloading CEF: ${CEF_DOWNLOAD_PATH}.sha1...")
      file(DOWNLOAD "${CEF_DOWNLOAD_URL}.sha1" "${CEF_DOWNLOAD_PATH}.sha1")
      file(READ "${CEF_DOWNLOAD_PATH}.sha1" CEF_SHA1)

      # Download the binary distribution and verify the hash.
      message(STATUS "Downloading CEF: ${CEF_DOWNLOAD_PATH}...")
      file(
        DOWNLOAD "${CEF_DOWNLOAD_URL}" "${CEF_DOWNLOAD_PATH}"
        EXPECTED_HASH SHA1=${CEF_SHA1}
        SHOW_PROGRESS
      )
    endif ()

    # Extract the binary distribution.
    message(STATUS "Extracting CEF: ${CEF_DOWNLOAD_PATH}...")
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E tar xzf "${CEF_DOWNLOAD_DIR}/${CEF_DOWNLOAD_FILENAME}"
      WORKING_DIRECTORY ${CEF_DOWNLOAD_DIR}
    )
  endif ()
endfunction ()

macro(set_openspace_cef_target_out_dir)
  if (${CMAKE_GENERATOR} STREQUAL "Ninja" OR
      ${CMAKE_GENERATOR} STREQUAL "Unix Makefiles")
    # By default Ninja and Make builds don't create a subdirectory named after
    # the configuration.
    # set(CEF_TARGET_OUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_BUILD_TYPE}")
    set(CEF_TARGET_OUT_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CMAKE_BUILD_TYPE}")

    # Output binaries (executables, libraries) to the correct directory.
    # set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CEF_TARGET_OUT_DIR})
    # set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CEF_TARGET_OUT_DIR})
  else ()
    # set(CEF_TARGET_OUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/$<CONFIGURATION>")
    set(CEF_TARGET_OUT_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>")
  endif ()
endmacro ()

macro(add_windows_cef_manifest target_dir manifest_path target extension)
  add_custom_command(
      TARGET ${target}
      POST_BUILD
      COMMAND "mt.exe" -nologo
      -manifest \"${manifest_path}/${target}.${extension}.manifest\" \"${manifest_path}/compatibility.manifest\"
      -outputresource:"${target_dir}/${target}.${extension}"\;\#1
      COMMENT "Adding manifest..."
  )
endmacro ()


# Add a logical target that can be used to link the specified libraries into an
# executable target.
macro(add_cef_logical_target target debug_lib release_lib)
  add_library(${target} ${CEF_LIBTYPE} IMPORTED GLOBAL)
  set_target_properties(${target} PROPERTIES
    IMPORTED_LOCATION "${release_lib}"
    IMPORTED_LOCATION_DEBUG "${debug_lib}"
    IMPORTED_LOCATION_RELEASE "${release_lib}"
  )
endmacro ()
