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

# https://nodejs.org/dist/v8.9.4/win-x64/node.exe
# https://nodejs.org/dist/v8.9.4/node-v8.9.4-darwin-x64.tar.gz
# https://nodejs.org/dist/v8.9.4/node-v8.9.4-linux-x64.tar.xz

function(DownloadNodeJs version download_dir)
  if (MSVC)
    set(basename "node")
    set(filename "${basename}.exe")
    set(path "v${version}/win-x64/${filename}")
  endif ()
  if (APPLE)
    set(basename "node-v${version}-darwin-x64")
    set(filename "${basename}.tar.gz")
    set(path "v${version}/${filename}")
  endif ()
  if (UNIX AND NOT APPLE)
    set(basename "node-v${version}-linux-x64")
    set(filename "${basename}.tar.xz")
    set(path "v${version}/${filename}")
  endif ()

  # Specify the binary distribution type and download directory.
  set(NODEJS_DOWNLOAD_DIR "${download_dir}")

  # The location where we expect the extracted binary distribution.
  set(NODEJS_ROOT "${NODEJS_DOWNLOAD_DIR}" CACHE INTERNAL "NODEJS_ROOT")

  # Download and/or extract the binary distribution if necessary.
  if (NOT IS_DIRECTORY "${NODEJS_ROOT}")
    set(NODEJS_DOWNLOAD_SOURCE "v${version}/node-v${version}-${suffix}")
    set(NODEJS_DOWNLOAD_PATH "${NODEJS_DOWNLOAD_DIR}/${filename}")
    if (NOT EXISTS "${NODEJS_DOWNLOAD_PATH}")
      set(NODEJS_DOWNLOAD_URL "https://nodejs.org/dist/${path}")

      # Download the binary distribution.
      message(STATUS "Downloading NodeJs: ${NODEJS_DOWNLOAD_PATH}...")
      file(DOWNLOAD "${NODEJS_DOWNLOAD_URL}" "${NODEJS_DOWNLOAD_PATH}" SHOW_PROGRESS)
    endif ()

    message(STATUS "URL: ${NODEJS_DOWNLOAD_URL}")

    # Extract the binary distribution for unix
    if (APPLE)
      # Apple uses tar.gz
      message(STATUS "Extracting NodeJs: ${NODEJS_DOWNLOAD_PATH} in ${NODEJS_DOWNLOAD_DIR}")
      execute_process(
        COMMAND tar xzf ${NODEJS_DOWNLOAD_PATH}
        WORKING_DIRECTORY ${NODEJS_DOWNLOAD_DIR}
      )
    endif ()
    if (UNIX AND NOT APPLE)
      # Linux uses tar.xz
      message(STATUS "Extracting NodeJs: ${NODEJS_DOWNLOAD_PATH} in ${NODEJS_DOWNLOAD_DIR}")
      execute_process(
        COMMAND tar xf ${NODEJS_DOWNLOAD_PATH}
        WORKING_DIRECTORY ${NODEJS_DOWNLOAD_DIR}
      )
    endif ()

    if (UNIX)
      FILE(COPY ${NODEJS_DOWNLOAD_DIR}/${basename}/bin/node DESTINATION ${NODEJS_DOWNLOAD_DIR})
      FILE(REMOVE_RECURSE ${NODEJS_DOWNLOAD_DIR}/${basename})
      FILE(REMOVE ${NODEJS_DOWNLOAD_PATH})
    endif ()
  endif ()
endfunction ()
