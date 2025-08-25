##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2025                                                                #
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
    if (CMAKE_SYSTEM_PROCESSOR MATCHES "ARM64|aarch64")
      set(basename "node")
      set(filename "${basename}.exe")
      set(path "v${version}/win-arm64/${filename}")
    else () # Default to x64
      set(basename "node")
      set(filename "${basename}.exe")
      set(path "v${version}/win-x64/${filename}")
    endif ()
  elseif (APPLE)
    if (CMAKE_SYSTEM_PROCESSOR MATCHES "ARM64|aarch64")
      set(basename "node-v${version}-darwin-arm64")
      set(filename "${basename}.tar.gz")
      set(path "v${version}/${filename}")
    else () # Default to x64
      set(basename "node-v${version}-darwin-x64")
      set(filename "${basename}.tar.gz")
      set(path "v${version}/${filename}")
    endif ()
  elseif (UNIX)
    if (CMAKE_SYSTEM_PROCESSOR MATCHES "ARM64|aarch64")
      set(basename "node-v${version}-linux-arm64")
      set(filename "${basename}.tar.xz")
      set(path "v${version}/${filename}")
    else () # Default to x64
      set(basename "node-v${version}-linux-x64")
      set(filename "${basename}.tar.xz")
      set(path "v${version}/${filename}")
    endif ()
  endif ()

  # Create the file if it doesn't exist
  file(MAKE_DIRECTORY "${download_dir}")
  file(TOUCH "${download_dir}/version.txt")

  # Read the file
  file(STRINGS "${download_dir}/version.txt" existing_version)

  # Download and/or extract the binary distribution if necessary
  if ("${existing_version}" STREQUAL "${version}")
    return()
  endif ()

  set(NODEJS_DOWNLOAD_PATH "${download_dir}/${filename}")
  set(NODEJS_DOWNLOAD_URL "https://nodejs.org/dist/${path}")

  # Download the binary distribution
  message(STATUS "Downloading Node.js: ${NODEJS_DOWNLOAD_PATH}")
  file(DOWNLOAD "${NODEJS_DOWNLOAD_URL}" "${NODEJS_DOWNLOAD_PATH}" SHOW_PROGRESS)

  message(STATUS "URL: ${NODEJS_DOWNLOAD_URL}")

  # Extract the binary distribution for unix
  if (APPLE)
    # Apple uses tar.gz
    message(STATUS "Extracting Node.js: ${NODEJS_DOWNLOAD_PATH} in ${download_dir}")
    execute_process(
      COMMAND tar xzf ${NODEJS_DOWNLOAD_PATH}
      WORKING_DIRECTORY ${download_dir}
    )
  endif ()
  if (UNIX AND NOT APPLE)
    # Linux uses tar.xz
    message(STATUS "Extracting Node.js: ${NODEJS_DOWNLOAD_PATH} in ${download_dir}")
    execute_process(
      COMMAND tar xf ${NODEJS_DOWNLOAD_PATH}
      WORKING_DIRECTORY ${download_dir}
    )
  endif ()

  if (UNIX)
    file(COPY ${download_dir}/${basename}/bin/node DESTINATION ${download_dir})
    file(REMOVE_RECURSE ${download_dir}/${basename})
    file(REMOVE ${NODEJS_DOWNLOAD_PATH})
  endif ()

  file(WRITE "${download_dir}/version.txt" "${version}")
endfunction ()
