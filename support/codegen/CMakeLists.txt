##########################################################################################
#                                                                                        #
# OpenSpace Codegen                                                                      #
#                                                                                        #
# Copyright (c) 2021-2026                                                                #
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

cmake_minimum_required(VERSION 4.0)

project(codegen VERSION 1.0.0 LANGUAGES CXX)

find_package(common-compile-settings CONFIG REQUIRED)

# Redirecting the output would leak into a surrounding project that vendors codegen
if (PROJECT_IS_TOP_LEVEL)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
endif ()

add_subdirectory(lib)

add_executable(codegen-tool main.cpp)
target_link_libraries(codegen-tool PRIVATE codegen-lib)
set_compile_settings(codegen-tool)

# Install and export rules that make codegen consumable through
# `find_package(codegen CONFIG REQUIRED)`, both the `codegen::codegen-lib` library and the
# `codegen::codegen-tool` executable that a consumer runs as a build step
if (PROJECT_IS_TOP_LEVEL)
  include(GNUInstallDirs)
  include(CMakePackageConfigHelpers)

  set(CODEGEN_INSTALL_CMAKEDIR "${CMAKE_INSTALL_DATADIR}/codegen")

  install(
    TARGETS codegen-lib codegen-tool
    EXPORT codegenTargets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    FILE_SET HEADERS DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  )
  install(
    EXPORT codegenTargets
    FILE codegenTargets.cmake
    NAMESPACE codegen::
    DESTINATION ${CODEGEN_INSTALL_CMAKEDIR}
  )

  configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/support/cmake/codegenConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/codegenConfig.cmake
    INSTALL_DESTINATION ${CODEGEN_INSTALL_CMAKEDIR}
  )
  write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/codegenConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
  )
  install(
    FILES
      ${CMAKE_CURRENT_BINARY_DIR}/codegenConfig.cmake
      ${CMAKE_CURRENT_BINARY_DIR}/codegenConfigVersion.cmake
    DESTINATION ${CODEGEN_INSTALL_CMAKEDIR}
  )
endif ()
