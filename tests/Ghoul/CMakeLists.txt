##########################################################################################
#                                                                                        #
# GHOUL                                                                                  #
# General Helpful Open Utility Library                                                   #
#                                                                                        #
# Copyright (c) 2012-2026                                                                #
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

find_package(Catch2 3 CONFIG REQUIRED)

add_executable(GhoulTest)
set_openspace_compile_settings(GhoulTest)
target_sources(
  GhoulTest
  PRIVATE
    main.cpp
    test_commandlineparser.cpp
    test_crc32.cpp
    test_csvreader.cpp
    test_dictionary.cpp
    test_dictionaryjsonformatter.cpp
    test_dictionaryluaformatter.cpp
    test_filesystem.cpp
    test_luaconversions.cpp
    test_luatodictionary.cpp
    test_memorypool.cpp
    test_templatefactory.cpp
)

target_compile_definitions(GhoulTest PRIVATE
  # Jenkins shouldn't ask for asserts when they happen, but just throw
  "GHL_THROW_ON_ASSERT"
  "GHOUL_HAVE_TESTS"
  "TEST_ROOT_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}\""
)

if (MSVC)
  target_compile_options(GhoulTest PRIVATE "-Od" "/bigobj")
else ()
  target_compile_options(GhoulTest PRIVATE "-O0" )
endif ()

target_link_libraries(GhoulTest PRIVATE Catch2::Catch2 Ghoul)

target_precompile_headers(GhoulTest PRIVATE
  <catch2/catch_test_macros.hpp>
)

add_test(NAME GhoulTest COMMAND GhoulTest)
