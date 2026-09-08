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

# Ghoul is resolved through the vcpkg overlay port at
# ext/ghoul/support/vcpkg/ports/ghoul, which builds the enclosing submodule checkout.
# The ghoul submodule stays checked out because that port builds from its source path.
begin_dependency("Ghoul")
find_package(ghoul CONFIG REQUIRED)
end_dependency()

# Spice
begin_dependency("Spice")
set(SPICE_BUILD_SHARED_LIBRARY OFF CACHE BOOL "" FORCE)
add_subdirectory(spice SYSTEM)
target_compile_features(spice PUBLIC cxx_std_20)
set_target_properties(spice PROPERTIES FOLDER "External")
end_dependency()

# nlohmann JSON (vcpkg)
begin_dependency("nlohmann")
find_package(nlohmann_json CONFIG REQUIRED)
end_dependency()

# Curl
begin_dependency("CURL")
if (WIN32)
  add_library(external-curl SHARED IMPORTED GLOBAL)
  target_include_directories(external-curl INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/curl/include)
  set_property(TARGET external-curl PROPERTY IMPORTED_IMPLIB ${CMAKE_CURRENT_SOURCE_DIR}/curl/lib/libcurl.lib)
  set_property(TARGET external-curl PROPERTY IMPORTED_LOCATION
    ${CMAKE_CURRENT_SOURCE_DIR}/curl/lib/libcurl.dll
    ${CMAKE_CURRENT_SOURCE_DIR}/curl/lib/libeay32.dll
    ${CMAKE_CURRENT_SOURCE_DIR}/curl/lib/ssleay32.dll
  )
else ()
  find_package(CURL)
  if (CURL_FOUND)
    add_library(external-curl INTERFACE)
    target_include_directories(external-curl INTERFACE ${CURL_INCLUDE_DIRS})
    target_link_libraries(external-curl INTERFACE ${CURL_LIBRARIES})
  endif ()
endif ()
end_dependency()
