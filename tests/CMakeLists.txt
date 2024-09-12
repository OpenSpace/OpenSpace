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

add_executable(
  OpenSpaceTest
  main.cpp
  test_assetloader.cpp
  test_concurrentqueue.cpp
  test_distanceconversion.cpp
  test_documentation.cpp
  test_horizons.cpp
  test_iswamanager.cpp
  test_jsonformatting.cpp
  test_latlonpatch.cpp
  test_lrucache.cpp
  test_lua_createsinglecolorimage.cpp
  test_profile.cpp
  test_rawvolumeio.cpp
  test_scriptscheduler.cpp
  test_settings.cpp
  test_sgctedit.cpp
  test_spicemanager.cpp
  test_timeconversion.cpp
  test_timeline.cpp
  test_timequantizer.cpp

  property/test_property_optionproperty.cpp
  property/test_property_listproperties.cpp
  property/test_property_selectionproperty.cpp

  regression/517.cpp
)

set_openspace_compile_settings(OpenSpaceTest)

target_include_directories(OpenSpaceTest
  PUBLIC
    "../apps/OpenSpace/ext/sgct/ext/json/include"
    "../apps/OpenSpace/ext/sgct/ext/json-schema-validator/src"
)

target_compile_definitions(OpenSpaceTest PUBLIC "GHL_THROW_ON_ASSERT")
target_link_libraries(OpenSpaceTest PUBLIC Catch2 openspace-core sgct)

target_precompile_headers(OpenSpaceTest PRIVATE
  <catch2/catch_test_macros.hpp>
)

foreach (library_name ${all_enabled_modules})
  get_target_property(library_type ${library_name} TYPE)
  if (NOT ${library_type} STREQUAL "SHARED_LIBRARY")
    target_link_libraries(OpenSpaceTest PRIVATE ${library_name})
  endif ()
endforeach ()

if (OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  # Add the CEF binary distribution's cmake/ directory to the module path and
  # find CEF to initialize it properly.
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBBROWSER_MODULE_PATH}/cmake")
  include(webbrowser_helpers)

  set_cef_targets("${CEF_ROOT}" OpenSpaceTest)
  run_cef_platform_config("${CEF_ROOT}" "${CEF_TARGET}" "${WEBBROWSER_MODULE_PATH}")
endif ()

set_target_properties(OpenSpaceTest PROPERTIES FOLDER "Unit Tests")
