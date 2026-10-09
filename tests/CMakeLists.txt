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

add_executable(OpenSpaceTest)
set_openspace_compile_settings(OpenSpaceTest)
add_test(NAME OpenSpaceTest COMMAND OpenSpaceTest)

target_sources(
  OpenSpaceTest
  PRIVATE
    main.cpp
    testhelper.h

    test_assetloader.cpp
    test_commandlineparser.cpp
    test_concurrentqueue.cpp
    test_crc32.cpp
    test_csvreader.cpp
    test_dictionary.cpp
    test_dictionaryjsonformatter.cpp
    test_dictionaryluaformatter.cpp
    test_distanceconversion.cpp
    test_documentation.cpp
    test_filesystem.cpp
    test_horizons.cpp
    test_iswamanager.cpp
    test_jsonformatting.cpp
    test_kepler.cpp
    test_latlonpatch.cpp
    test_lrucache.cpp
    test_luaconversions.cpp
    test_luatodictionary.cpp
    test_lua_createsinglecolorimage.cpp
    test_lua_factoryconstructors.cpp
    test_lua_property.cpp
    test_lua_propertyvalue.cpp
    test_lua_setpropertyvalue.cpp
    test_memorypool.cpp
    test_profile.cpp
    test_rawvolumeio.cpp
    test_scriptscheduler.cpp
    test_sessionrecording.cpp
    test_settings.cpp
    test_sgctedit.cpp
    test_spicemanager.cpp
    test_templatefactory.cpp
    test_timeconversion.cpp
    test_timeline.cpp
    test_timequantizer.cpp

    property/test_property_optionproperty.cpp
    property/test_property_listproperties.cpp
    property/test_property_selectionproperty.cpp

    regression/517.cpp

    codegen/execution_enums/execution_enums_arrayify.cpp
    codegen/execution_enums/execution_enums_basic.cpp
    codegen/execution_enums/execution_enums_keys.cpp
    codegen/execution_enums/execution_enums_mapping.cpp
    codegen/execution_enums/execution_enums_multiple.cpp
    codegen/execution_luawrapper/execution_luawrapper_arguments_enums.cpp
    codegen/execution_luawrapper/execution_luawrapper_arguments_structs.cpp
    codegen/execution_luawrapper/execution_luawrapper_basic.cpp
    codegen/execution_luawrapper/execution_luawrapper_comments.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_pointer.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_bool.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dictionary.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat2x2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat2x3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat2x4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat3x2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat3x3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat3x4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat4x2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat4x3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dmat4x4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_double.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dvec2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dvec3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_dvec4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_float.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_int.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_ivec2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_ivec3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_ivec4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat2x2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat2x3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat2x4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat3x2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat3x3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat3x4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat4x2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat4x3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_mat4x4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_path.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_string.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_tuple.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_variant.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_vec2.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_vec3.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_vec4.cpp
    codegen/execution_luawrapper/execution_luawrapper_types_void.cpp
    codegen/execution_misc/execution_misc_parentdoc.cpp
    codegen/execution_misc/execution_misc_multiple.cpp
    codegen/execution_structs/execution_structs_array.cpp
    codegen/execution_structs/execution_structs_attributes.cpp
    codegen/execution_structs/execution_structs_basic_types.cpp
    codegen/execution_structs/execution_structs_basic_types_optional.cpp
    codegen/execution_structs/execution_structs_basic_types_optional_vector.cpp
    codegen/execution_structs/execution_structs_basic_types_vector.cpp
    codegen/execution_structs/execution_structs_comments.cpp
    codegen/execution_structs/execution_structs_enummapping.cpp
    codegen/execution_structs/execution_structs_enums.cpp
    codegen/execution_structs/execution_structs_map.cpp
    codegen/execution_structs/execution_structs_map_enum_key.cpp
    codegen/execution_structs/execution_structs_multiple.cpp
    codegen/execution_structs/execution_structs_optional_variant_vector.cpp
    codegen/execution_structs/execution_structs_other.cpp
    codegen/execution_structs/execution_structs_shadowing.cpp
    codegen/execution_structs/execution_structs_simple.cpp
    codegen/execution_structs/execution_structs_substructs.cpp
    codegen/execution_structs/execution_structs_tuple.cpp
    codegen/execution_structs/execution_structs_variant.cpp
    codegen/execution_structs/execution_structs_variant_filesystem.cpp

    codegen/parsing_enums/parsing_enums_arrayify.cpp
    codegen/parsing_enums/parsing_enums_basic.cpp
    codegen/parsing_enums/parsing_enums_keys.cpp
    codegen/parsing_enums/parsing_enums_mapping.cpp
    codegen/parsing_enums/parsing_enums_multiple.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_basic.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_comments.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_fail.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_return.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_bool.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dictionary.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat2x2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat2x3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat2x4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat3x2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat3x3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat3x4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat4x2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat4x3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dmat4x4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_double.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dvec2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dvec3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_dvec4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_enums.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_float.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_int.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_ivec2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_ivec3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_ivec4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat2x2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat2x3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat2x4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat3x2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat3x3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat3x4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat4x2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat4x3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_mat4x4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_path.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_pointer.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_string.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_structs.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_tuple.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_variant.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_vec2.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_vec3.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_vec4.cpp
    codegen/parsing_luawrapper/parsing_luawrapper_types_void.cpp
    codegen/parsing_mixed/parsing_mixed_basic.cpp
    codegen/parsing_mixed/parsing_multiple_dictionaries.cpp
    codegen/parsing_structs/parsing_structs_attributes.cpp
    codegen/parsing_structs/parsing_structs_attributes_bool.cpp
    codegen/parsing_structs/parsing_structs_attributes_dictionary.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_bool.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dictionary.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat2x2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat2x3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat2x4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat3x2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat3x3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat3x4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat4x2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat4x3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dmat4x4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_double.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dvec2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dvec3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_dvec4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_float.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_int.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_ivec2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_ivec3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_ivec4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat2x2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat2x3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat2x4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat3x2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat3x3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat3x4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat4x2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat4x3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_mat4x4.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_string.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_vec2.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_vec3.cpp
    codegen/parsing_structs/parsing_structs_attributes_fail_vec4.cpp
    codegen/parsing_structs/parsing_structs_attributes_float.cpp
    codegen/parsing_structs/parsing_structs_attributes_int.cpp
    codegen/parsing_structs/parsing_structs_attributes_ivec2.cpp
    codegen/parsing_structs/parsing_structs_attributes_ivec3.cpp
    codegen/parsing_structs/parsing_structs_attributes_ivec4.cpp
    codegen/parsing_structs/parsing_structs_attributes_path.cpp
    codegen/parsing_structs/parsing_structs_attributes_string.cpp
    codegen/parsing_structs/parsing_structs_attributes_vec2.cpp
    codegen/parsing_structs/parsing_structs_attributes_vec3.cpp
    codegen/parsing_structs/parsing_structs_attributes_vec4.cpp
    codegen/parsing_structs/parsing_structs_enums.cpp
    codegen/parsing_structs/parsing_structs_fail.cpp
    codegen/parsing_structs/parsing_structs_map.cpp
    codegen/parsing_structs/parsing_structs_other.cpp
    codegen/parsing_structs/parsing_structs_struct.cpp
    codegen/parsing_structs/parsing_structs_variable.cpp
    codegen/parsing_structs/parsing_structs_variant.cpp
    codegen/parsing_structs/parsing_structs_variant_fail.cpp
)

find_package(Catch2 CONFIG REQUIRED)
find_package(sgct CONFIG REQUIRED)
find_package(nlohmann_json CONFIG REQUIRED)
find_package(nlohmann_json_schema_validator CONFIG REQUIRED)
find_package(stbimage CONFIG REQUIRED)

target_compile_definitions(
  OpenSpaceTest
  PRIVATE
  "OPENSPACE_THROW_ON_ASSERT"
  "TEST_ROOT_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}\""
  "CATCH_CONFIG_ENABLE_ALL_STRINGMAKERS"
)

target_link_libraries(
  OpenSpaceTest
  PRIVATE
    Catch2::Catch2
    codegen::codegen-lib
    openspace-core
    openspace-module-collection
    sgct::sgct
    unofficial::stbimage::stbimage-write-impl
    nlohmann_json::nlohmann_json
    nlohmann_json_schema_validator::validator
)

target_precompile_headers(
  OpenSpaceTest
  PRIVATE
    <openspace/documentation/documentation.h>
    <openspace/documentation/verifier.h>
    <openspace/format.h>
    <openspace/glm.h>
    <catch2/catch_test_macros.hpp>
)

if (MSVC)
  # The unit tests frequently creates stacksizes that are a few hundred kb which can
  # create stackoverflows. To prevent this, we allocate a larger stack size of 8MB, which
  # will be good enough to run the existing tests
  target_link_options(OpenSpaceTest PRIVATE "/STACK:8388608")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # On GNU, we are running into limits of variable tracking restrictions, so similarly, we
  # disable that tracking since we don't intend to debug this unit test application anyway
  target_compile_options(OpenSpaceTest PRIVATE "-fno-var-tracking-assignments")
endif ()

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
else ()
  message(WARNING "Web configured to be included, but no CEF_ROOT was found, please try configuring CMake again.")
endif ()

# It is important that the __codegen.h does not actually exist so that this target is
# never considered as finished
add_custom_target(run_test_codegen ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/__codegen_unittest.h")
set_target_properties(run_test_codegen PROPERTIES FOLDER "support")
add_dependencies(OpenSpaceTest run_test_codegen)
add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/__codegen_unittest.h"
  COMMAND codegen::codegen-tool ARGS "${CMAKE_CURRENT_SOURCE_DIR}/codegen/execution_enums" "${CMAKE_CURRENT_SOURCE_DIR}/codegen/execution_luawrapper" "${CMAKE_CURRENT_SOURCE_DIR}/codegen/execution_misc" "${CMAKE_CURRENT_SOURCE_DIR}/codegen/execution_structs"
  VERBATIM
)
