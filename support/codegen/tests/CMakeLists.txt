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

add_executable(codegentest)

target_sources(
  codegentest
  PRIVATE
    main.cpp

    execution_enums/execution_enums_arrayify.cpp
    execution_enums/execution_enums_basic.cpp
    execution_enums/execution_enums_keys.cpp
    execution_enums/execution_enums_mapping.cpp
    execution_enums/execution_enums_multiple.cpp
    execution_luawrapper/execution_luawrapper_arguments_enums.cpp
    execution_luawrapper/execution_luawrapper_arguments_structs.cpp
    execution_luawrapper/execution_luawrapper_basic.cpp
    execution_luawrapper/execution_luawrapper_comments.cpp
    execution_luawrapper/execution_luawrapper_types_pointer.cpp
    execution_luawrapper/execution_luawrapper_types_bool.cpp
    execution_luawrapper/execution_luawrapper_types_dictionary.cpp
    execution_luawrapper/execution_luawrapper_types_dmat2x2.cpp
    execution_luawrapper/execution_luawrapper_types_dmat2x3.cpp
    execution_luawrapper/execution_luawrapper_types_dmat2x4.cpp
    execution_luawrapper/execution_luawrapper_types_dmat3x2.cpp
    execution_luawrapper/execution_luawrapper_types_dmat3x3.cpp
    execution_luawrapper/execution_luawrapper_types_dmat3x4.cpp
    execution_luawrapper/execution_luawrapper_types_dmat4x2.cpp
    execution_luawrapper/execution_luawrapper_types_dmat4x3.cpp
    execution_luawrapper/execution_luawrapper_types_dmat4x4.cpp
    execution_luawrapper/execution_luawrapper_types_double.cpp
    execution_luawrapper/execution_luawrapper_types_dvec2.cpp
    execution_luawrapper/execution_luawrapper_types_dvec3.cpp
    execution_luawrapper/execution_luawrapper_types_dvec4.cpp
    execution_luawrapper/execution_luawrapper_types_float.cpp
    execution_luawrapper/execution_luawrapper_types_int.cpp
    execution_luawrapper/execution_luawrapper_types_ivec2.cpp
    execution_luawrapper/execution_luawrapper_types_ivec3.cpp
    execution_luawrapper/execution_luawrapper_types_ivec4.cpp
    execution_luawrapper/execution_luawrapper_types_mat2x2.cpp
    execution_luawrapper/execution_luawrapper_types_mat2x3.cpp
    execution_luawrapper/execution_luawrapper_types_mat2x4.cpp
    execution_luawrapper/execution_luawrapper_types_mat3x2.cpp
    execution_luawrapper/execution_luawrapper_types_mat3x3.cpp
    execution_luawrapper/execution_luawrapper_types_mat3x4.cpp
    execution_luawrapper/execution_luawrapper_types_mat4x2.cpp
    execution_luawrapper/execution_luawrapper_types_mat4x3.cpp
    execution_luawrapper/execution_luawrapper_types_mat4x4.cpp
    execution_luawrapper/execution_luawrapper_types_path.cpp
    execution_luawrapper/execution_luawrapper_types_string.cpp
    execution_luawrapper/execution_luawrapper_types_tuple.cpp
    execution_luawrapper/execution_luawrapper_types_variant.cpp
    execution_luawrapper/execution_luawrapper_types_vec2.cpp
    execution_luawrapper/execution_luawrapper_types_vec3.cpp
    execution_luawrapper/execution_luawrapper_types_vec4.cpp
    execution_luawrapper/execution_luawrapper_types_void.cpp
    execution_misc/execution_misc_parentdoc.cpp
    execution_misc/execution_misc_multiple.cpp
    execution_structs/execution_structs_array.cpp
    execution_structs/execution_structs_attributes.cpp
    execution_structs/execution_structs_basic_types.cpp
    execution_structs/execution_structs_basic_types_optional.cpp
    execution_structs/execution_structs_basic_types_optional_vector.cpp
    execution_structs/execution_structs_basic_types_vector.cpp
    execution_structs/execution_structs_comments.cpp
    execution_structs/execution_structs_enummapping.cpp
    execution_structs/execution_structs_enums.cpp
    execution_structs/execution_structs_map.cpp
    execution_structs/execution_structs_map_enum_key.cpp
    execution_structs/execution_structs_multiple.cpp
    execution_structs/execution_structs_optional_variant_vector.cpp
    execution_structs/execution_structs_other.cpp
    execution_structs/execution_structs_shadowing.cpp
    execution_structs/execution_structs_simple.cpp
    execution_structs/execution_structs_substructs.cpp
    execution_structs/execution_structs_tuple.cpp
    execution_structs/execution_structs_variant.cpp
    execution_structs/execution_structs_variant_filesystem.cpp

    parsing_enums/parsing_enums_arrayify.cpp
    parsing_enums/parsing_enums_basic.cpp
    parsing_enums/parsing_enums_keys.cpp
    parsing_enums/parsing_enums_mapping.cpp
    parsing_enums/parsing_enums_multiple.cpp
    parsing_luawrapper/parsing_luawrapper_basic.cpp
    parsing_luawrapper/parsing_luawrapper_comments.cpp
    parsing_luawrapper/parsing_luawrapper_fail.cpp
    parsing_luawrapper/parsing_luawrapper_return.cpp
    parsing_luawrapper/parsing_luawrapper_types_bool.cpp
    parsing_luawrapper/parsing_luawrapper_types_dictionary.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat2x2.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat2x3.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat2x4.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat3x2.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat3x3.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat3x4.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat4x2.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat4x3.cpp
    parsing_luawrapper/parsing_luawrapper_types_dmat4x4.cpp
    parsing_luawrapper/parsing_luawrapper_types_double.cpp
    parsing_luawrapper/parsing_luawrapper_types_dvec2.cpp
    parsing_luawrapper/parsing_luawrapper_types_dvec3.cpp
    parsing_luawrapper/parsing_luawrapper_types_dvec4.cpp
    parsing_luawrapper/parsing_luawrapper_types_enums.cpp
    parsing_luawrapper/parsing_luawrapper_types_float.cpp
    parsing_luawrapper/parsing_luawrapper_types_int.cpp
    parsing_luawrapper/parsing_luawrapper_types_ivec2.cpp
    parsing_luawrapper/parsing_luawrapper_types_ivec3.cpp
    parsing_luawrapper/parsing_luawrapper_types_ivec4.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat2x2.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat2x3.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat2x4.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat3x2.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat3x3.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat3x4.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat4x2.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat4x3.cpp
    parsing_luawrapper/parsing_luawrapper_types_mat4x4.cpp
    parsing_luawrapper/parsing_luawrapper_types_path.cpp
    parsing_luawrapper/parsing_luawrapper_types_pointer.cpp
    parsing_luawrapper/parsing_luawrapper_types_string.cpp
    parsing_luawrapper/parsing_luawrapper_types_structs.cpp
    parsing_luawrapper/parsing_luawrapper_types_tuple.cpp
    parsing_luawrapper/parsing_luawrapper_types_variant.cpp
    parsing_luawrapper/parsing_luawrapper_types_vec2.cpp
    parsing_luawrapper/parsing_luawrapper_types_vec3.cpp
    parsing_luawrapper/parsing_luawrapper_types_vec4.cpp
    parsing_luawrapper/parsing_luawrapper_types_void.cpp
    parsing_mixed/parsing_mixed_basic.cpp
    parsing_mixed/parsing_multiple_dictionaries.cpp
    parsing_structs/parsing_structs_attributes.cpp
    parsing_structs/parsing_structs_attributes_bool.cpp
    parsing_structs/parsing_structs_attributes_dictionary.cpp
    parsing_structs/parsing_structs_attributes_fail_bool.cpp
    parsing_structs/parsing_structs_attributes_fail_dictionary.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat2x2.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat2x3.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat2x4.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat3x2.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat3x3.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat3x4.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat4x2.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat4x3.cpp
    parsing_structs/parsing_structs_attributes_fail_dmat4x4.cpp
    parsing_structs/parsing_structs_attributes_fail_double.cpp
    parsing_structs/parsing_structs_attributes_fail_dvec2.cpp
    parsing_structs/parsing_structs_attributes_fail_dvec3.cpp
    parsing_structs/parsing_structs_attributes_fail_dvec4.cpp
    parsing_structs/parsing_structs_attributes_fail_float.cpp
    parsing_structs/parsing_structs_attributes_fail_int.cpp
    parsing_structs/parsing_structs_attributes_fail_ivec2.cpp
    parsing_structs/parsing_structs_attributes_fail_ivec3.cpp
    parsing_structs/parsing_structs_attributes_fail_ivec4.cpp
    parsing_structs/parsing_structs_attributes_fail_mat2x2.cpp
    parsing_structs/parsing_structs_attributes_fail_mat2x3.cpp
    parsing_structs/parsing_structs_attributes_fail_mat2x4.cpp
    parsing_structs/parsing_structs_attributes_fail_mat3x2.cpp
    parsing_structs/parsing_structs_attributes_fail_mat3x3.cpp
    parsing_structs/parsing_structs_attributes_fail_mat3x4.cpp
    parsing_structs/parsing_structs_attributes_fail_mat4x2.cpp
    parsing_structs/parsing_structs_attributes_fail_mat4x3.cpp
    parsing_structs/parsing_structs_attributes_fail_mat4x4.cpp
    parsing_structs/parsing_structs_attributes_fail_string.cpp
    parsing_structs/parsing_structs_attributes_fail_vec2.cpp
    parsing_structs/parsing_structs_attributes_fail_vec3.cpp
    parsing_structs/parsing_structs_attributes_fail_vec4.cpp
    parsing_structs/parsing_structs_attributes_float.cpp
    parsing_structs/parsing_structs_attributes_int.cpp
    parsing_structs/parsing_structs_attributes_ivec2.cpp
    parsing_structs/parsing_structs_attributes_ivec3.cpp
    parsing_structs/parsing_structs_attributes_ivec4.cpp
    parsing_structs/parsing_structs_attributes_path.cpp
    parsing_structs/parsing_structs_attributes_string.cpp
    parsing_structs/parsing_structs_attributes_vec2.cpp
    parsing_structs/parsing_structs_attributes_vec3.cpp
    parsing_structs/parsing_structs_attributes_vec4.cpp
    parsing_structs/parsing_structs_enums.cpp
    parsing_structs/parsing_structs_fail.cpp
    parsing_structs/parsing_structs_map.cpp
    parsing_structs/parsing_structs_other.cpp
    parsing_structs/parsing_structs_struct.cpp
    parsing_structs/parsing_structs_variable.cpp
    parsing_structs/parsing_structs_variant.cpp
    parsing_structs/parsing_structs_variant_fail.cpp
)

find_package(Catch2 CONFIG REQUIRED)
target_link_libraries(codegentest PRIVATE Catch2::Catch2 codegen::codegen-lib openspace-core openspace-module-collection)

# codegentest only needs the *write* half (stbi_write_*): the *read* half (stbi_load,
# ...) is already satisfied by assimp, which vendors its own stb_image copy, so linking
# the stbimage port's read implementation too would be a duplicate-symbol conflict
find_package(stbimage CONFIG REQUIRED)
target_link_libraries(codegentest PRIVATE unofficial::stbimage::stbimage-write-impl)

set_compile_settings(codegentest)

if (MSVC)
  # The unit tests frequently creates stacksizes that are a few hundred kb which can
  # create stackoverflows. To prevent this, we allocate a larger stack size of 8MB, which
  # will be good enough to run the existing tests
  target_link_options(codegentest PRIVATE "/STACK:8388608")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # On GNU, we are running into limits of variable tracking restrictions, so similarly, we
  # disable that tracking since we don't intend to debug this unit test application anyway
  target_compile_options(codegentest PRIVATE "-fno-var-tracking-assignments")
endif ()

target_precompile_headers(codegentest PRIVATE
  <openspace/documentation/documentation.h>
  <openspace/documentation/verifier.h>
  <openspace/format.h>
  <openspace/glm.h>
  <catch2/catch_test_macros.hpp>
)

if (OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  # wanted by CEF
  set(PROJECT_ARCH "x86_64")

  # Add the CEF binary distribution's cmake/ directory to the module path and
  # find CEF to initialize it properly.
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBBROWSER_MODULE_PATH}/cmake")
  include(webbrowser_helpers)

  set_cef_targets("${CEF_ROOT}" codegentest)
  run_cef_platform_config("${CEF_ROOT}" "${CEF_TARGET}" "${WEBBROWSER_MODULE_PATH}")
else ()
  message(WARNING "Web configured to be included, but no CEF_ROOT was found, please try configuring CMake again.")
endif ()

# It is important that the __codegen.h does not actually exist so that this target
# is never considered as finished
add_custom_target(run_test_codegen ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/__codegen_unittest.h")
add_dependencies(codegentest run_test_codegen)
add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/__codegen_unittest.h"
  COMMAND codegen::codegen-tool ARGS "${CMAKE_CURRENT_SOURCE_DIR}/execution_enums" "${CMAKE_CURRENT_SOURCE_DIR}/execution_luawrapper" "${CMAKE_CURRENT_SOURCE_DIR}/execution_misc" "${CMAKE_CURRENT_SOURCE_DIR}/execution_structs"
  VERBATIM
)
