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

function(set_cef_targets cef_root main_target)
  # find cef cmake helpers
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${cef_root}/cmake")
  include(cef_support)

  # Use <PackageName>_ROOT variables
  # https://cmake.org/cmake/help/git-stage/policy/CMP0074.html
  cmake_policy(SET CMP0074 NEW)
  find_package(CEF REQUIRED)

  # ensure out target dir is set
  set_openspace_cef_target_out_dir()

  # main CEF executable target
  set(CEF_TARGET ${main_target} PARENT_SCOPE)
endfunction ()

function(run_cef_platform_config cef_root cef_target module_path)
  # find cef cmake helpers
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${cef_root}/cmake")
  include(cef_support)

  # Use <PackageName>_ROOT variables
  # https://cmake.org/cmake/help/git-stage/policy/CMP0074.html
  cmake_policy(SET CMP0074 NEW)
  find_package(CEF REQUIRED)

  if (OS_MACOSX)
    run_cef_macosx_config("${cef_target}" "${module_path}")
  endif ()
  if (OS_WINDOWS)
    run_cef_windows_config("${cef_target}" "${cef_root}" "${module_path}")
  endif ()
  if (OS_LINUX)
    run_cef_linux_config("${cef_target}" "${cef_root}")
  endif ()
endfunction ()

function(run_cef_macosx_config CEF_ROOT module_path)
  if (${CMAKE_GENERATOR} STREQUAL "Ninja" OR ${CMAKE_GENERATOR} STREQUAL "Unix Makefiles")
    set(CEF_OUTPUT_PREFIX "")
  else ()
    set(CEF_OUTPUT_PREFIX "$<CONFIG>/")
  endif ()

  set(CEF_FINAL_APP "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CEF_OUTPUT_PREFIX}${CEF_TARGET}.app")
  set(CEF_INTERMEDIATE_HELPER_APP "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CEF_OUTPUT_PREFIX}${CEF_HELPER_TARGET}.app")
  set(CEF_INTERMEDIATE_HELPER_APP_GPU "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CEF_OUTPUT_PREFIX}${CEF_HELPER_TARGET_GPU}.app")
  set(CEF_INTERMEDIATE_HELPER_APP_RENDERER "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CEF_OUTPUT_PREFIX}${CEF_HELPER_TARGET_RENDERER}.app")

  set(CEF_FINAL_HELPER_APP "${CEF_FINAL_APP}/Contents/Frameworks/${CEF_HELPER_TARGET}.app")
  set(CEF_FINAL_HELPER_APP_GPU "${CEF_FINAL_APP}/Contents/Frameworks/${CEF_HELPER_TARGET_GPU}.app")
  set(CEF_FINAL_HELPER_APP_RENDERER "${CEF_FINAL_APP}/Contents/Frameworks/${CEF_HELPER_TARGET_RENDERER}.app")
  set(CEF_FRAMEWORK_LOCATION "${CEF_BINARY_DIR}/Chromium Embedded Framework.framework")
  set(CEF_FRAMEWORK_FINAL_LOCATION "${CEF_FINAL_APP}/Contents/Frameworks/Chromium Embedded Framework.framework")

  add_dependencies(${CEF_TARGET} libcef_dll_wrapper "${CEF_HELPER_TARGET}" "${CEF_HELPER_TARGET_GPU}" "${CEF_HELPER_TARGET_RENDERER}")

  # target_link_libraries(${CEF_TARGET} PUBLIC libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
  target_link_libraries(${CEF_TARGET} PUBLIC libcef_dll_wrapper ${CEF_STANDARD_LIBS})
  set_target_properties(${CEF_TARGET} PROPERTIES
    RESOURCE "${WEBBROWSER_RESOURCES_SRCS}"
    MACOSX_BUNDLE_INFO_PLIST ${module_path}/mac/Info.plist
  )

  # Copy files into the main app bundle.
  add_custom_command(
    TARGET ${CEF_TARGET}
    POST_BUILD
    # Copy the helper app bundle into the Frameworks directory.
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CEF_INTERMEDIATE_HELPER_APP}" "${CEF_FINAL_HELPER_APP}"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CEF_INTERMEDIATE_HELPER_APP_GPU}" "${CEF_FINAL_HELPER_APP_GPU}"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CEF_INTERMEDIATE_HELPER_APP_RENDERER}" "${CEF_FINAL_HELPER_APP_RENDERER}"
    # Copy the CEF framework into the Frameworks directory.
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CEF_FRAMEWORK_LOCATION}" "${CEF_FRAMEWORK_FINAL_LOCATION}"
    VERBATIM
  )

  # copy dynamic libraries to bundle
  file(GLOB LIBRARIES_TO_COPY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/*.dylib")
  foreach (lib_file ${LIBRARIES_TO_COPY})
    get_filename_component(file_name "${lib_file}" NAME)
    add_custom_command(
      TARGET ${CEF_TARGET} POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${lib_file}"
      "${CEF_FINAL_APP}/Contents/${file_name}"
    )
  endforeach ()

  if (NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
    # Manually process and copy over resource files.
    # The Xcode generator handles this via the set_target_properties RESOURCE directive.
    set(PREFIXES "mac/")  # Remove these prefixes from input file paths.
    COPY_MACOSX_RESOURCES("${WEBBROWSER_RESOURCES_SOURCES}" "${PREFIXES}" "${CEF_TARGET}" "${module_path}" "${CEF_FINAL_APP}")
  endif ()
endfunction ()

function(run_cef_windows_config CEF_TARGET CEF_ROOT MODULE_PATH)
  # Executable target.
  add_dependencies(${CEF_TARGET} libcef_dll_wrapper)
  target_link_libraries(${CEF_TARGET} PUBLIC libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
  include_directories(${CEF_ROOT})

  add_dependencies(${CEF_TARGET} libcef_dll_wrapper "${CEF_HELPER_TARGET}")

  if (USE_SANDBOX)
    # Logical target used to link the cef_sandbox library.
    message(STATUS "Using CEF in Sandboxed mode.")
    add_logical_target("cef_sandbox_lib" "${CEF_SANDBOX_LIB_DEBUG}" "${CEF_SANDBOX_LIB_RELEASE}")
    target_link_libraries(${CEF_TARGET} cef_sandbox_lib ${CEF_SANDBOX_STANDARD_LIBS})
  endif ()

  # Add the custom manifest files to the executable.
  set_openspace_cef_target_out_dir()
  add_windows_cef_manifest("${CEF_TARGET_OUT_DIR}" "${MODULE_PATH}" "${CEF_TARGET}" "exe")
endfunction ()

function(run_cef_linux_config CEF_TARGET CEF_ROOT)
  # Executable target.
  add_dependencies(${CEF_TARGET} libcef_dll_wrapper)
  target_link_libraries(${CEF_TARGET} PUBLIC libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
  include_directories(${CEF_ROOT})

  add_dependencies(${CEF_TARGET} libcef_dll_wrapper "${CEF_HELPER_TARGET}")

  if (USE_SANDBOX)
    # Logical target used to link the cef_sandbox library.
    message(STATUS "Using CEF in Sandboxed mode.")
    add_logical_target("cef_sandbox_lib" "${CEF_SANDBOX_LIB_DEBUG}" "${CEF_SANDBOX_LIB_RELEASE}")
    target_link_libraries(${CEF_TARGET} cef_sandbox_lib ${CEF_SANDBOX_STANDARD_LIBS})
  endif ()

  # Add the custom manifest files to the executable.
  set_openspace_cef_target_out_dir()
endfunction ()

function(set_modules_dependency_on_cef_libraries LIB_DEPENDENT)
  if (WIN32 OR OS_LINUX)
    target_link_libraries(${LIB_DEPENDENT} INTERFACE libcef_lib)
  endif ()
  target_link_libraries(${LIB_DEPENDENT} INTERFACE libcef_dll_wrapper)
endfunction ()

