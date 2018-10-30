##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2018                                                                #
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
    SET_OPENSPACE_CEF_TARGET_OUT_DIR()

    # main CEF executable target
    set(CEF_TARGET ${main_target} PARENT_SCOPE)
endfunction()

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
    endif()
    if (OS_WINDOWS)
        run_cef_windows_config("${cef_target}" "${cef_root}" "${module_path}")
    endif()
    if (OS_LINUX)
        run_cef_linux_config("${cef_target}")
    endif()
endfunction()

function(run_cef_macosx_config CEF_ROOT module_path)
    set(CEF_FINAL_APP "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug/${CEF_TARGET}.app")
    set(CEF_FINAL_HELPER_APP "${CEF_FINAL_APP}/Contents/${CEF_HELPER_TARGET}.app")
    set(CEF_FRAMEWORK_LOCATION "${CEF_BINARY_DIR}/Chromium Embedded Framework.framework")
    set(CEF_FRAMEWORK_FINAL_LOCATION "${CEF_FINAL_APP}/Contents/Frameworks/Chromium Embedded Framework.framework")

    add_dependencies(${CEF_TARGET} libcef_dll_wrapper "${CEF_HELPER_TARGET}")

    target_link_libraries(${CEF_TARGET} libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
    set_target_properties(${CEF_TARGET} PROPERTIES
        RESOURCE "${WEBBROWSER_RESOURCES_SRCS}"
        MACOSX_BUNDLE_INFO_PLIST ${module_path}/mac/Info.plist
        )

    # Copy files into the main app bundle.
    add_custom_command(
        TARGET ${CEF_TARGET}
        POST_BUILD
        # Copy the helper app bundle into the Frameworks directory.
        COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Debug${CEF_HELPER_APP}" "${CEF_FINAL_HELPER_APP}"
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

    # Fix the framework rpath in the main executable.
    FIX_MACOSX_MAIN_FRAMEWORK_RPATH(${CEF_TARGET})

    if(NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
        # Manually process and copy over resource files.
        # The Xcode generator handles this via the set_target_properties RESOURCE directive.
        set(PREFIXES "mac/")  # Remove these prefixes from input file paths.
        COPY_MACOSX_RESOURCES("${WEBBROWSER_RESOURCES_SOURCES}" "${PREFIXES}" "${CEF_TARGET}" "${module_path}" "${CEF_FINAL_APP}")
    endif()
endfunction()

function(run_cef_windows_config CEF_TARGET CEF_ROOT MODULE_PATH)
    # Executable target.
    add_dependencies(${CEF_TARGET} libcef_dll_wrapper)
    target_link_libraries(${CEF_TARGET} libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
    include_directories(${CEF_ROOT})

    add_dependencies(${CEF_TARGET} libcef_dll_wrapper "${CEF_HELPER_TARGET}")

    if(USE_SANDBOX)
        # Logical target used to link the cef_sandbox library.
        message(STATUS "Using CEF in Sandboxed mode.")
        ADD_LOGICAL_TARGET("cef_sandbox_lib" "${CEF_SANDBOX_LIB_DEBUG}" "${CEF_SANDBOX_LIB_RELEASE}")
        target_link_libraries(${CEF_TARGET} cef_sandbox_lib ${CEF_SANDBOX_STANDARD_LIBS})
    endif()

    # Add the custom manifest files to the executable.
    SET_OPENSPACE_CEF_TARGET_OUT_DIR()
    ADD_WINDOWS_CEF_MANIFEST("${CEF_TARGET_OUT_DIR}" "${MODULE_PATH}" "${CEF_TARGET}" "exe")

    # Copy binary and resource files to the target output directory.
    copy_files("${CEF_TARGET}" "${CEF_BINARY_FILES}" "${CEF_BINARY_DIR}" "$<TARGET_FILE_DIR:${CEF_TARGET}>")
    copy_files("${CEF_TARGET}" "${CEF_RESOURCE_FILES}" "${CEF_RESOURCE_DIR}" "$<TARGET_FILE_DIR:${CEF_TARGET}>")
endfunction()

function(run_cef_linux_config CEF_ROOT)
    message(ERROR "Linux is not yet supported for Web Browser Module.")
endfunction()
