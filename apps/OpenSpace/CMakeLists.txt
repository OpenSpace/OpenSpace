##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2017                                                                #
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

include(${GHOUL_BASE_DIR}/support/cmake/CopySharedLibraries.cmake)

set(APPLICATION_NAME OpenSpace)
set(APPLICATION_LINK_TO_OPENSPACE ON)

# wanted by CEF
set(CMAKE_BUILD_TYPE Debug CACHE INTERNAL "CMAKE_BUILD_TYPE")

if (WIN32)
    set(RESOURCE_FILE ${OPENSPACE_APPS_DIR}/OpenSpace/openspace.rc)
endif ()


########################
# OpenVR section start #
########################
option(OPENSPACE_OPENVR_SUPPORT "Build OpenSpace application with OpenVR support" OFF)

if (OPENSPACE_OPENVR_SUPPORT)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${OPENSPACE_EXT_DIR}/sgct/cmake/modules/")

    find_package(OpenVR REQUIRED)

    set(SGCT_OPENVR_DEFINITIONS OPENVR_SUPPORT)

    if(NOT SGCT_OPENVR_INCLUDE_DIRECTORY)
        if( WIN32 )
            find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
                NAMES SGCTOpenVR.h
                PATHS $ENV{SGCT_ROOT_DIR}/additional_includes/openvr ${OPENSPACE_EXT_DIR}/sgct/additional_includes/openvr NO_DEFAULT_PATH
                REQUIRED)
        else()
            find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
                NAMES SGCTOpenVR.h
                PATH_SUFFIXES SGCTOpenVR
                PATHS $ENV{SGCT_ROOT_DIR}/additional_includes/openvr ${OPENSPACE_EXT_DIR}/sgct/additional_includes/openvr
                REQUIRED)
        endif()
    else()
        set(SGCT_OPENVR_FILES ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.h ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.cpp)
    endif()
else()
    set(OPENVR_INCLUDE_DIRS "")
    set(SGCT_OPENVR_INCLUDE_DIRECTORY "")
    set(OPENVR_LIBRARY "")
    set(SGCT_OPENVR_DEFINITIONS "")
endif()
########################
#  OpenVR section end  #
########################

########################
# Spout section  start #
########################
if (WIN32)
    option(OPENSPACE_SPOUT_SUPPORT "Build OpenSpace application with Spout support" OFF)
endif ()

set(SPOUT_INCLUDE_DIRS "")
set(SPOUT_LIBRARY "")
set(SPOUT_DEFINITIONS "")
if (OPENSPACE_SPOUT_SUPPORT)
    set(SPOUT_INCLUDE_DIRS ${OPENSPACE_APPS_DIR}/OpenSpace/ext/spout)
    set(SPOUT_LIBRARY ${OPENSPACE_APPS_DIR}/OpenSpace/ext/spout/SpoutLibrary.lib)
    set(SPOUT_DEFINITIONS "OPENSPACE_HAS_SPOUT")
endif ()

########################
#  Spout section  end  #
########################

# Building on MacOS? Bundle int into an .app!
if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    add_executable(${APPLICATION_NAME} MACOSX_BUNDLE
        ${SGCT_OPENVR_FILES}
        ${OPENSPACE_APPS_DIR}/OpenSpace/main.cpp
        ${RESOURCE_FILE}
    )
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    add_executable(${APPLICATION_NAME}
        ${SGCT_OPENVR_FILES}
        ${OPENSPACE_APPS_DIR}/OpenSpace/main.cpp
        ${RESOURCE_FILE}
    )
else ()
    add_executable(${APPLICATION_NAME}
        ${SGCT_OPENVR_FILES}
        ${OPENSPACE_APPS_DIR}/OpenSpace/main.cpp
        ${RESOURCE_FILE}
    )
endif ()

if (OPENSPACE_SPOUT_SUPPORT)
    ghl_copy_files(
        ${APPLICATION_NAME}
        ${OPENSPACE_APPS_DIR}/OpenSpace/ext/spout/SpoutLibrary.dll
    )
endif ()

target_include_directories(
    ${APPLICATION_NAME} PUBLIC
    ${OPENSPACE_BASE_DIR}/include
    ${OPENVR_INCLUDE_DIRS}
    ${SGCT_OPENVR_INCLUDE_DIRECTORY}
    ${SPOUT_INCLUDE_DIRS}
)

target_link_libraries(${APPLICATION_NAME}
    libOpenSpace
    ${OPENVR_LIBRARY}
    ${SPOUT_LIBRARY}
)

if (OPENSPACE_MODULE_WEBGUI AND CEF_ROOT)
    # Add the CEF binary distribution's cmake/ directory to the module path and
    # find CEF to initialize it properly.
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBGUI_MODULE_PATH}/cmake")
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CEF_ROOT}/cmake")
    find_package(CEF REQUIRED)

    # include CEF support functions and macros
    include(cef_support)

    # ensure out target dir is set
    SET_OPENSPACE_CEF_TARGET_OUT_DIR()

    # main CEF executable target
    set(CEF_TARGET ${APPLICATION_NAME})
    #SET_EXECUTABLE_TARGET_PROPERTIES(${CEF_TARGET})

    # Logical target used to link the libcef library.
    ADD_LOGICAL_TARGET("libcef_lib" "${CEF_LIB_DEBUG}" "${CEF_LIB_RELEASE}")
    message(STATUS "------------------- CEF_LIB_DEBUG: ${CEF_LIB_DEBUG}")
    message(STATUS "------------------- CEF_LIB_RELEASE: ${CEF_LIB_RELEASE}")

    # platform specific config
    if(OS_MACOSX)
        set(CEF_APP "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CEF_TARGET}.app")
        set(CEF_FINAL_APP "${CEF_TARGET_OUT_DIR}/${CEF_TARGET}.app")

        add_dependencies(${CEF_TARGET} libcef_dll_wrapper "${CEF_HELPER_TARGET}")

        target_link_libraries(${CEF_TARGET} libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
        set_target_properties(${CEF_TARGET} PROPERTIES
            RESOURCE "${WEBGUI_RESOURCES_SRCS}"
            MACOSX_BUNDLE_INFO_PLIST ${WEBGUI_MODULE_PATH}/mac/Info.plist
        )

        # Copy files into the main app bundle.
        add_custom_command(
            TARGET ${CEF_TARGET}
            POST_BUILD
            # Copy the built app bundle into the proper cmake build type directory.
            COMMAND ${CMAKE_COMMAND} -E copy_directory
                    "${CEF_APP}"
                    "${CEF_FINAL_APP}"
            # Copy the helper app bundle into the Frameworks directory.
            COMMAND ${CMAKE_COMMAND} -E copy_directory
                    "${CEF_HELPER_APP}"
                    "${CEF_FINAL_APP}/Contents/Frameworks/${CEF_HELPER_TARGET}.app"
            # Copy the CEF framework into the Frameworks directory.
            COMMAND ${CMAKE_COMMAND} -E copy_directory
                    "${CEF_BINARY_DIR}/Chromium Embedded Framework.framework"
                    "${CEF_FINAL_APP}/Contents/Frameworks/Chromium Embedded Framework.framework"
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
            COPY_MACOSX_RESOURCES("${WEBGUI_RESOURCES_SOURCES}" "${PREFIXES}" "${CEF_TARGET}" "${WEBGUI_MODULE_PATH}" "${CEF_FINAL_APP}")
        endif()
    endif (OS_MACOSX)

	if (OS_WINDOWS)
		# Executable target.
		add_dependencies(${CEF_TARGET} libcef_dll_wrapper)
		target_link_libraries(${CEF_TARGET} libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
        include_directories(${CEF_ROOT})

		if(USE_SANDBOX)
			# Logical target used to link the cef_sandbox library.
            message(STATUS "Using CEF in Sandboxed mode.")
			ADD_LOGICAL_TARGET("cef_sandbox_lib" "${CEF_SANDBOX_LIB_DEBUG}" "${CEF_SANDBOX_LIB_RELEASE}")
			target_link_libraries(${CEF_TARGET} cef_sandbox_lib ${CEF_SANDBOX_STANDARD_LIBS})
		endif()

		# Add the custom manifest files to the executable.
        ADD_WINDOWS_CEF_MANIFEST("${WEBGUI_MODULE_PATH}" "${CEF_TARGET}" "exe")

		# Copy binary and resource files to the target output directory.
		copy_files("${CEF_TARGET}" "${CEF_BINARY_FILES}" "${CEF_BINARY_DIR}" "$<TARGET_FILE_DIR:${CEF_TARGET}>")
		copy_files("${CEF_TARGET}" "${CEF_RESOURCE_FILES}" "${CEF_RESOURCE_DIR}" "$<TARGET_FILE_DIR:${CEF_TARGET}>")
	endif (OS_WINDOWS)
elseif (OPENSPACE_MODULE_WEBGUI)
    message(WARNING "WebGUI configured to be included, but no CEF_ROOT was found, please try configuring CMake again.")
endif ()



target_compile_definitions(${APPLICATION_NAME} PUBLIC
    ${SGCT_OPENVR_DEFINITIONS}
    ${SPOUT_DEFINITIONS}
)

if (MSVC)
    target_link_libraries(${APPLICATION_NAME} Dbghelp.lib)
    set_target_properties(${APPLICATION_NAME} PROPERTIES LINK_FLAGS
        "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
    )
endif()
