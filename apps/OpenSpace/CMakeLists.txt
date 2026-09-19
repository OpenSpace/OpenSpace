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

include(${PROJECT_SOURCE_DIR}/ext/ghoul/support/cmake/message_macros.cmake)
include(${PROJECT_SOURCE_DIR}/support/cmake/application_definition.cmake)

# We are getting all_enabled_modules from the handle_applications.cmake file which gets
# it from the main CMakeLists file

#####
# OpenVR
#####
# OPENSPACE_OPENVR_SUPPORT is declared in the top-level CMakeLists.txt (before project())
# so it can drive the vcpkg "openvr" manifest feature.
if (OPENSPACE_OPENVR_SUPPORT)
  begin_header("Dependency: OpenVR")
  find_package(openvr CONFIG REQUIRED)

  set(SGCT_OPENVR_DEFINITIONS OPENVR_SUPPORT)
  if (NOT SGCT_OPENVR_INCLUDE_DIRECTORY)
    if (WIN32)
      find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
        NAMES SGCTOpenVR.h
        PATHS ${PROJECT_SOURCE_DIR}/ext/sgct/additional_includes/openvr NO_DEFAULT_PATH
        REQUIRED
      )
    else ()
      find_path(SGCT_OPENVR_INCLUDE_DIRECTORY
        NAMES SGCTOpenVR.h
        PATH_SUFFIXES SGCTOpenVR
        PATHS ${PROJECT_SOURCE_DIR}/ext/sgct/additional_includes/openvr
        REQUIRED
      )
    endif ()
  else ()
    set(SGCT_OPENVR_FILES
      ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.h
      ${SGCT_OPENVR_INCLUDE_DIRECTORY}/SGCTOpenVR.cpp
    )
  endif ()
  end_header("Dependency: OpenVR")
endif ()

#####
# Spout
#####
if (SGCT_SPOUT_SUPPORT AND NOT OPENSPACE_MODULE_SPOUT)
  message(WARNING "Spout support from SGCT was requested, but OpenSpace Spout module was disabled")
  message(STATUS "Enabling module as dependency")
  set(OPENSPACE_MODULE_SPOUT ON CACHE BOOL "Build OPENSPACE_MODULE_SPOUTModule" FORCE)
endif ()


create_new_application(OpenSpace
  ${SGCT_OPENVR_FILES}
  ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.rc
  ${CMAKE_CURRENT_SOURCE_DIR}/openspace.icns
)

set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/openspace.icns PROPERTIES MACOSX_PACKAGE_LOCATION "Resources")

target_include_directories(
  OpenSpace PRIVATE
  ${OPENVR_INCLUDE_DIRS}
  ${SGCT_OPENVR_INCLUDE_DIRECTORY}
  ${SPOUT_INCLUDE_DIRS}
)

target_link_libraries(OpenSpace PRIVATE openspace-core openspace-module-collection ${OPENVR_LIBRARY} ${SPOUT_LIBRARY})

target_compile_definitions(OpenSpace PRIVATE
  ${SGCT_OPENVR_DEFINITIONS}
  ${SPOUT_DEFINITIONS}
)

if (OPENSPACE_BREAK_ON_FLOATING_POINT_EXCEPTION)
  target_compile_definitions(OpenSpace PRIVATE "OPENSPACE_BREAK_ON_FLOATING_POINT_EXCEPTION")
endif ()

begin_header("Dependency: SGCT")

# SGCT is resolved through the vcpkg overlay port at
# apps/OpenSpace/ext/sgct/support/vcpkg/ports/sgct, which builds the enclosing submodule
# checkout. The sgct submodule stays checked out because that port builds from its source
# path (and additional_includes/openvr is still consumed from it above).
find_package(sgct CONFIG REQUIRED)
target_link_libraries(OpenSpace PRIVATE sgct::sgct)

if (UNIX)
  target_link_libraries(OpenSpace PRIVATE Xcursor Xinerama X11)
endif ()

add_custom_command(TARGET OpenSpace POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/ext/sgct/sgct.schema.json
  ${CMAKE_CURRENT_SOURCE_DIR}/../../config/schema/sgct.schema.json
  COMMAND_EXPAND_LISTS
)

end_header("Dependency: SGCT")

# Profile Editor / Launcher GUI
option(OPENSPACE_APPLICATION_OPENSPACE-LAUNCHER "Build Launcher GUI" ON)
if (OPENSPACE_APPLICATION_OPENSPACE-LAUNCHER)
  begin_header("Dependency: Profile Editor")
  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ext/launcher)
  target_link_libraries(OpenSpace PRIVATE openspace-ui-launcher)
  target_compile_definitions(OpenSpace PRIVATE OPENSPACE_HAS_LAUNCHER)
  end_header("Dependency: Profile Editor")

  if (WIN32)
    # Qt comes from vcpkg and is linked dynamically (LGPL compliance). The Qt DLLs
    # themselves are picked up by the $<TARGET_RUNTIME_DLLS:OpenSpace> copy further down,
    # but the runtime plugins (platforms/qwindows.dll, styles, imageformats, tls, ...) are
    # loaded at runtime and not seen as link dependencies, so windeployqt is run to place
    # them next to the executable.
    # Qt6CoreMacros.cmake itself does a plain find_program(WINDEPLOYQT_EXECUTABLE ...)
    # internally (for its own deploy-script support) with no reliable hint towards
    # vcpkg's own Qt install, so on a machine with an unrelated Qt SDK on PATH it caches
    # WINDEPLOYQT_EXECUTABLE pointing at that other (potentially different-version) copy.
    # Qt then refuses to load the mismatched-version plugins that tool deploys ("Ignoring
    # QPA plugin due to mismatching Qt versions") - only visible in
    # Release/RelWithDebInfo/MinSizeRel because the --debug lookup happens to fall back to
    # the target's actual Qt dependencies when no bundled debug plugin exists, while
    # --release does not. Use our own cache variable name so we don't just read back Qt's
    # already-wrong cached value, and explicitly restrict the search to vcpkg's host-tools
    # Qt6 install so we always get the exact windeployqt matching the linked Qt version.
    file(GLOB _openspace_qt_host_tools_dirs "${_VCPKG_INSTALLED_DIR}/*/tools/Qt6/bin")
    find_program(OPENSPACE_WINDEPLOYQT_EXECUTABLE
      NAMES windeployqt
      HINTS ${_openspace_qt_host_tools_dirs}
      NO_DEFAULT_PATH
    )
    unset(_openspace_qt_host_tools_dirs)
    find_program(OPENSPACE_WINDEPLOYQT_EXECUTABLE NAMES windeployqt REQUIRED)

    get_filename_component(_openspace_windeployqt_dir "${OPENSPACE_WINDEPLOYQT_EXECUTABLE}" DIRECTORY)
    find_program(OPENSPACE_WINDEPLOYQT_DEBUG_EXECUTABLE
      NAMES windeployqt.debug.bat
      HINTS ${_openspace_windeployqt_dir}
      NO_DEFAULT_PATH
    )
    unset(_openspace_windeployqt_dir)
    if (NOT OPENSPACE_WINDEPLOYQT_DEBUG_EXECUTABLE)
      set(OPENSPACE_WINDEPLOYQT_DEBUG_EXECUTABLE ${OPENSPACE_WINDEPLOYQT_EXECUTABLE})
    endif ()

    add_custom_command(
      TARGET OpenSpace POST_BUILD
      COMMAND $<IF:$<CONFIG:Debug>,${OPENSPACE_WINDEPLOYQT_DEBUG_EXECUTABLE},${OPENSPACE_WINDEPLOYQT_EXECUTABLE}>
        $<IF:$<CONFIG:Debug>,--debug,--release>
        --no-translations
        --no-compiler-runtime
        --verbose=0
        "$<TARGET_FILE:OpenSpace>"
      # windeployqt.exe is itself a host tool built for the "x64-windows" (dynamic)
      # triplet, and it fails to detect that OpenSpace actually links the
      # "x64-windows-static-md" triplet's Qt (vcpkg splits libs under bin/ from plugins
      # under Qt6/plugins/, which confuses its usual same-prefix layout heuristic). It
      # ends up deploying its own host triplet's plugins, which are a different,
      # ABI-incompatible Qt build (same version number, but "Cannot load library ...:
      # The specified procedure could not be found" at runtime). Overwrite whatever it
      # copied with the plugins from the triplet actually linked into OpenSpace.
      COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/$<IF:$<CONFIG:Debug>,debug/,>Qt6/plugins"
        "$<TARGET_FILE_DIR:OpenSpace>"
      COMMENT "Deploying Qt runtime dependencies via windeployqt"
      VERBATIM
    )
  endif ()

  if (UNIX)
    add_custom_command(TARGET OpenSpace POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/$<IF:$<CONFIG:Debug>,debug/,>Qt6/plugins"
        "$<TARGET_FILE_DIR:OpenSpace>"
      COMMENT "Deploying Qt runtime plugins"
      VERBATIM
    )
  endif ()
endif ()

# Web Browser and Web gui
# Why not put these in the module's path? Because they do not have access to the
# target as of July 2017, which is needed.
if (OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  # wanted by CEF
  set(PROJECT_ARCH "x86_64")

  if (WIN32)
    set(RESOURCE_FILE openspace.rc)
  endif ()

  # Add the CEF binary distribution's cmake/ directory to the module path and
  # find CEF to initialize it properly.
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBBROWSER_MODULE_PATH}/cmake")
  include(webbrowser_helpers)

  set_cef_targets("${CEF_ROOT}" OpenSpace)
  run_cef_platform_config("${CEF_ROOT}" "${CEF_TARGET}" "${WEBBROWSER_MODULE_PATH}")
else ()
  message(WARNING "Web configured to be included, but no CEF_ROOT was found, please try configuring CMake again")
endif ()

if (MSVC)
  begin_header("Dependency: Dbghelp")
  # This library is used for being able to output the callstack if an exception escapes
  target_link_libraries(OpenSpace PRIVATE Dbghelp.lib)
  end_header()
endif ()

if (OPENSPACE_NVTOOLS_ENABLED)
  target_link_libraries(OpenSpace PRIVATE "${OPENSPACE_NVTOOLS_PATH}/lib/x64/nvToolsExt64_1.lib")
endif ()

if (WIN32)
  add_custom_command(TARGET OpenSpace POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_RUNTIME_DLLS:OpenSpace> $<TARGET_FILE_DIR:OpenSpace>
    COMMAND_EXPAND_LISTS
  )
endif ()
