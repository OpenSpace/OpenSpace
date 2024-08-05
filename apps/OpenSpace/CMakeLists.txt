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

include(${PROJECT_SOURCE_DIR}/ext/ghoul/support/cmake/message_macros.cmake)
include(${PROJECT_SOURCE_DIR}/support/cmake/application_definition.cmake)

# We are getting all_enabled_modules from the handle_applications.cmake file which gets
# it from the main CMakeLists file

#####
# OpenVR
#####
option(OPENSPACE_OPENVR_SUPPORT "Build OpenSpace application with OpenVR support" OFF)

if (OPENSPACE_OPENVR_SUPPORT)
  begin_header("Dependency: OpenVR")
  find_package(OpenVR REQUIRED)

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


#####
# MacOS
#####
if (APPLE)
  set(MACOSX_BUNDLE_ICON_FILE openspace.icns)
  set(CMAKE_OSX_DEPLOYMENT_TARGET "10.15" CACHE STRING "Minimum OS X deployment version" FORCE)
  set(CMAKE_XCODE_ATTRIBUTE_OTHER_CODE_SIGN_FLAGS "--deep" CACHE STRING "Other Code Signing Flags" FORCE)
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

set(SGCT_TEXT OFF CACHE BOOL "" FORCE)
set(SGCT_DEP_INCLUDE_FREETYPE OFF CACHE BOOL "" FORCE)
set(SGCT_DEP_INCLUDE_FMT OFF CACHE BOOL "" FORCE)
set(SGCT_DEP_INCLUDE_SCN OFF CACHE BOOL "" FORCE)
set(SGCT_DEP_INCLUDE_CATCH2 OFF CACHE BOOL "" FORCE)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ext/sgct)
target_link_libraries(OpenSpace PRIVATE sgct)

set_target_properties(sgct PROPERTIES FOLDER "External")
set_target_properties(glfw PROPERTIES FOLDER "External")
if (TARGET SGCTTest)
  set_target_properties(SGCTTest PROPERTIES FOLDER "External")
endif ()

if (UNIX AND (NOT APPLE))
  target_link_libraries(OpenSpace PRIVATE Xcursor Xinerama X11)
endif ()

add_custom_command(TARGET OpenSpace POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/ext/sgct/sgct.schema.json
  ${CMAKE_CURRENT_SOURCE_DIR}/../../config/schema/sgct.schema.json
  COMMAND_EXPAND_LISTS
)

end_header("Dependency: SGCT")

begin_header("Dependency: Profile Editor")
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ext/launcher)
target_link_libraries(OpenSpace PRIVATE openspace-ui-launcher)
end_header("Dependency: Profile Editor")

if (WIN32)
  # Find the windeployqt application
  get_target_property(_qmake_executable Qt6::qmake IMPORTED_LOCATION)
  get_filename_component(_qt_bin_dir "${_qmake_executable}" DIRECTORY)
  find_program(WINDEPLOYQT_EXECUTABLE windeployqt HINTS "${_qt_bin_dir}")
  add_custom_command(
    TARGET OpenSpace POST_BUILD
    COMMAND "${CMAKE_COMMAND}" -E env PATH="${_qt_bin_dir}" "${WINDEPLOYQT_EXECUTABLE}" --verbose 0 --no-compiler-runtime --no-translations \"$<TARGET_FILE:OpenSpace>\"
    COMMENT "Deploying Qt libraries"
  )
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
elseif ()
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
