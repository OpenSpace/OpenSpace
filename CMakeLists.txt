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

cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

project(OpenSpace)

set(OPENSPACE_VERSION_MAJOR 0)
set(OPENSPACE_VERSION_MINOR 14)
set(OPENSPACE_VERSION_PATCH 1)
set(OPENSPACE_VERSION_STRING "Beta-4")


set(OPENSPACE_BASE_DIR "${PROJECT_SOURCE_DIR}")
set(OPENSPACE_CMAKE_EXT_DIR "${OPENSPACE_BASE_DIR}/support/cmake")
set(GHOUL_BASE_DIR "${OPENSPACE_BASE_DIR}/ext/ghoul")

include(${OPENSPACE_CMAKE_EXT_DIR}/module_common.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/global_variables.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/handle_applications.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/handle_modules.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/copy_shared_libraries.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/handle_external_library.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/message_macros.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/include_gtest.cmake)

begin_header("Configuring OpenSpace project")
message(STATUS "CMake version: ${CMAKE_VERSION}")

# Bail out if the user tries to generate a 32 bit project.
if (NOT ${CMAKE_SIZEOF_VOID_P} EQUAL 8)
  message(FATAL_ERROR "OpenSpace can only be generated for 64 bit architectures.")
endif()

##########################################################################################
# Cleanup project                                                                        #
##########################################################################################
set(OPENSPACE_APPS_DIR "${OPENSPACE_BASE_DIR}/apps")
set(OPENSPACE_EXT_DIR "${OPENSPACE_BASE_DIR}/ext")

if (NOT EXISTS ${OPENSPACE_EXT_DIR}/ghoul/CMakeLists.txt)
  message(FATAL_ERROR "Git submodules are missing. Please run \n"
    "git submodule update --init --recursive \n"
    "to download the missing dependencies."
  )
endif()

set_property(GLOBAL PROPERTY USE_FOLDERS On)
set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER CMake)

mark_as_advanced(CMAKE_BACKWARDS_COMPATIBILITY CMAKE_BUILD_TYPE CMAKE_DEBUG_POSTFIX
  CMAKE_INSTALL_PREFIX CMAKE_OSX_ARCHITECTURES CMAKE_OSX_DEPLOYMENT_TARGET
  CMAKE_OSX_SYSROOT CMAKE_RELEASE_POSTFIX)

# Set build output directories
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_CMAKE_EXT_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OPENSPACE_BASE_DIR}/bin)

##########################################################################################
# Main                                                                                   #
##########################################################################################

# Get the current working branch
execute_process(
  COMMAND git rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE OPENSPACE_GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the latest abbreviated commit hash of the working branch
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE OPENSPACE_GIT_COMMIT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# See if working directory is clean or not
execute_process(
  COMMAND git diff-index --quiet HEAD --
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE OPENSPACE_GIT_STATUS_RETURN
)
if (NOT OPENSPACE_GIT_STATUS_RETURN EQUAL 0)
  set(OPENSPACE_GIT_STATUS "uncommitted changes")
else()
  set(OPENSPACE_GIT_STATUS "")
endif()

option(OPENSPACE_WARNINGS_AS_ERRORS "Treat warnings as errors" OFF)

include(src/CMakeLists.txt)

##########################################################################################
# Add external dependencies                                                              #
##########################################################################################
# System libraries
if (APPLE)
  begin_dependency("Core Libraries")
  target_include_directories(openspace-core PUBLIC "/Developer/Headers/FlatCarbon")
  find_library(COREFOUNDATION_LIBRARY CoreFoundation)
  find_library(CARBON_LIBRARY Carbon)
  find_library(COCOA_LIBRARY Carbon)
  find_library(APP_SERVICES_LIBRARY ApplicationServices)
  mark_as_advanced(CARBON_LIBRARY COCOA_LIBRARY APP_SERVICES_LIBRARY)
  target_link_libraries(openspace-core ${CARBON_LIBRARY} ${COREFOUNDATION_LIBRARY}
    ${COCOA_LIBRARY} ${APP_SERVICES_LIBRARY})
  end_dependency()
endif()

# Ghoul
add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul)
target_link_libraries(openspace-core Ghoul)
set_folder_location(Lua "External")
set_folder_location(lz4 "External")
set_folder_location(GhoulTest "Unit Tests")
link_directories("${GHOUL_LIBRARY_DIRS}")

# Spice
begin_dependency("Spice")
add_subdirectory(${OPENSPACE_EXT_DIR}/spice)
target_link_libraries(openspace-core Spice)
set_folder_location(Spice "External")
end_dependency()

# Curl
begin_dependency("CURL")
if (WIN32)
  set(CURL_ROOT_DIR "${OPENSPACE_EXT_DIR}/curl")
  target_include_directories(openspace-core SYSTEM PUBLIC ${CURL_ROOT_DIR}/include)
  target_link_libraries(openspace-core ${CURL_ROOT_DIR}/lib/libcurl.lib)
  target_compile_definitions(openspace-core PUBLIC "OPENSPACE_CURL_ENABLED" "CURL_STATICLIB")
else ()
  find_package(CURL)
  if (CURL_FOUND)
    target_include_directories(openspace-core SYSTEM PUBLIC ${CURL_INCLUDE_DIRS})
    target_link_libraries(openspace-core ${CURL_LIBRARIES})
    target_compile_definitions(openspace-core PUBLIC "OPENSPACE_CURL_ENABLED")
  endif ()
endif()
end_dependency()

# Qt
# Unfortunately, we have to set this value manually; sigh
# In the future, if the Qt version is updated, just add to this variable ---abock
if (APPLE)
  set(CMAKE_PREFIX_PATH
    "~/Qt/5.6/clang_64/lib/cmake"
    "~/Qt/5.7/clang_64/lib/cmake"
    "~/Qt/5.8/clang_64/lib/cmake"
    "~/Qt/5.9/clang_64/lib/cmake"
    "~/Qt/5.10/clang_64/lib/cmake"
    "~/Qt/5.11/clang_64/lib/cmake"
    "~/Qt/5.12/clang_64/lib/cmake"
  )
endif ()

if (MSVC)
  option(OPENSPACE_ENABLE_VLD "Enable the Visual Leak Detector" OFF)
  if (OPENSPACE_ENABLE_VLD)
    begin_dependency("Visual Leak Detector")
    target_compile_definitions(openspace-core PUBLIC "OPENSPACE_ENABLE_VLD")
    target_link_libraries(openspace-core ${OPENSPACE_EXT_DIR}/vld/lib/vld.lib)
    target_include_directories(openspace-core PUBLIC ${OPENSPACE_EXT_DIR}/vld)
    end_dependency()
  endif ()

  option(OPENSPACE_VTUNE_ENABLED "Include VTune support" OFF)
  set(OPENSPACE_VTUNE_PATH "C:/Program Files (x86)/IntelSWTools/VTune Amplifier 2019" CACHE STRING "Path to VTune installation")
  if (OPENSPACE_VTUNE_ENABLED)
    begin_dependency("Intel VTune")
    target_compile_definitions(openspace-core PUBLIC "OPENSPACE_HAS_VTUNE")
    target_include_directories(openspace-core PUBLIC "${OPENSPACE_VTUNE_PATH}/include")
    target_link_libraries(openspace-core "${OPENSPACE_VTUNE_PATH}/lib64/libittnotify.lib")
    end_dependency()
  endif ()

  option(OPENSPACE_NVTOOLS_ENABLED "Include support for Nvidia Tools Extensions" OFF)
  set(OPENSPACE_NVTOOLS_PATH "C:/Program Files/NVIDIA Corporation/NvToolsExt")
  if (OPENSPACE_NVTOOLS_ENABLED)
    begin_dependency("Nvidia Tools Extension")
    target_compile_definitions(openspace-core PUBLIC "OPENSPACE_HAS_NVTOOLS")
    target_include_directories(openspace-core PUBLIC "${OPENSPACE_NVTOOLS_PATH}/include")
    end_dependency()
  endif ()

endif ()

##########################################################################################
# Tests                                                                                  #
##########################################################################################
option(OPENSPACE_HAVE_TESTS "Activate the OpenSpace unit tests" ON)
if (OPENSPACE_HAVE_TESTS)
  include_gtest("${GHOUL_BASE_DIR}/ext/googletest")

  file(GLOB_RECURSE OPENSPACE_TEST_FILES ${OPENSPACE_BASE_DIR}/tests/*.inl)
  add_executable(OpenSpaceTest ${OPENSPACE_BASE_DIR}/tests/main.cpp ${OPENSPACE_TEST_FILES})

  target_include_directories(OpenSpaceTest PUBLIC
    "${OPENSPACE_BASE_DIR}/include"
    "${OPENSPACE_BASE_DIR}/tests"
    "${OPENSPACE_EXT_DIR}/ghoul/ext/googletest/googletest/include"
  )
  target_compile_definitions(OpenSpaceTest PUBLIC
    "GHL_THROW_ON_ASSERT" "GTEST_HAS_TR1_TUPLE=0"
  )
  target_link_libraries(OpenSpaceTest gtest openspace-core)

  set_folder_location(OpenSpaceTest "Unit Tests")

  if (MSVC)
    set_target_properties(OpenSpaceTest PROPERTIES LINK_FLAGS
      "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
    )
  endif ()
  set_openspace_compile_settings(OpenSpaceTest)
endif (OPENSPACE_HAVE_TESTS)


begin_header("Configuring Modules")
set(OPENSPACE_EXTERNAL_MODULES_PATHS "" CACHE STRING "List of external modules")
handle_modules("${OPENSPACE_BASE_DIR}/modules" "${OPENSPACE_EXTERNAL_MODULES_PATHS}")
end_header("End: Configuring Modules")
message(STATUS "")

begin_header("Configuring Applications")
handle_applications()
end_header("End: Configuring Applications")
message(STATUS "")


# Web Browser and Web gui
# Why not put these in the module's path? Because they do not have access to the
# target as of July 2017, which is needed.
if (OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  # wanted by CEF
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "CMAKE_BUILD_TYPE")

  set(PROJECT_ARCH "x86_64")

  if (WIN32)
    set(RESOURCE_FILE ${OPENSPACE_APPS_DIR}/OpenSpace/openspace.rc)
  endif ()

  # Add the CEF binary distribution's cmake/ directory to the module path and
  # find CEF to initialize it properly.
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${WEBBROWSER_MODULE_PATH}/cmake")
  include(webbrowser_helpers)

  if (TARGET OpenSpaceTest)
    set_cef_targets("${CEF_ROOT}" OpenSpaceTest)
    run_cef_platform_config("${CEF_ROOT}" "${CEF_TARGET}" "${WEBBROWSER_MODULE_PATH}")
  endif ()
elseif (OPENSPACE_MODULE_WEBBROWSER)
  message(WARNING "Web configured to be included, but no CEF_ROOT was found, please try configuring CMake again.")
endif ()

##########################################################################################
# Misc settings                                                                          #
##########################################################################################
option(OPENSPACE_WITH_ABUFFER_RENDERER "Compile ABuffer Renderer" OFF)
if (OPENSPACE_WITH_ABUFFER_RENDERER)
  target_compile_definitions(openspace-core PUBLIC "OPENSPACE_WITH_ABUFFER_RENDERER")
endif ()

option(OPENSPACE_WITH_INSTRUMENTATION "Add instrumentation options" OFF)
if (OPENSPACE_WITH_INSTRUMENTATION)
  target_compile_definitions(openspace-core PUBLIC "OPENSPACE_WITH_INSTRUMENTATION")
endif ()


# Just in case, create the bin directory
add_custom_command(
  TARGET openspace-core
  PRE_BUILD COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)

# Manage the CPack packaging
include(${OPENSPACE_CMAKE_EXT_DIR}/packaging.cmake)

end_header("End: Configuring OpenSpace project")
