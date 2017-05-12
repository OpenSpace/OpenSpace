# Main CMakeLists.txt to build the Grok project using CMake (www.cmake.org)
# Written by Mathieu Malaterre

# This CMake project will by default create a library called Grok.
# But if you want to use this project within your own (CMake) project
# you will eventually like to prefix the library to avoid linking confusion
# For this purpose you can define a CMake var: GROK_NAMESPACE to whatever you like
# e.g.:
# set(GROK_NAMESPACE "FOO")
cmake_minimum_required(VERSION 3.1)

if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
  if (NOT (${CMAKE_VERSION} VERSION_LESS 3.0))
    cmake_policy(SET CMP0042 NEW)
  endif()
endif()

if(NOT GROK_NAMESPACE)
  set(GROK_NAMESPACE "GROK")
  set(GROK_STANDALONE 1)
endif()
# In all cases:
#string(TOLOWER ${GROK_NAMESPACE} GROK_LIBRARY_NAME)
set(GROK_LIBRARY_NAME openjp2)
set(GROK_PLUGIN_NAME openjp2_plugin)

project(${GROK_NAMESPACE} )

# Do full dependency headers.
include_regular_expression("^.*$")

#-----------------------------------------------------------------------------
# GROK version number, useful for packaging and doxygen doc:
set(GROK_VERSION_MAJOR 2)
set(GROK_VERSION_MINOR 1)
set(GROK_VERSION_BUILD 1)
set(GROK_VERSION
  "${GROK_VERSION_MAJOR}.${GROK_VERSION_MINOR}.${GROK_VERSION_BUILD}")
set(PACKAGE_VERSION
  "${GROK_VERSION_MAJOR}.${GROK_VERSION_MINOR}.${GROK_VERSION_BUILD}")

# Because autotools does not support X.Y notation for SOVERSION, we have to use
# two numbering, one for the Grok version and one for Grok soversion
# version | soversion
#   1.0   |  0
#   1.1   |  1
#   1.2   |  2
#   1.3   |  3
#   1.4   |  4
#   1.5   |  5
#   1.5.1 |  5
#   2.0   |  6
#   2.0.1 |  6
#   2.1   |  7
#   2.1.1 |  7
# above is the recommendation by the Grok team. If you really need to override this default,
# you can specify your own GROK_SOVERSION at cmake configuration time:
# cmake -DGROK_SOVERSION:STRING=42 /path/to/grok
if(NOT GROK_SOVERSION)
  set(GROK_SOVERSION 7)
endif(NOT GROK_SOVERSION)
set(GROK_LIBRARY_PROPERTIES
  VERSION   "${GROK_VERSION_MAJOR}.${GROK_VERSION_MINOR}.${GROK_VERSION_BUILD}"
  SOVERSION "${GROK_SOVERSION}"
)

# --------------------------------------------------------------------------
# Path to additional CMake modules
set(CMAKE_MODULE_PATH
    ${${GROK_NAMESPACE}_SOURCE_DIR}/cmake
    ${CMAKE_MODULE_PATH})

# --------------------------------------------------------------------------
# On Visual Studio 8 MS deprecated C. This removes all 1.276E1265 security
# warnings
if(WIN32)
  if(NOT BORLAND)
    if(NOT CYGWIN)
      if(NOT MINGW)
        if(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
          add_definitions(
            -D_CRT_FAR_MAPPINGS_NO_DEPRECATE
            -D_CRT_IS_WCTYPE_NO_DEPRECATE
            -D_CRT_MANAGED_FP_NO_DEPRECATE
            -D_CRT_NONSTDC_NO_DEPRECATE
            -D_CRT_SECURE_NO_DEPRECATE
            -D_CRT_SECURE_NO_DEPRECATE_GLOBALS
            -D_CRT_SETERRORMODE_BEEP_SLEEP_NO_DEPRECATE
            -D_CRT_TIME_FUNCTIONS_NO_DEPRECATE
            -D_CRT_VCCLRIT_NO_DEPRECATE
            -D_SCL_SECURE_NO_DEPRECATE
            )
        endif()
      endif()
    endif()
  endif()
endif()


# --------------------------------------------------------------------------
# Install directories
# Build DOCUMENTATION (not in ALL target and only if Doxygen is found)
option(BUILD_DOC "Build the HTML documentation (with doxygen if available)." OFF)

string(TOLOWER ${PROJECT_NAME} projectname)
set(GROK_INSTALL_SUBDIR "openjpeg-${GROK_VERSION_MAJOR}.${GROK_VERSION_MINOR}")

if(NOT GROK_INSTALL_BIN_DIR)
  set(GROK_INSTALL_BIN_DIR "bin")
endif()

if(NOT GROK_INSTALL_LIB_DIR)
  set(GROK_INSTALL_LIB_DIR "lib")
endif()

if(NOT GROK_INSTALL_SHARE_DIR)
  set(GROK_INSTALL_SHARE_DIR "share")
endif()

if(NOT GROK_INSTALL_DATA_DIR)
  set(GROK_INSTALL_DATA_DIR "${GROK_INSTALL_SHARE_DIR}/${GROK_INSTALL_SUBDIR}")
endif()

if(NOT GROK_INSTALL_INCLUDE_DIR)
  set(GROK_INSTALL_INCLUDE_DIR "include/${GROK_INSTALL_SUBDIR}")
endif()

if(BUILD_DOC)
if(NOT GROK_INSTALL_MAN_DIR)
  set(GROK_INSTALL_MAN_DIR "share/man/")
endif()

if(NOT GROK_INSTALL_DOC_DIR)
  set(GROK_INSTALL_DOC_DIR "share/doc/${GROK_INSTALL_SUBDIR}")
endif()
endif()

if(NOT GROK_INSTALL_PACKAGE_DIR)
  # We could install *.cmake files in share/ however those files contains
  # hardcoded path to libraries on a multi-arch system (fedora/debian) those
  # path will be different (lib/i386-linux-gnu vs lib/x86_64-linux-gnu)
  set(GROK_INSTALL_PACKAGE_DIR "${GROK_INSTALL_LIB_DIR}/${GROK_INSTALL_SUBDIR}")
endif()

if (APPLE)
	list(APPEND GROK_LIBRARY_PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/${GROK_INSTALL_LIB_DIR}")
	option(OPJ_USE_DSYMUTIL "Call dsymutil on binaries after build." OFF)
endif()

#-----------------------------------------------------------------------------
# Big endian test:
include (${CMAKE_ROOT}/Modules/TestBigEndian.cmake)
TEST_BIG_ENDIAN(OPJ_BIG_ENDIAN)

#-----------------------------------------------------------------------------
# Setup file for setting custom ctest vars
configure_file(
  ${${GROK_NAMESPACE}_SOURCE_DIR}/cmake/CTestCustom.cmake.in
  ${${GROK_NAMESPACE}_BINARY_DIR}/CTestCustom.cmake
  @ONLY
  )

#-----------------------------------------------------------------------------
# Grok build configuration options.
option(BUILD_SHARED_LIBS "Build Grok shared library and link executables against it." ON)
set (EXECUTABLE_OUTPUT_PATH ${GROK_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
set (LIBRARY_OUTPUT_PATH ${GROK_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
mark_as_advanced(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

#-----------------------------------------------------------------------------
# configure name mangling to allow multiple libraries to coexist
# peacefully
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/openjpeg_mangle.h.in)
set(MANGLE_PREFIX ${GROK_LIBRARY_NAME})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/openjpeg_mangle.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/openjpeg_mangle.h
               @ONLY)
endif()

#-----------------------------------------------------------------------------
# Compiler specific flags:
if(CMAKE_COMPILER_IS_GNUCC)
  # Do not use ffast-math for all build, it would produce incorrect results, only set for release:
  set(GROK_LIBRARY_COMPILE_OPTIONS ${GROK_LIBRARY_COMPILE_OPTIONS} "$<$<CONFIG:Release>:-ffast-math>")
endif()

#-----------------------------------------------------------------------------
# opj_config.h generation (1/2)

# Option Choose whether to use static runtime
include(ucm)
if(BUILD_SHARED_LIBS)
    ucm_set_runtime(DYNAMIC)
else()
    ucm_set_runtime(STATIC)
endif()

# Check if some include files are provided by the system
include(EnsureFileInclude)
# These files are mandatory
ensure_file_include("string.h"   HAVE_STRING_H YES)
ensure_file_include("memory.h"   HAVE_MEMORY_H YES)
ensure_file_include("stdlib.h"   HAVE_STDLIB_H YES)
ensure_file_include("stdio.h"    HAVE_STDIO_H  YES)
ensure_file_include("math.h"     HAVE_MATH_H   YES)
ensure_file_include("float.h"    HAVE_FLOAT_H  YES)
ensure_file_include("time.h"     HAVE_TIME_H   YES)
ensure_file_include("stdarg.h"   HAVE_STDARG_H YES)
ensure_file_include("ctype.h"    HAVE_CTYPE_H  YES)
ensure_file_include("assert.h"   HAVE_ASSERT_H YES)

# why check this one ? for openjpip ?
include (${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
CHECK_INCLUDE_FILE("strings.h"      HAVE_STRINGS_H)
CHECK_INCLUDE_FILE("sys/stat.h"     HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE("sys/types.h"    HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE("unistd.h"       HAVE_UNISTD_H)

# Enable Large file support
include(TestLargeFiles)
OPJ_TEST_LARGE_FILES(OPJ_HAVE_LARGEFILES)

# Allocating Aligned Memory Blocks
include(CheckIncludeFiles)
check_include_files(malloc.h OPJ_HAVE_MALLOC_H)
include(CheckSymbolExists)
# _aligned_alloc https://msdn.microsoft.com/en-us/library/8z34s9c6.aspx
check_symbol_exists(_aligned_malloc malloc.h OPJ_HAVE__ALIGNED_MALLOC)
# posix_memalign (needs _POSIX_C_SOURCE >= 200112L on Linux)
set(CMAKE_REQUIRED_DEFINITIONS -D_POSIX_C_SOURCE=200112L)
check_symbol_exists(posix_memalign stdlib.h OPJ_HAVE_POSIX_MEMALIGN)
unset(CMAKE_REQUIRED_DEFINITIONS)
# memalign (obsolete)
check_symbol_exists(memalign malloc.h OPJ_HAVE_MEMALIGN)
#-----------------------------------------------------------------------------
# Build Library
add_subdirectory(src/lib)
option(BUILD_LUTS_GENERATOR "Build utility to generate t1_luts.h" OFF)

#-----------------------------------------------------------------------------
# Build Applications
option(BUILD_CODEC "Build the CODEC executables" ON)
option(BUILD_PLUGIN_LOADER "Enable loading of T1 plugin" OFF)
mark_as_advanced(BUILD_PLUGIN_LOADER)

if(BUILD_CODEC)
  # OFF: It will only build 3rd party libs if they are not found on the system
  # ON: 3rd party libs will ALWAYS be build, and used
  option(BUILD_THIRDPARTY "Build the thirdparty executables if it is needed" OFF)
  add_subdirectory(thirdparty)
  add_subdirectory(src/bin)
endif ()

#-----------------------------------------------------------------------------
# opj_config.h generation (2/2)
configure_file(
 ${CMAKE_CURRENT_SOURCE_DIR}/src/lib/openjp2/opj_config.h.cmake.in
 ${CMAKE_CURRENT_BINARY_DIR}/src/lib/openjp2/opj_config.h
 @ONLY
 )

 configure_file(
 ${CMAKE_CURRENT_SOURCE_DIR}/src/lib/openjp2/opj_config_private.h.cmake.in
 ${CMAKE_CURRENT_BINARY_DIR}/src/lib/openjp2/opj_config_private.h
 @ONLY
 )

#-----------------------------------------------------------------------------
# build documentation in doc subdir:
if(BUILD_DOC)
  add_subdirectory(doc)
endif()

#-----------------------------------------------------------------------------
# Buld Testing
option(BUILD_TESTING "Build the tests." OFF)
if(BUILD_TESTING)
  if(BUILD_CODEC)
    enable_testing()
    include(CTest)

    # Search Grok data needed for the tests
    # They could be found via git on the Grok GitHub code project
    find_path(GROK_DATA_ROOT README-OPJ-DATA
      PATHS $ENV{GROK_DATA_ROOT} ${CMAKE_SOURCE_DIR}/../grok-test-data
      NO_DEFAULT_PATH NO_CMAKE_FIND_ROOT_PATH
      )

    # Add repository where to find tests
    add_subdirectory(tests)

  else()
    message(FATAL_ERROR "You need build codec to run the tests")
  endif()
endif()

#-----------------------------------------------------------------------------
# install all targets referenced as GROKTargets
install(EXPORT OpenJPEGTargets DESTINATION ${GROK_INSTALL_PACKAGE_DIR})
configure_file( ${${GROK_NAMESPACE}_SOURCE_DIR}/cmake/OpenJPEGConfig.cmake.in
  ${${GROK_NAMESPACE}_BINARY_DIR}/OpenJPEGConfig.cmake
  @ONLY
)
install( FILES ${GROK_BINARY_DIR}/OpenJPEGConfig.cmake
  DESTINATION ${GROK_INSTALL_PACKAGE_DIR}
)

#-----------------------------------------------------------------------------
# install CHANGES and LICENSE
if(BUILD_DOC)
if(EXISTS ${GROK_SOURCE_DIR}/CHANGES)
  install(FILES CHANGES DESTINATION ${GROK_INSTALL_DOC_DIR})
endif()

install(FILES LICENSE DESTINATION ${GROK_INSTALL_DOC_DIR})
endif()

include (cmake/OpenJPEGCPack.cmake)

#-----------------------------------------------------------------------------
# pkgconfig support
# enabled by default on Unix, disabled by default on other platforms
if(UNIX)
  option(BUILD_PKGCONFIG_FILES "Build and install pkg-config files" ON)
else()
  option(BUILD_PKGCONFIG_FILES "Build and install pkg-config files" OFF)
endif()
if(BUILD_PKGCONFIG_FILES)
  # install in lib and not share (see multi-arch note above)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/lib/openjp2/libopenjp2.pc.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/libopenjp2.pc @ONLY)
  install( FILES  ${CMAKE_CURRENT_BINARY_DIR}/libopenjp2.pc DESTINATION
    ${GROK_INSTALL_LIB_DIR}/pkgconfig )
#

endif()

#-----------------------------------------------------------------------------
