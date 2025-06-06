find_package(PkgConfig)
pkg_check_modules(PC_LIBSOUP REQUIRED QUIET libsoup-3.0)

find_path(LIBSOUP_INCLUDE_DIRS
    NAMES libsoup/soup.h
    HINTS ${PC_LIBSOUP_INCLUDEDIR}
          ${PC_LIBSOUP_INCLUDE_DIRS}
    PATH_SUFFIXES libsoup-3.0
)

find_library(LIBSOUP_LIBRARIES
    NAMES soup-3.0
    HINTS ${PC_LIBSOUP_LIBDIR}
          ${PC_LIBSOUP_LIBRARY_DIRS}
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(libsoup-3.0 REQUIRED_VARS LIBSOUP_INCLUDE_DIRS LIBSOUP_LIBRARIES VERSION_VAR PC_LIBSOUP_VERSION)


# Provide alias for libsoup-3.0
if (DEFINED LIBSOUP_LIBRARIES)
    message(STATUS "<*> LIBSOUP_LIBRARIES = ${LIBSOUP_LIBRARIES}")
    message(STATUS "<*> LIBSOUP_INCLUDE_DIRS = ${LIBSOUP_INCLUDE_DIRS}")
else()
    message(FATAL_ERROR "libsoup-3.0 library not found, but it is required by this project!")
endif()
