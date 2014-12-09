

set(SPICE_INCLUDE_DIR "${SPICE_ROOT_DIR}/include")

if(WIN32)
    set(SPICE_LIBRARY "${SPICE_ROOT_DIR}/lib/msvc12/cspice.lib")
elseif(APPLE)
    set(SPICE_LIBRARY "${SPICE_ROOT_DIR}/lib/gcc_osx/cspice.a")
else()
    set(SPICE_LIBRARY "${SPICE_ROOT_DIR}/lib/gcc_linux/cspice.a")
endif()

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(SPICE DEFAULT_MSG SPICE_LIBRARY SPICE_INCLUDE_DIR)

if(SPICE_FOUND)
    set(SPICE_LIBRARIES ${SPICE_LIBRARY})
    set(SPICE_INCLUDE_DIRS ${SPICE_INCLUDE_DIR})
endif()

mark_as_advanced(SPICE_INCLUDE_DIR SPICE_LIBRARY)

