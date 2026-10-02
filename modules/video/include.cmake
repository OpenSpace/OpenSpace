set(OPENSPACE_DEPENDENCIES
  base
  globebrowsing
)

# On Windows the module links libmpv from the prebuilt archive. That archive ships an
# x64 dll only, so an ARM64 build.
if (WIN32 AND CMAKE_CXX_COMPILER_ARCHITECTURE_ID STREQUAL "ARM64")
  set(DEFAULT_MODULE OFF)
  set(SUPPORTED OFF)
endif ()
