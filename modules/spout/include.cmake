# Spout is not supported on non-Windows machines
if (NOT WIN32)
  set(DEFAULT_MODULE OFF)
  set(SUPPORTED OFF)
endif ()

# Spout's SDK is x86-only
if (CMAKE_CXX_COMPILER_ARCHITECTURE_ID STREQUAL "ARM64")
  set(DEFAULT_MODULE OFF)
  set(SUPPORTED OFF)
endif ()
