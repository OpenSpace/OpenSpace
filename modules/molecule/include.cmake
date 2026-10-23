# mold (scanberg/mdlib) is x86-only
if (CMAKE_CXX_COMPILER_ARCHITECTURE_ID STREQUAL "ARM64")
  set(SUPPORTED OFF)
endif ()
