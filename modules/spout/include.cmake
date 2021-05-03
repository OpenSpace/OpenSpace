if (WIN32)
  set(DEFAULT_MODULE ON)
endif ()

# Spout is not supported on non-Windows machines
if (NOT WIN32)
  set(SUPPORTED OFF)
endif ()
