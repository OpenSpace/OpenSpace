# Spout is not supported on non-Windows machines
if (NOT WIN32)
  set(DEFAULT_MODULE OFF)
  set(SUPPORTED OFF)
endif ()
