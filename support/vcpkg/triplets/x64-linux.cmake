# Overlay triplet that shadows the built-in x64-linux triplet.
#
# OpenSpace builds all of its dependencies statically on Linux. Qt is the exception:
# OpenSpace ships under Qt's LGPL v3 license, which is only practical to comply with when
# Qt is linked dynamically (the user must be able to substitute a modified Qt build). So the
# qt* ports are forced to dynamic linkage here and ship as .so files next to the
# executables, while everything else stays static. See the equivalent
# x64-windows-static-md.cmake overlay triplet for the Windows side of this policy.

set(VCPKG_TARGET_ARCHITECTURE x64)
set(VCPKG_CRT_LINKAGE dynamic)
set(VCPKG_LIBRARY_LINKAGE static)

set(VCPKG_CMAKE_SYSTEM_NAME Linux)

if (PORT MATCHES "^qt")
  set(VCPKG_LIBRARY_LINKAGE dynamic)
endif ()
