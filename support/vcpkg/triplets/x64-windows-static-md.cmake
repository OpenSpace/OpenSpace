# Overlay triplet that shadows the built-in x64-windows-static-md.
#
# OpenSpace builds all of its dependencies statically against the dynamic CRT (Ghoul's
# glbinding overlay port is static-only, which rules out the fully dynamic x64-windows).
# Qt is the exception: OpenSpace ships under Qt's LGPL v3 license, which is only practical
# to comply with when Qt is linked dynamically (the user must be able to substitute a
# modified Qt build). So the qt* ports are forced to dynamic linkage here and ship as DLLs
# next to the executables, while everything else stays static.

set(VCPKG_TARGET_ARCHITECTURE x64)
set(VCPKG_CRT_LINKAGE dynamic)
set(VCPKG_LIBRARY_LINKAGE static)
set(VCPKG_PROVIDED_FORTRAN ON)

if (PORT MATCHES "^qt")
  set(VCPKG_LIBRARY_LINKAGE dynamic)
endif ()
