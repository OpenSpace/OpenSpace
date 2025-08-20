message(STATUS "Running prune.cmake: removing developer files from staged install...")

# Convenience var
set(_root "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}")

# --------------------------------------------------------------------
# Remove whole unwanted directories
# --------------------------------------------------------------------
file(REMOVE_RECURSE
  "${_root}/include"
  "${_root}/share/glbinding"
  "${_root}/share/Tracy"
  "${_root}/share/SoLoud"
  "${_root}/share/pkgconfig"
  "${_root}/lib/pkgconfig"
)

# --------------------------------------------------------------------
# Static libraries
# --------------------------------------------------------------------
file(GLOB_RECURSE _static_libs "${_root}/*.a")
if(_static_libs)
  file(REMOVE ${_static_libs})
endif()

# --------------------------------------------------------------------
# Git leftovers
# --------------------------------------------------------------------
file(GLOB_RECURSE _gitfiles "${_root}/*.git")
file(GLOB_RECURSE _gitignores "${_root}/.gitignore")
if(_gitfiles)
  file(REMOVE ${_gitfiles})
endif()
if(_gitignores)
  file(REMOVE ${_gitignores})
endif()

# --------------------------------------------------------------------
# CMake fragments
# --------------------------------------------------------------------
file(GLOB_RECURSE _cmakefiles "${_root}/*.cmake")
if(_cmakefiles)
  file(REMOVE ${_cmakefiles})
endif()

# Remove CMakeLists.txt specifically
file(GLOB_RECURSE _cmakelists "${_root}/CMakeLists.txt")
if(_cmakelists)
  file(REMOVE ${_cmakelists})
endif()

# --------------------------------------------------------------------
# zlib man page (example of pruning a single file family)
# --------------------------------------------------------------------
file(GLOB _zlib_man3 "${_root}/share/man/man3/zlib*")
if(_zlib_man3)
  file(REMOVE ${_zlib_man3})
endif()

# --------------------------------------------------------------------
# pkgconfig files and dirs
# --------------------------------------------------------------------
file(GLOB_RECURSE _pkgconfigs
  "${_root}/*.pc"
  "${_root}/*/pkgconfig"
)
if(_pkgconfigs)
  file(REMOVE_RECURSE ${_pkgconfigs})
endif()

# --------------------------------------------------------------------
# cmake dirs
# --------------------------------------------------------------------
file(GLOB_RECURSE _cmakedirs
  "${_root}/*/cmake"
)
if(_cmakedirs)
  file(REMOVE_RECURSE ${_cmakedirs})
endif()

# --------------------------------------------------------------------
# Source files (.c, .cpp, .h, .hpp)
# --------------------------------------------------------------------
file(GLOB_RECURSE _src_c "${_root}/*.c")
if(_src_c)
  file(REMOVE ${_src_c})
endif()

file(GLOB_RECURSE _src_cpp "${_root}/*.cpp")
if(_src_cpp)
  file(REMOVE ${_src_cpp})
endif()

file(GLOB_RECURSE _src_h "${_root}/*.h")
if(_src_h)
  file(REMOVE ${_src_h})
endif()

file(GLOB_RECURSE _src_hpp "${_root}/*.hpp")
if(_src_hpp)
  file(REMOVE ${_src_hpp})
endif()

message(STATUS "Prune step finished.")
