# Overlay port. SoLoud has no registry port and its own upstream CMake build (contrib/) is not
# meant to be consumed via find_package: its exported config lands in a platform-specific
# location, installs headers under include/soloud instead of flat include/, and never records
# an INTERFACE_INCLUDE_DIRECTORIES on the exported target (upstream exposes headers only via a
# directory-scoped include_directories(../include)), none of which match OpenSpace's usage
# (`#include <soloud.h>`). fix-install-paths.patch corrects all of this, and adds a `soloud::`
# namespace to the exported target to match the rest of this project's vcpkg ports.
# use-system-alsa.patch makes the Linux ALSA backend go through find_package(ALSA)/ALSA::ALSA
# instead of upstream's bare find_library(asound), so the port picks up the alsa dependency
# declared below instead of silently depending on whatever asound happens to be on the system.

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO jarikomppa/soloud
    REF e82fd32c1f62183922f08c14c814a02b58db1873
    SHA512 e3bf97914e8a94741366c944ca574f20d7111f9fead939d56df52ebb50440401648a536585d90afce0208715e78e741f20c6b56d43a60f1fa9ea7c4f45635af9
    HEAD_REF master
    PATCHES
        fix-install-paths.patch
        use-system-alsa.patch
)

if(VCPKG_LIBRARY_LINKAGE STREQUAL "dynamic")
    list(APPEND OPTIONS -DSOLOUD_DYNAMIC=ON -DSOLOUD_STATIC=OFF)
else()
    list(APPEND OPTIONS -DSOLOUD_DYNAMIC=OFF -DSOLOUD_STATIC=ON)
endif()

# Mirrors the backend selection previously hardcoded in modules/audio/CMakeLists.txt: SDL2 off,
# WinMM on Windows, ALSA on Linux. The NULL backend stays on the upstream default (ON).
list(APPEND OPTIONS -DSOLOUD_BACKEND_SDL2=OFF)
if(VCPKG_TARGET_IS_WINDOWS)
    list(APPEND OPTIONS -DSOLOUD_BACKEND_WINMM=ON)
elseif(VCPKG_TARGET_IS_LINUX)
    list(APPEND OPTIONS -DSOLOUD_BACKEND_ALSA=ON)
endif()

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}/contrib"
    OPTIONS
        # SoLoud's contrib/CMakeLists.txt still says `cmake_minimum_required(VERSION 2.8)`;
        # CMake 4 removed support for that compatibility level outright.
        -DCMAKE_POLICY_VERSION_MINIMUM=3.5
        -DSOLOUD_C_API=OFF
        -DSOLOUD_BUILD_DEMOS=OFF
        -DSOLOUD_GENERATE_GLUE=OFF
        ${OPTIONS}
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH share/soloud/cmake)

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")
