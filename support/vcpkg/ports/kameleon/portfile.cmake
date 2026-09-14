# Overlay port. Kameleon has no registry port, and the OpenSpace fork of the library
# (OpenSpace/Kameleon, previously vendored as the modules/kameleon/ext/kameleon git submodule)
# never calls install(EXPORT ...) and installs no CMake package config at all, so
# find_package(kameleon CONFIG REQUIRED) cannot succeed against it as-is.
# add-cmake-config.patch adds the top-level install(EXPORT ...)/KameleonConfig.cmake
# machinery, while ccmc-install.patch and cdf-install.patch add the matching
# install(TARGETS ...) calls for the ccmc and cdf targets, producing Kameleon::ccmc and
# Kameleon::cdf, matching the find_package(<pkg> CONFIG REQUIRED) + namespaced-target
# pattern used by every other dependency in this project's vcpkg migration.
# The pinned REF is the tip of upstream master: it includes a fix (upstreamed from this
# migration) adding missing standard-library includes throughout src/ccmc that newer MSVC
# STL versions no longer transitively provide.

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO OpenSpace/Kameleon
    REF d956fec7c67d061d62d932e848510c64716e0cab
    SHA512 ac99af7f7e1efc979ff9cab248176950b4526a88c430f0d5c72d6c2e7099191716528b8ea41f6091630b8b0c5068cfab4ec2552f9ad39214485bd21bd4ff58fa
    HEAD_REF master
    PATCHES
        add-cmake-config.patch
        ccmc-install.patch
        cdf-install.patch
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DKAMELEON_LIBRARY_ONLY=ON
        -DKAMELEON_USE_HDF5=OFF
        -DCDF_BUILD_ZLIB=ON
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH lib/cmake/Kameleon)

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/COPYING")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")
