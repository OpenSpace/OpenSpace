# Overlay port. Spice (JPL's NAIF SPICE toolkit) has no vcpkg registry port. The
# OpenSpace/Spice repository (previously vendored as the ext/spice git submodule)
# combines the per-platform source releases (Visual Studio, Linux, macOS, Cygwin)
# into a single source tree of common and platform-specific files. This port
# downloads the source from GitHub and builds it as a static spice::spice library.

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO OpenSpace/Spice
    REF 00fb7876faff7cac8cb12fa87ba5ddcad1485fbe
    SHA512 76f3d354e5589ceb901ab87241f33101ae80fc69447f21e1ff9b49400b89966ca2177f60c1455e2c6d6d19db3d987602301d3088e9d9828ee944f427203748ca
    HEAD_REF vcpkg
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DSPICE_BUILD_SHARED_LIBRARY=OFF
        -DSPICE_ENABLE_INSTALL=ON
)

vcpkg_cmake_install()
vcpkg_cmake_config_fixup(CONFIG_PATH share/spice)
vcpkg_copy_pdbs()

file(
    REMOVE_RECURSE
        "${CURRENT_PACKAGES_DIR}/debug/include"
        "${CURRENT_PACKAGES_DIR}/debug/share"
)

file(
    INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage"
    DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
)

# Spice ships no LICENSE file; disclaimer.txt carries the disclaimer verbatim from the top
# of every source file in the toolkit.
vcpkg_install_copyright(FILE_LIST "${CMAKE_CURRENT_LIST_DIR}/disclaimer.txt")