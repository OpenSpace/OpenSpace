# Overlay port. The registry ccfits port at baseline 04a9d8e5212d01ee1dd9478eadd9caade4f8b0d4 builds
# upstream CCfits as-is, which never calls install(EXPORT ...) and installs no CMake package config at
# all -- find_package(ccfits CONFIG REQUIRED) cannot succeed against it. add-cmake-config.patch adds a
# CCfits::CCfits exported target (linked PUBLIC against CFITSIO::cfitsio, since CCfits is a static
# library and consumers need cfitsio's symbols at final link time too) plus a generated
# ccfitsConfig.cmake, matching the find_package(<pkg> CONFIG REQUIRED) + namespaced-target pattern used
# by every other dependency in this project's vcpkg migration. The file is named lowercase because
# find_package(ccfits ...) only searches for ccfitsConfig.cmake/ccfits-config.cmake, and Linux
# filesystems (unlike Windows/macOS) are case-sensitive.

vcpkg_download_distfile(ARCHIVE
    URLS "https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/ccfits/v2.7/CCfits-2.7.tar.gz"
    FILENAME "CCfits-2.7.tar.gz"
    SHA512 5cb802f41cf0695d0e49924ee163151ee657b93158246766d04c192518c7bed30383405d87b5fb312f5f44af26d5ede3104fab90d93cc232e950f8ae66050fde
)

vcpkg_extract_source_archive(
    SOURCE_PATH
    ARCHIVE "${ARCHIVE}"
    PATCHES
        dependencies.diff
        dll_exports.patch
        add-cmake-config.patch
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
)
vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH lib/cmake/CCfits)

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/License.txt")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")
