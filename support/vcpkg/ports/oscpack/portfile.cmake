# Overlay port. The registry oscpack port at baseline 04a9d8e5212d01ee1dd9478eadd9caade4f8b0d4 builds
# upstream oscpack via a plain install(TARGETS) with no install(EXPORT) and no generated CMake package
# config, so find_package(oscpack CONFIG REQUIRED) cannot succeed against it. add-cmake-config.patch
# adds an oscpack::oscpack exported target plus a generated oscpackConfig.cmake, matching the
# find_package(<pkg> CONFIG REQUIRED) + namespaced-target pattern used by every other dependency in
# this project's vcpkg migration.

if(VCPKG_TARGET_IS_WINDOWS)
    # oscpack's Windows UDP socket implementation is not annotated for DLL export/import.
    vcpkg_check_linkage(ONLY_STATIC_LIBRARY)
endif()

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO RossBencina/oscpack
    REF release_1_1_0
    SHA512 7a61a364cab4914c81e113d7aeee2b4accf5e560f500df6634232e0093f564ed4bb0ef8e87d2c8a18f245b0c7ec25f41e64f42f20a6654c22bb5c02aa253bbd0
    PATCHES
        add-cmake-config.patch
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        # oscpack's CMakeLists.txt still says `cmake_minimum_required(VERSION 2.6)`;
        # CMake 4 removed support for that compatibility level outright.
        -DCMAKE_POLICY_VERSION_MINIMUM=3.5
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH lib/cmake/oscpack)

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")
