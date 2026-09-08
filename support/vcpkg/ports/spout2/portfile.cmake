# Overlay port. Identical to the registry spout2 port at baseline
# 04a9d8e5212d01ee1dd9478eadd9caade4f8b0d4, except that the Spout DLLs are kept on static
# library triplets instead of being deleted. See vcpkg.json for the rationale.

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO leadedge/Spout2
    REF 62362774c96547d63b502d7efd5cfbf138eb7570 #v2.007.010
    SHA512 89d0dcec719c068e27c2f55605e4b45b32fe3a5e097c821b0aa45f4ee9284e63830bd741ac7bb1bff917190d9a51daa36b452580fc673c05767b7bfcbc9a494f
    HEAD_REF master
    PATCHES
        fix-include-path.patch
        fix-dx-keyed.patch
)

if(VCPKG_CRT_LINKAGE STREQUAL "static")
    list(APPEND OPTIONS -DSPOUT_BUILD_CMT=ON)
else()
    list(APPEND OPTIONS -DSPOUT_BUILD_CMT=OFF)
endif()

vcpkg_check_features(
    OUT_FEATURE_OPTIONS FEATURE_OPTIONS
    FEATURES
        dx              SPOUT_BUILD_SPOUTDX
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DSKIP_INSTALL_ALL=OFF
        ${FEATURE_OPTIONS}
        ${OPTIONS}
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH lib/cmake/${PORT})

# Handle copyright & usage
vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

# remove unneeded files
file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")

# Spout only ever builds Spout / SpoutLibrary as SHARED libraries (Spout_static is an
# additional static archive enabled through SPOUT_BUILD_CMT). The exported CMake package
# always defines the SHARED imported targets Spout2::Spout and Spout2::SpoutLibrary, so the
# DLLs they point at must stay in place even on a static-library triplet; deleting them
# (as the registry port does) makes find_package(Spout2 CONFIG REQUIRED) fail for any
# consumer, and SGCT links Spout2::SpoutLibrary unconditionally on Windows.
if(VCPKG_LIBRARY_LINKAGE STREQUAL "static")
    set(VCPKG_POLICY_DLLS_IN_STATIC_LIBRARY enabled)
endif()
