##########################################################################################
# SGCT                                                                                   #
# Simple Graphics Cluster Toolkit                                                        #
#                                                                                        #
# Copyright (c) 2012-2026                                                                #
# For conditions of distribution and use, see copyright notice in LICENSE.md             #
##########################################################################################

vcpkg_from_github(
  OUT_SOURCE_PATH SOURCE_PATH
  REPO sgct/sgct
  REF bea94dde19a6d1497d531334f635ed3753f8ee1c
  SHA512 7655e4177011f330ca8c7479c140a85dcfc4e36cfb5346964e9c5051fa1e10b88a45b52b0672d13682ebe51abf40f6833c45beab39a5b6f6019223e9293e90b0
  HEAD_REF master
)

vcpkg_check_features(
  OUT_FEATURE_OPTIONS FEATURE_OPTIONS
  FEATURES
    ndi      SGCT_NDI_SUPPORT
    scalable SGCT_SCALABLE_SUPPORT
)

vcpkg_cmake_configure(
  SOURCE_PATH "${SOURCE_PATH}"
  OPTIONS
    ${FEATURE_OPTIONS}
    -DSGCT_BUILD_TESTS=OFF
    -DSGCT_BUILD_CALIBRATOR=OFF
    -DSGCT_ENABLE_EDIT_CONTINUE=OFF
)

vcpkg_cmake_install()
vcpkg_cmake_config_fixup(CONFIG_PATH share/sgct)
vcpkg_copy_pdbs()

# SGCT's own build does not install the configuration schema, but consumers ship it next
# to the application so that editors can validate the cluster configuration files. Install
# it alongside the CMake config, where ${sgct_DIR} points, so a consumer can copy it out.
file(
  INSTALL "${SOURCE_PATH}/sgct.schema.json"
  DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
)

file(
  REMOVE_RECURSE
    "${CURRENT_PACKAGES_DIR}/debug/include"
    "${CURRENT_PACKAGES_DIR}/debug/share"
)

file(
  INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage"
  DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
)
vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE.md")
