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
  REF 12d9c38941f69191eead3782747be936c8c2e6c3
  SHA512 801d4644db419118cd71788cd278e350e4ecd43322ab17e2860feb6692380b4e046f4ccde14450bcf1914fcd596cfa84bcbb63ae86db27885263b09fed43282d
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
