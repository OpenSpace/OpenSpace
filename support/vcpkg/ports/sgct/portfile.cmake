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
  REF ad83444a779d0eac0b404dca8d383958c4c60d93
  SHA512 4c4c59a49ec3978e9171f360ef046af30ca075fc169bda7be9a7d230a42021edcb72e1a82cf9e2236649bccf75b37c7c50df5b52068934b00d82af00c7828432
  HEAD_REF master
)

vcpkg_check_features(
  OUT_FEATURE_OPTIONS FEATURE_OPTIONS
  FEATURES
    calibrator SGCT_BUILD_CALIBRATOR
    ndi        SGCT_NDI_SUPPORT
    scalable   SGCT_SCALABLE_SUPPORT
)

# Spout's SDK is x86-only, which is why the manifest gates spout2 on 'windows & !arm64'.
# SGCT defaults SGCT_SPOUT_SUPPORT the same way from the target architecture, but it is
# passed explicitly so the port never depends on that detection succeeding
if (VCPKG_TARGET_ARCHITECTURE STREQUAL "arm64")
  set(SPOUT_OPTION -DSGCT_SPOUT_SUPPORT=OFF)
else ()
  set(SPOUT_OPTION -DSGCT_SPOUT_SUPPORT=ON)
endif ()

vcpkg_cmake_configure(
  SOURCE_PATH "${SOURCE_PATH}"
  OPTIONS
    ${FEATURE_OPTIONS}
    ${SPOUT_OPTION}
    -DSGCT_BUILD_TESTS=OFF
    -DSGCT_ENABLE_EDIT_CONTINUE=OFF
  # Only the release calibrator is shipped, so building it a second time is wasted work.
  # vcpkg_cmake_configure passes OPTIONS before OPTIONS_DEBUG and CMake keeps the last -D
  # it sees for a cache variable, so this overrides what the feature check emitted above
  OPTIONS_DEBUG
    -DSGCT_BUILD_CALIBRATOR=OFF
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

if ("calibrator" IN_LIST FEATURES)
  # SGCT installs the calibrator into bin/, together with the test patterns it resolves
  # against its working directory. Everything moves into the tools folder from there: that
  # is where a consumer looks for an executable, and a static triplet is not allowed to
  # leave anything behind in bin/. The patterns have to go first, because AUTO_CLEAN only
  # removes bin/ once nothing but the tool itself is left in it
  file(GLOB calibrator_patterns "${CURRENT_PACKAGES_DIR}/bin/test-pattern-*.png")
  file(COPY ${calibrator_patterns} DESTINATION "${CURRENT_PACKAGES_DIR}/tools/${PORT}")
  file(REMOVE ${calibrator_patterns})

  vcpkg_copy_tools(TOOL_NAMES calibrator AUTO_CLEAN)
endif ()

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
