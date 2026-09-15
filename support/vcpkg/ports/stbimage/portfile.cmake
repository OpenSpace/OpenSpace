# stbimage has no upstream registry port; This is a small library that compiles the
# stb_image/stb_image_write implementation exactly once so it can be shared without
# duplicate-symbol/ODR conflicts (see https://github.com/sgct/stbimage for details).

vcpkg_from_github(
  OUT_SOURCE_PATH SOURCE_PATH
  REPO sgct/stbimage
  REF a531835e711aa97a095b73290fb6cec0173542a3
  SHA512 d0b1ab0795ed9cc8bde6282a2e58d46603f8a7cde8080545734dc7eec2d58c57257527641ad94a887e4fb04256589c6501646f0afe7102625497755e3749ee7a
  HEAD_REF master
)

vcpkg_check_linkage(ONLY_STATIC_LIBRARY)

vcpkg_cmake_configure(SOURCE_PATH "${SOURCE_PATH}")

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH share/stbimage)

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")

file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")
vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
