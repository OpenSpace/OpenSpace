# stbimage has no upstream registry port; This is a small library that compiles the
# stb_image/stb_image_write implementation so it can be shared without
# duplicate-symbol/ODR conflicts (see https://github.com/sgct/stbimage for details).

vcpkg_from_github(
  OUT_SOURCE_PATH SOURCE_PATH
  REPO sgct/stbimage
  REF c2ff2f66602a2b54478163d785a38cfae703c94c
  SHA512 2a4ca94421e79ad6f2a029f11199ed4efcf1bfe71052d28326b459a5e29da967ed145441a6285e27a9321ef2d3c2498a621304bc3c0cbe119902dadbfa71a2e1
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
