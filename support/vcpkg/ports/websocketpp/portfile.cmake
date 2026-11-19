vcpkg_from_git(
    OUT_SOURCE_PATH SOURCE_PATH
    URL https://github.com/OpenSpace/websocketpp.git
    REF 29ec2d4bc42c02d8a852972692f60d931b00cf7b
)

# WebSocket++ is header only, so the upstream build system is bypassed entirely
file(COPY "${SOURCE_PATH}/websocketpp" DESTINATION "${CURRENT_PACKAGES_DIR}/include")
file(REMOVE "${CURRENT_PACKAGES_DIR}/include/websocketpp/CMakeLists.txt")

file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")
vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/COPYING")
