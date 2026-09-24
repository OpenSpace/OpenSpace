# Overlay port. TUIO has no registry port, and the OpenSpace fork of the TUIO C++ client
# library (OpenSpace/TUIO11_CPP, previously vendored as the modules/touch/ext/libTUIO11 git
# submodule) ships no CMake build at all -- only a Makefile and Windows/macOS IDE project
# files. CMakeLists.txt and tuioConfig.cmake.in in this overlay port are OpenSpace-authored;
# they build the TUIO client sources together with the bundled oscpack OSC implementation
# into a single tuio::tuio target, matching the find_package(<pkg> CONFIG REQUIRED) +
# namespaced-target pattern used by every other dependency in this project's vcpkg migration.
# The library is always built static: LibExport.h's LIBDECL only expands to
# __declspec(dllexport) when LIB_EXPORT is defined, which nothing here ever does, so a shared
# build would export no symbols on Windows.

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO OpenSpace/TUIO11_CPP
    REF 9c543bc229cc99dcf9628f6e401287af54bc533d
    SHA512 f29b68fabe4cf323d876a701951d231b85ad6f77dff5d4529765b5ab7ed628c15c6f5d126f0104fa5a7407b9fa08a303f87a77576b85adfd4289930d396f451e
    HEAD_REF master
)

file(COPY "${CMAKE_CURRENT_LIST_DIR}/CMakeLists.txt" DESTINATION "${SOURCE_PATH}")
file(COPY "${CMAKE_CURRENT_LIST_DIR}/tuioConfig.cmake.in" DESTINATION "${SOURCE_PATH}")

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH share/tuio)

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE.txt")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include")
