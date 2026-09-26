vcpkg_from_git(
    OUT_SOURCE_PATH SOURCE_PATH
    URL https://github.com/cginternals/glbinding.git
    REF c8a66c92e8a81d82e384a4c80207913519c1f17e # v3.5.0
)

# Upstream's non-system-install path hardcodes INSTALL_BIN to "." instead of "bin", so the
# DLLs built for a dynamic triplet land next to the package root instead of <prefix>/bin
# (and debug/bin), where vcpkg and consumers expect runtime binaries to be.
vcpkg_replace_string(
    "${SOURCE_PATH}/CMakeLists.txt"
    "set\\(INSTALL_BIN[ \t]+\"\\.\"\\)"
    "set(INSTALL_BIN \"bin\")"
    REGEX
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DOPTION_BUILD_TESTS=OFF
        -DOPTION_BUILD_TOOLS=OFF
        -DOPTION_BUILD_EXAMPLES=OFF
        -DOPTION_BUILD_DOCS=OFF
        -DOPTION_BUILD_CHECK=OFF
        -DGIT_REV=0
        -DCMAKE_DISABLE_FIND_PACKAGE_cpplocate=ON
    MAYBE_UNUSED_VARIABLES
        CMAKE_DISABLE_FIND_PACKAGE_cpplocate
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()

# glbinding installs into a self-contained layout (`<prefix>/glbinding-config.cmake` plus
# `<prefix>/cmake/<module>/`) that vcpkg_cmake_config_fixup cannot process. The tree below
# is relocated into `share/glbinding` by hand, keeping the config file and the `cmake`
# directory together because the config resolves the modules relative to itself.
set(GLBINDING_SHARE "${CURRENT_PACKAGES_DIR}/share/${PORT}")
file(MAKE_DIRECTORY "${GLBINDING_SHARE}")
file(RENAME "${CURRENT_PACKAGES_DIR}/glbinding-config.cmake" "${GLBINDING_SHARE}/glbinding-config.cmake")
file(RENAME "${CURRENT_PACKAGES_DIR}/cmake" "${GLBINDING_SHARE}/cmake")

# Fold the debug import fragments in next to the release ones, where the module export file
# picks them up through its glob, and point them at the debug library directory
file(GLOB GLBINDING_DEBUG_FRAGMENTS "${CURRENT_PACKAGES_DIR}/debug/cmake/*/*-export-debug.cmake")
foreach(FRAGMENT IN LISTS GLBINDING_DEBUG_FRAGMENTS)
    get_filename_component(MODULE_DIR "${FRAGMENT}" DIRECTORY)
    get_filename_component(MODULE_NAME "${MODULE_DIR}" NAME)
    get_filename_component(FRAGMENT_NAME "${FRAGMENT}" NAME)

    set(RELOCATED "${GLBINDING_SHARE}/cmake/${MODULE_NAME}/${FRAGMENT_NAME}")
    file(RENAME "${FRAGMENT}" "${RELOCATED}")
    vcpkg_replace_string("${RELOCATED}" [[${_IMPORT_PREFIX}/lib/]] [[${_IMPORT_PREFIX}/debug/lib/]])
    # For a dynamic triplet the fragment also carries an IMPORTED_LOCATION_DEBUG pointing at
    # the DLL under bin/, which vcpkg installs to debug/bin/ alongside the debug import lib.
    # Static triplets have no such reference, hence IGNORE_UNCHANGED.
    vcpkg_replace_string("${RELOCATED}" [[${_IMPORT_PREFIX}/bin/]] [[${_IMPORT_PREFIX}/debug/bin/]] IGNORE_UNCHANGED)
endforeach()

# Moving the export files two directories deeper invalidates the prefix they compute from
# their own location
file(GLOB GLBINDING_EXPORTS "${GLBINDING_SHARE}/cmake/*/*-export.cmake")
foreach(EXPORT_FILE IN LISTS GLBINDING_EXPORTS)
    vcpkg_replace_string("${EXPORT_FILE}"
[[get_filename_component(_IMPORT_PREFIX "${CMAKE_CURRENT_LIST_FILE}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)]]
[[get_filename_component(_IMPORT_PREFIX "${CMAKE_CURRENT_LIST_FILE}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)]]
    )
endforeach()

vcpkg_install_copyright(FILE_LIST "${CURRENT_PACKAGES_DIR}/LICENSE")

file(REMOVE_RECURSE
    "${CURRENT_PACKAGES_DIR}/debug/cmake"
    "${CURRENT_PACKAGES_DIR}/debug/include"
    "${CURRENT_PACKAGES_DIR}/debug/share"
    "${CURRENT_PACKAGES_DIR}/include/KHR" # already published by egl-registry
)

foreach(STRAY_FILE AUTHORS LICENSE README.md VERSION glbinding-config.cmake)
    file(REMOVE
        "${CURRENT_PACKAGES_DIR}/${STRAY_FILE}"
        "${CURRENT_PACKAGES_DIR}/debug/${STRAY_FILE}"
    )
endforeach()
