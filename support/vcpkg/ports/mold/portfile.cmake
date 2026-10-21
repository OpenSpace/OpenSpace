# Overlay port. mdlib ("mold") has no registry port, and upstream (scanberg/mdlib)
# ships no install rules at all, so find_package(mold CONFIG REQUIRED) cannot succeed
# against it as-is. add-install-rules.patch adds the install(TARGETS ...)/install(EXPORT ...)
# machinery and a generated moldConfig.cmake, producing a mold::mdlib target and matching the
# find_package(<pkg> CONFIG REQUIRED) + namespaced-target pattern used by every other
# dependency in OpenSpace.
#
# Upstream mdlib now natively supports ARM64 (NEON via SIMDe abstraction) in addition to
# x86-64. Shaders are precompiled into C byte arrays (gl_shaders.inl) and linked directly
# into mdlib, eliminating the runtime shader path dependency.

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO scanberg/mdlib
    REF 00bb2e118f8113a0b223c0a588fa29a7dae4c305
    SHA512 13edaf0b94d2fc418544bc4cc338d925cb8dc6dda01ef448e5c329cde80fac050914df12711a3fb92f847e1d0b08a05678d6e1d80e86addd9682470ce16cf553
    HEAD_REF master
    PATCHES
        add-install-rules.patch
)

file(COPY "${CMAKE_CURRENT_LIST_DIR}/moldConfig.cmake.in" DESTINATION "${SOURCE_PATH}")

# MD_LINK_STDLIB_STATIC defaults to ON, which forces mdlib's MSVC_RUNTIME_LIBRARY to the
# static CRT and adds -static-libgcc/-static-libstdc++ on GCC, regardless of what the
# triplet asked for. Drive it from the triplet's CRT linkage instead.
if (VCPKG_CRT_LINKAGE STREQUAL "static")
    set(LINK_STDLIB_STATIC ON)
else ()
    set(LINK_STDLIB_STATIC OFF)
endif ()

# On x86/x64, mdlib uses AVX2 intrinsics. Match the AVX2 baseline for x86 architectures.
# On ARM64 (including ARM64 Windows), mdlib uses SIMDe/NEON and does not use /arch:AVX2.
if (VCPKG_TARGET_ARCHITECTURE STREQUAL "x64" OR VCPKG_TARGET_ARCHITECTURE STREQUAL "x86")
    if (VCPKG_TARGET_IS_WINDOWS)
        set(VCPKG_C_FLAGS "${VCPKG_C_FLAGS} /arch:AVX2")
        set(VCPKG_CXX_FLAGS "${VCPKG_CXX_FLAGS} /arch:AVX2")
    else ()
        set(VCPKG_C_FLAGS "${VCPKG_C_FLAGS} -march=x86-64-v3")
        set(VCPKG_CXX_FLAGS "${VCPKG_CXX_FLAGS} -march=x86-64-v3")
    endif ()
endif ()

set(VCPKG_BUILD_TYPE release)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DMD_UNITTEST=OFF
        -DMD_BENCHMARK=OFF
        -DMD_ENABLE_GPU=OFF
        -DMD_ENABLE_HDF5=OFF
        -DMD_LINK_STDLIB_STATIC=${LINK_STDLIB_STATIC}
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH share/mold)

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

file(REMOVE_RECURSE
    "${CURRENT_PACKAGES_DIR}/debug/include"
    "${CURRENT_PACKAGES_DIR}/debug/share"
)
