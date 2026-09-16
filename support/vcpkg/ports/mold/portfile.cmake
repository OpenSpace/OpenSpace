# Overlay port. mdlib ("mold") has no registry port, and upstream (scanberg/mdlib,
# previously vendored as the modules/molecule/ext/mold git submodule) ships no install
# rules at all, so find_package(mold CONFIG REQUIRED) cannot succeed against it as-is.
# add-install-rules.patch adds the install(TARGETS ...)/install(EXPORT ...) machinery and a
# generated moldConfig.cmake, producing a mold::mdlib target and matching the
# find_package(<pkg> CONFIG REQUIRED) + namespaced-target pattern used by every other
# dependency in this project's vcpkg migration.
#
# The other three patches are the OpenSpace-local changes that used to live as
# pathpatch_[123]_*.patch next to modules/molecule/CMakeLists.txt and were applied to the
# submodule working tree by a git-apply step during configure:
#   md-path-exe-dir-decl.patch / md-path-exe-dir-impl.patch  add md_path_exe_dir()
#   runtime-shader-path.patch                                makes md_gl.c resolve its GLSL
#                                                            relative to the executable
# Upstream bakes the absolute build-time shader directory into the library through the
# MD_SHADER_DIR define, which is useless once the library is a redistributable package.
#
# REF is pinned to the commit the submodule pointed at; the three OpenSpace patches were
# written against that revision.

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO scanberg/mdlib
    REF 70a27a285029f1606cd540a99edd8ec14af23a38
    SHA512 2febcff5de5d358b28e6031d903e932ba3de25832a3bff06d864966b12d7814677d09a630d77ea65bd5dedd95c5deb98717892a3b2d2c0267b6d2fa626c26897
    HEAD_REF master
    PATCHES
        md-path-exe-dir-decl.patch
        md-path-exe-dir-impl.patch
        runtime-shader-path.patch
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

# mdlib uses AVX2 intrinsics unconditionally. Without an explicit arch flag, GCC/Clang
# reject them at compile time, which is what breaks the build inside a Docker container.
# Match the blanket AVX2/x86-64-v3 baseline the rest of the project compiles with (see
# common-compile-settings).
if (VCPKG_TARGET_IS_WINDOWS)
    set(VCPKG_C_FLAGS "${VCPKG_C_FLAGS} /arch:AVX2")
    set(VCPKG_CXX_FLAGS "${VCPKG_CXX_FLAGS} /arch:AVX2")
else ()
    set(VCPKG_C_FLAGS "${VCPKG_C_FLAGS} -march=x86-64-v3")
    set(VCPKG_CXX_FLAGS "${VCPKG_CXX_FLAGS} -march=x86-64-v3")
endif ()

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS
        -DMD_UNITTEST=OFF
        -DMD_BENCHMARK=OFF
        # Upstream's "relative resource path" mode copies the GLSL next to the build
        # output; runtime-shader-path.patch removes every use of the resulting
        # MD_SHADER_DIR, and the shaders are installed into share/mold/shaders instead.
        -DMD_USE_RELATIVE_RESOURCE_PATH=OFF
        -DMD_LINK_STDLIB_STATIC=${LINK_STDLIB_STATIC}
)

vcpkg_cmake_install()
vcpkg_copy_pdbs()
vcpkg_cmake_config_fixup(CONFIG_PATH share/mold)

vcpkg_install_copyright(FILE_LIST "${SOURCE_PATH}/LICENSE")
file(INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

# The GLSL is installed into share/mold/shaders for both configurations; only the release
# copy is of any use.
file(REMOVE_RECURSE
    "${CURRENT_PACKAGES_DIR}/debug/include"
    "${CURRENT_PACKAGES_DIR}/debug/share"
)
