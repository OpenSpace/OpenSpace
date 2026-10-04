##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
#                                                                                        #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#                                                                                        #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#                                                                                        #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

set(CPACK_MONOLITHIC_INSTALL TRUE)
include(InstallRequiredSystemLibraries)

set(CPACK_PACKAGE_NAME "OpenSpace")
# Deliberately not CPACK_PACKAGE_DESCRIPTION_FILE pointing at README.md: that file
# contains raw HTML and shield badges which turn into noise in the Debian control file
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
  "Interactive visualization of the entire known universe"
)
set(CPACK_PACKAGE_DESCRIPTION
  "OpenSpace is an open source, non-commercial interactive data visualization software \
designed to visualize the entire known universe and portray our ongoing efforts to \
investigate the cosmos. It supports interactive presentation of dynamic data from \
observations, simulations, and space mission planning and operations, and can drive \
anything from a personal computer to a planetarium dome."
)
# CPackRPM does not read CPACK_PACKAGE_DESCRIPTION. Left unset, its %description falls
# back to CPack's own "This is an installer created using CPack" boilerplate, which is
# what "dnf info openspace" would then show
set(CPACK_RPM_PACKAGE_DESCRIPTION "${CPACK_PACKAGE_DESCRIPTION}")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/LICENSE.md")
# OPENSPACE_VERSION_* are only filled in for release builds (see the top-level
# CMakeLists.txt), so for every other build fall back to the version in the vcpkg
# manifest. Without this the packages end up named "OpenSpace-" and carry CPack's 0.1.1
# default version
if (OPENSPACE_VERSION_MAJOR)
  set(OPENSPACE_VERSION_NUMBER
    "${OPENSPACE_VERSION_MAJOR}.${OPENSPACE_VERSION_MINOR}.${OPENSPACE_VERSION_PATCH}"
  )
else ()
  file(READ "${PROJECT_SOURCE_DIR}/vcpkg.json" OPENSPACE_VCPKG_MANIFEST)
  string(JSON OPENSPACE_VERSION_NUMBER GET "${OPENSPACE_VCPKG_MANIFEST}" "version-string")
  unset(OPENSPACE_VCPKG_MANIFEST)
endif ()

# The RPM generator rejects a version that is not purely numeric, so a prerelease suffix
# ("0.22.0-dev") is kept in the package file name but stripped from the version that ends
# up in the package metadata
if (OPENSPACE_VERSION_NUMBER MATCHES "^([0-9]+)\\.([0-9]+)\\.([0-9]+)")
  set(CPACK_PACKAGE_VERSION_MAJOR "${CMAKE_MATCH_1}")
  set(CPACK_PACKAGE_VERSION_MINOR "${CMAKE_MATCH_2}")
  set(CPACK_PACKAGE_VERSION_PATCH "${CMAKE_MATCH_3}")
  set(CPACK_PACKAGE_VERSION "${CMAKE_MATCH_1}.${CMAKE_MATCH_2}.${CMAKE_MATCH_3}")
else ()
  message(WARNING "Could not parse OpenSpace version '${OPENSPACE_VERSION_NUMBER}'")
  set(CPACK_PACKAGE_VERSION "${OPENSPACE_VERSION_NUMBER}")
endif ()

# Both of these have to be composed here rather than left to the CPack module: the
# CPACK_PACKAGE_VERSION that the module derives is only available after include(CPack),
# which is the last line of this file
set(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenSpace-${OPENSPACE_VERSION_NUMBER}")
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${OPENSPACE_VERSION_NUMBER}")

if (WIN32)
  set(CPACK_GENERATOR "ZIP" "NSIS")
  set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_FILE_NAME}-win64")
  set(CPACK_NSIS_PACKAGE_NAME "OpenSpace ${CPACK_PACKAGE_VERSION}")
  set(CPACK_NSIS_INSTALL_ROOT "C:\\\\OpenSpace\\\\")
  set(CPACK_NSIS_DISPLAY_NAME "OpenSpace ${CPACK_PACKAGE_VERSION}")
  set(CPACK_NSIS_URL_INFO_ABOUT "https://openspaceproject.com/")
  set(CPACK_NSIS_CONTACT "support@openspaceproject.com")
  set(CPACK_NSIS_HELP_LINK "https://openspaceproject.com/")
  set(CPACK_NSIS_MUI_ICON "${PROJECT_SOURCE_DIR}\\\\apps\\\\OpenSpace\\\\openspace.ico")
  set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\OpenSpace.exe")
  # The redistributable itself is staged by the install(CODE) further down. The registry
  # key is read from the 64-bit view as the NSIS installer itself is a 32-bit executable
  file(WRITE "${CMAKE_BINARY_DIR}/vcredist.nsh" [=[
!macro OpenSpaceInstallVCRedist
  SetRegView 64
  ReadRegDWORD $0 HKLM "SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64" "Installed"
  SetRegView 32
  StrCmp $0 1 openspace_vcredist_done
  DetailPrint "Installing the Microsoft Visual C++ Redistributable"
  ExecWait '"$INSTDIR\vc_redist.x64.exe" /install /quiet /norestart' $1
  StrCmp $1 3010 0 openspace_vcredist_done
  SetRebootFlag true
  openspace_vcredist_done:
!macroend
]=])
  # makensis cannot resolve forward slashes in an !include, and the backslashes have to be
  # doubled for CPackConfig.cmake like they are for the other paths in here
  string(REPLACE "/" "\\\\" OPENSPACE_VCREDIST_NSH "${CMAKE_BINARY_DIR}/vcredist.nsh")
  set(CPACK_NSIS_DEFINES "!include '${OPENSPACE_VCREDIST_NSH}'")
  set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS
    "CreateShortCut \'$DESKTOP\\\\OpenSpace.lnk\' \'$INSTDIR\\\\bin\\\\OpenSpace.exe\'
    !insertmacro OpenSpaceInstallVCRedist"
  )
  set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS "Delete \'$DESKTOP\\\\OpenSpace.lnk\'")
else()
  set(CPACK_GENERATOR "ZIP" "TGZ" "DEB" "RPM")
  if (EXISTS "/etc/os-release")
    file(READ "/etc/os-release" OS_RELEASE)
    # The quotes around VERSION_ID are optional in os-release and distributions differ:
    # Ubuntu writes VERSION_ID="26.04", Fedora writes VERSION_ID=44. Matching only the
    # quoted spelling left the version empty and every Fedora package named "linux-amd64"
    if (OS_RELEASE MATCHES "ubuntu")
      set(LINUX_DISTRO "ubuntu")
      string(REGEX MATCH "VERSION_ID=\"?([0-9.]+)\"?" _ ${OS_RELEASE})
      set(LINUX_DISTRO_VERSION "${CMAKE_MATCH_1}")
      string(REPLACE "." "" LINUX_DISTRO_VERSION ${LINUX_DISTRO_VERSION})
    elseif (OS_RELEASE MATCHES "fedora")
      set(LINUX_DISTRO "fedora")
      string(REGEX MATCH "VERSION_ID=\"?([0-9]+)\"?" _ ${OS_RELEASE})
      set(LINUX_DISTRO_VERSION "${CMAKE_MATCH_1}")
    elseif (OS_RELEASE MATCHES "arch")
      set(LINUX_DISTRO "archlinux")
      set(LINUX_DISTRO_VERSION "latest")
    else()
      set(LINUX_DISTRO "linux")
      set(LINUX_DISTRO_VERSION "")
    endif()
  else()
    set(LINUX_DISTRO "linux")
    set(LINUX_DISTRO_VERSION "")
  endif()
  if (LINUX_DISTRO AND LINUX_DISTRO_VERSION)
    set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_FILE_NAME}-${LINUX_DISTRO}${LINUX_DISTRO_VERSION}-amd64")
  else()
    set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_FILE_NAME}-linux-amd64")
  endif()

  set(OPENSPACE_INSTALL_ROOT "/opt/OpenSpace-${OPENSPACE_VERSION_NUMBER}")
  set(CPACK_OPENSPACE_INSTALL_ROOT "${OPENSPACE_INSTALL_ROOT}")
  set(CPACK_PROJECT_CONFIG_FILE
    "${PROJECT_SOURCE_DIR}/support/cmake/cpack_project_config.cmake"
  )
  set(CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST_ADDITION "/opt")

  ########################################################################################
  # Desktop integration                                                                  #
  ########################################################################################
  set(OPENSPACE_ICON_FILE "${PROJECT_SOURCE_DIR}/support/linux/openspace.png")
  set(OPENSPACE_LAUNCHER_SCRIPT "${PROJECT_BINARY_DIR}/linux/openspace-launcher")
  configure_file(
    "${PROJECT_SOURCE_DIR}/support/linux/openspace-launcher.in"
    "${OPENSPACE_LAUNCHER_SCRIPT}"
    @ONLY
    FILE_PERMISSIONS
      OWNER_READ OWNER_WRITE OWNER_EXECUTE
      GROUP_READ GROUP_EXECUTE
      WORLD_READ WORLD_EXECUTE
    NEWLINE_STYLE UNIX
  )

  set(OPENSPACE_DESKTOP_EXEC "openspace")
  set(OPENSPACE_PACKAGE_DESKTOP_FILE "${PROJECT_BINARY_DIR}/linux/openspace.desktop")
  configure_file(
    "${PROJECT_SOURCE_DIR}/support/linux/openspace.desktop.in"
    "${OPENSPACE_PACKAGE_DESKTOP_FILE}"
    @ONLY
    NEWLINE_STYLE UNIX
  )

  foreach (script postinst postrm)
    configure_file(
      "${PROJECT_SOURCE_DIR}/support/linux/deb/${script}.in"
      "${PROJECT_BINARY_DIR}/linux/deb/${script}"
      @ONLY
      FILE_PERMISSIONS
        OWNER_READ OWNER_WRITE OWNER_EXECUTE
        GROUP_READ GROUP_EXECUTE
        WORLD_READ WORLD_EXECUTE
      NEWLINE_STYLE UNIX
    )
  endforeach ()
  set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA
    "${PROJECT_BINARY_DIR}/linux/deb/postinst"
    "${PROJECT_BINARY_DIR}/linux/deb/postrm"
  )

  foreach (script post_install post_uninstall)
    configure_file(
      "${PROJECT_SOURCE_DIR}/support/linux/rpm/${script}.in"
      "${PROJECT_BINARY_DIR}/linux/rpm/${script}"
      @ONLY
      NEWLINE_STYLE UNIX
    )
  endforeach ()
  set(CPACK_RPM_POST_INSTALL_SCRIPT_FILE "${PROJECT_BINARY_DIR}/linux/rpm/post_install")
  set(CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE
    "${PROJECT_BINARY_DIR}/linux/rpm/post_uninstall"
  )

  ########################################################################################
  # Dependencies                                                                         #
  ########################################################################################
  set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
  set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://openspaceproject.com/")
  set(CPACK_DEBIAN_PACKAGE_SECTION "graphics")
  set(CPACK_RPM_PACKAGE_ARCHITECTURE "x86_64")
  set(CPACK_RPM_PACKAGE_URL "https://openspaceproject.com/")
  set(CPACK_RPM_PACKAGE_LICENSE "MIT")

  find_program(OPENSPACE_DPKG_SHLIBDEPS_EXECUTABLE dpkg-shlibdeps)
  mark_as_advanced(OPENSPACE_DPKG_SHLIBDEPS_EXECUTABLE)
  if (OPENSPACE_DPKG_SHLIBDEPS_EXECUTABLE)
    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
    # Without these, every soname that OpenSpace resolves out of its own bin/ - the vcpkg
    # Qt and the CEF runtime. Pointing dpkg-shlibdeps at the directories the libraries
    # were built in lets it recognize them as private and leave them out
    set(OPENSPACE_SHLIBDEPS_PRIVATE_DIRS "")
    if (NOT OPENSPACE_USE_SYSTEM_QT)
      list(APPEND OPENSPACE_SHLIBDEPS_PRIVATE_DIRS
        "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib"
        "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/debug/lib"
      )
    endif ()
    if (CEF_ROOT)
      list(APPEND OPENSPACE_SHLIBDEPS_PRIVATE_DIRS
        "${CEF_ROOT}/Release" "${CEF_ROOT}/Debug"
      )
    endif ()
    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS_PRIVATE_DIRS
      "${OPENSPACE_SHLIBDEPS_PRIVATE_DIRS}"
    )

    # SoLoud dlopens SDL3, so that SDL3 is in no soname list. A Recommends rather than a
    # Depends, because the package name only exists on recent distributions and it might
    # not be needed at all by OpenSpace
    set(CPACK_DEBIAN_PACKAGE_RECOMMENDS "libsdl3-0")
  else ()
    message(STATUS
      "dpkg-shlibdeps was not found. The DEB package will use a hand-written dependency "
      "list that cannot be verified on this machine"
    )
    if (OPENSPACE_USE_SYSTEM_QT)
      set(OPENSPACE_DEBIAN_QT_DEPENDS "libqt6core6, libqt6gui6, libqt6widgets6, ")
    else ()
      set(OPENSPACE_DEBIAN_QT_DEPENDS "")
    endif ()
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "${OPENSPACE_DEBIAN_QT_DEPENDS}libc6, libstdc++6, libgcc-s1, libgl1, libglx0, libx11-6, libx11-xcb1, libxcb1, libxcb-cursor0, libxcb-icccm4, libxcb-image0, libxcb-keysyms1, libxcb-randr0, libxcb-render-util0, libxcb-shape0, libxcb-sync1, libxcb-xfixes0, libxcb-xkb1, libxext6, libxrender1, libxi6, libxfixes3, libxdamage1, libxcomposite1, libxcursor1, libxrandr2, libxtst6, libxkbcommon0, libxkbcommon-x11-0, libwayland-client0, libfontconfig1, libfreetype6, libexpat1, libmpv2, libnss3, libnspr4, libdbus-1-3, libatspi2.0-0, libcups2, libdrm2, libgbm1, libasound2t64 | libasound2")
  endif ()

  # RPM derives both the requires and the provides from the packaged files but needs
  # corrections since we bundle our own libraries
  set(OPENSPACE_RPM_BUNDLED_PATTERNS
    "libQt6"
    "libcef[.]so[(]"
    "libEGL[.]so[(]"
    "libGLESv2[.]so[(]"
    "libvk_swiftshader[.]so[(]"
  )
  list(JOIN OPENSPACE_RPM_BUNDLED_PATTERNS "|" OPENSPACE_RPM_BUNDLED_REGEX)
  set(CPACK_RPM_SPEC_MORE_DEFINE
"%global __provides_exclude_from ^/opt/.*$
%global __requires_exclude ^(${OPENSPACE_RPM_BUNDLED_REGEX})"
  )
    # SoLoud dlopens SDL3, so that SDL3 is in no soname list. A Recommends rather than a
    # Depends, because the package name only exists on recent distributions and it might
    # not be needed at all by OpenSpace
  set(CPACK_RPM_PACKAGE_SUGGESTS "SDL3")

  ########################################################################################
  # AppImage                                                                             #
  ########################################################################################
  # CPack has no AppImage generator, so it is driven through the External generator and
  # support/cmake/appimage.cmake, which turns the staged install tree into an AppDir and
  # hands it to linuxdeploy. "External" is not added to CPACK_GENERATOR above: a plain
  # `cpack` should keep producing the archives and the DEB/RPM packages without requiring
  # the AppImage tooling.
  option(OPENSPACE_STRICT_PACKAGING
    "Treat packaging problems that only affect other machines as errors" OFF
  )

  find_program(OPENSPACE_LINUXDEPLOY_EXECUTABLE
    NAMES linuxdeploy linuxdeploy-x86_64.AppImage
  )
  find_program(OPENSPACE_LINUXDEPLOY_PLUGIN_QT
    NAMES linuxdeploy-plugin-qt linuxdeploy-plugin-qt-x86_64.AppImage
  )
  find_program(OPENSPACE_APPIMAGETOOL_EXECUTABLE
    NAMES appimagetool appimagetool-x86_64.AppImage
  )
  # A type 2 AppImage mounts itself through FUSE, and the runtime appimagetool embeds by
  # default needs libfuse2, which Ubuntu 24.04 and later do not install.
  set(OPENSPACE_APPIMAGE_RUNTIME_FILE "" CACHE FILEPATH
    "AppImage runtime to embed instead of the one that appimagetool ships with"
  )
  mark_as_advanced(OPENSPACE_LINUXDEPLOY_EXECUTABLE OPENSPACE_LINUXDEPLOY_PLUGIN_QT
    OPENSPACE_APPIMAGETOOL_EXECUTABLE
  )

  # The Qt plugin of linuxdeploy locates the Qt installation through qmake. Left to its
  # own devices it takes whatever qmake is on the PATH, which on a machine with an
  # unrelated Qt SDK installed deploys plugins from a different Qt build than the one
  # OpenSpace is linked against
  if (OPENSPACE_USE_SYSTEM_QT)
    set(OPENSPACE_QT_ROOT "${QT6_INSTALL_PREFIX}")
    set(OPENSPACE_QMAKE_HINT "${QT6_INSTALL_PREFIX}/${QT6_INSTALL_BINS}")
  else ()
    set(OPENSPACE_QT_ROOT "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}")
    set(OPENSPACE_QMAKE_HINT "${OPENSPACE_QT_ROOT}/tools/Qt6/bin")
  endif ()
  find_program(OPENSPACE_QMAKE_EXECUTABLE
    NAMES qmake6 qmake
    HINTS "${OPENSPACE_QMAKE_HINT}"
    NO_DEFAULT_PATH
  )
  mark_as_advanced(OPENSPACE_QMAKE_EXECUTABLE)

  # linuxdeploy does not deploy anything on the AppImage excludelist. That holds for the
  # font and graphics stack, but not for the backends that libmpv link:
  # - JACK is a pro-audio server that no desktop installs by default,
  #  - PipeWire is missing from older distributions, and libusb is not guaranteed either.
  # Everything here is passed to linuxdeploy with --library, which deploys it regardless
  # of the excludelist.
  #
  # SoLoud dlopens SDL3, so that SDL3 is in no soname list. A Recommends rather than a
  # Depends, because the package name only exists on recent distributions and it might
  # not be needed at all by OpenSpace
  set(OPENSPACE_APPIMAGE_FORCED_SONAMES
    "libjack.so.0"
    "libpipewire-0.3.so.0"
    "libusb-1.0.so.0"
    "libmujs.so"
    "libSDL3.so.0"
  )
  # The lookup goes through ldconfig rather than find_library because it has to agree with
  # what the dynamic loader will actually pick, and that is not always a standard library
  # directory: on Fedora libjack.so.0 is PipeWire's reimplementation and lives in
  # /usr/lib64/pipewire-0.3/jack/, which find_library does not search. Missing it there
  # produced an AppImage that failed to start on every distribution.
  set(OPENSPACE_APPIMAGE_FORCED_LIBRARIES "")
  execute_process(
    COMMAND ldconfig -p
    OUTPUT_VARIABLE OPENSPACE_LDCONFIG_CACHE
    ERROR_QUIET
  )
  foreach (soname ${OPENSPACE_APPIMAGE_FORCED_SONAMES})
    # Cache entries look like "\tlibjack.so.0 (libc6,x86-64) => /usr/lib/libjack.so.0"
    if (OPENSPACE_LDCONFIG_CACHE MATCHES "\n\t${soname} \\([^)]*x86-64[^)]*\\) => ([^\n]+)")
      list(APPEND OPENSPACE_APPIMAGE_FORCED_LIBRARIES "${CMAKE_MATCH_1}")
    elseif (OPENSPACE_STRICT_PACKAGING)
      message(FATAL_ERROR
        "AppImage: ${soname} was not found in the ldconfig cache. The resulting AppImage "
        "would silently differ from one built on a machine that has it, and would only "
        "run where the host provides it. Install the library on this build machine, or "
        "remove it from OPENSPACE_APPIMAGE_FORCED_SONAMES if it is genuinely no longer "
        "needed. Turn OPENSPACE_STRICT_PACKAGING off to build anyway"
      )
    else ()
      message(STATUS
        "AppImage: ${soname} was not found in the ldconfig cache and will not be "
        "bundled. The AppImage will only run on machines that provide it themselves"
      )
    endif ()
  endforeach ()
  set(CPACK_OPENSPACE_APPIMAGE_FORCED_LIBRARIES "${OPENSPACE_APPIMAGE_FORCED_LIBRARIES}")

  # The desktop entry and the AppRun both need the version baked in, so they are generated
  # rather than used from the source tree directly. The Exec here has to be the basename
  # of the executable that linuxdeploy deploys, not the /usr/bin/openspace symlink that
  # only the system packages have, or linuxdeploy rejects the desktop file.
  set(OPENSPACE_DESKTOP_EXEC "OpenSpace")
  set(OPENSPACE_APPIMAGE_DESKTOP_FILE "${PROJECT_BINARY_DIR}/appimage/openspace.desktop")
  set(OPENSPACE_APPIMAGE_APPRUN "${PROJECT_BINARY_DIR}/appimage/AppRun")
  configure_file(
    "${PROJECT_SOURCE_DIR}/support/linux/openspace.desktop.in"
    "${OPENSPACE_APPIMAGE_DESKTOP_FILE}"
    @ONLY
    NEWLINE_STYLE UNIX
  )
  configure_file(
    "${PROJECT_SOURCE_DIR}/support/appimage/AppRun.in"
    "${OPENSPACE_APPIMAGE_APPRUN}"
    @ONLY
    FILE_PERMISSIONS
      OWNER_READ OWNER_WRITE OWNER_EXECUTE
      GROUP_READ GROUP_EXECUTE
      WORLD_READ WORLD_EXECUTE
    NEWLINE_STYLE UNIX
  )

  set(CPACK_EXTERNAL_ENABLE_STAGING TRUE)
  set(CPACK_EXTERNAL_PACKAGE_SCRIPT "${PROJECT_SOURCE_DIR}/support/cmake/appimage.cmake")

  # Only variables whose name starts with CPACK_ make it into the generated
  # CPackConfig.cmake, which is the only way to get anything through to the script above
  set(CPACK_OPENSPACE_LINUXDEPLOY_EXECUTABLE "${OPENSPACE_LINUXDEPLOY_EXECUTABLE}")
  set(CPACK_OPENSPACE_LINUXDEPLOY_PLUGIN_QT "${OPENSPACE_LINUXDEPLOY_PLUGIN_QT}")
  set(CPACK_OPENSPACE_APPIMAGETOOL_EXECUTABLE "${OPENSPACE_APPIMAGETOOL_EXECUTABLE}")
  set(CPACK_OPENSPACE_APPIMAGE_RUNTIME_FILE "${OPENSPACE_APPIMAGE_RUNTIME_FILE}")
  set(CPACK_OPENSPACE_QMAKE_EXECUTABLE "${OPENSPACE_QMAKE_EXECUTABLE}")
  set(CPACK_OPENSPACE_QT_ROOT "${OPENSPACE_QT_ROOT}")
  set(CPACK_OPENSPACE_APPIMAGE_DESKTOP_FILE "${OPENSPACE_APPIMAGE_DESKTOP_FILE}")
  set(CPACK_OPENSPACE_APPIMAGE_APPRUN "${OPENSPACE_APPIMAGE_APPRUN}")
  # Not apps/OpenSpace/openspace.png: linuxdeploy only accepts icons in the sizes that the
  # hicolor theme defines, and that master is 1024x1024. support/linux/openspace.png is
  # the same image downscaled to 512x512, and has to be regenerated when the master
  # changes
  set(CPACK_OPENSPACE_APPIMAGE_ICON "${OPENSPACE_ICON_FILE}")
endif()
set(CPACK_PACKAGE_ICON "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.png")
set(CPACK_STRIP_FILES 1)
set(CPACK_PACKAGE_VENDOR "OpenSpace Project")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://openspaceproject.com/")
set(CPACK_PACKAGE_CONTACT "support@openspaceproject.com")

install(TARGETS OpenSpace RUNTIME DESTINATION bin COMPONENT Runtime)

if (WIN32)
  foreach (target AssetBuilder Sync TaskRunner OpenSpace_Helper)
    if (TARGET ${target})
      install(TARGETS ${target} RUNTIME DESTINATION bin COMPONENT Runtime)
    endif ()
  endforeach ()
endif ()

# The SGCT calibrator and the test patterns it loads from its working directory. They come
# out of the vcpkg tools folder rather than a target, see the top-level CMakeLists
if (EXISTS "${OPENSPACE_SGCT_CALIBRATOR}")
  install(PROGRAMS "${OPENSPACE_SGCT_CALIBRATOR}" DESTINATION bin COMPONENT Runtime)
  install(FILES ${OPENSPACE_SGCT_CALIBRATOR_PATTERNS} DESTINATION bin COMPONENT Runtime)
endif ()

if (WIN32)
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/bin/" DESTINATION bin COMPONENT Runtime FILES_MATCHING PATTERN "*.dll")
  if (OPENSPACE_USE_SYSTEM_QT)
    # The blanket copy of the vcpkg bin folder above no longer covers Qt, so the Qt DLLs
    # and the runtime plugins have to be taken from the system installation instead
    install(IMPORTED_RUNTIME_ARTIFACTS Qt6::Core Qt6::Gui Qt6::Widgets Qt6::Network RUNTIME DESTINATION bin COMPONENT Runtime)
    install(DIRECTORY "${QT6_INSTALL_PREFIX}/${QT6_INSTALL_PLUGINS}/" DESTINATION bin COMPONENT Runtime)
  else ()
    install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/$<IF:$<CONFIG:Debug>,debug/,>Qt6/plugins/" DESTINATION bin COMPONENT Runtime)
  endif ()
  # The plugins go into bin/ and not a plugins/ folder of their own, as Qt only searches
  # next to the executable and no qt.conf is shipped
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/bin/" DESTINATION bin COMPONENT Runtime FILES_MATCHING PATTERN "msvcp*.dll" PATTERN "vcruntime*.dll")
endif()

if (WIN32)
  # Downloaded when the package is staged rather than while configuring, so that a regular
  # configure does not need network access. The download is cached in the build directory,
  # which keeps the ZIP and NSIS generators from fetching it twice. It ends up in the
  # package root, where the NSIS installer picks it up to run it
  install(CODE "set(OPENSPACE_VCREDIST_CACHE \"${CMAKE_BINARY_DIR}/vc_redist.x64.exe\")" COMPONENT Runtime)
  install(CODE [[
    if (NOT EXISTS "${OPENSPACE_VCREDIST_CACHE}")
      message(STATUS "Downloading the Microsoft Visual C++ Redistributable")
      file(DOWNLOAD
        "https://aka.ms/vs/17/release/vc_redist.x64.exe"
        "${OPENSPACE_VCREDIST_CACHE}.part"
        STATUS OPENSPACE_VCREDIST_STATUS
        TLS_VERIFY ON
      )
      list(GET OPENSPACE_VCREDIST_STATUS 0 OPENSPACE_VCREDIST_CODE)
      if (NOT OPENSPACE_VCREDIST_CODE EQUAL 0)
        file(REMOVE "${OPENSPACE_VCREDIST_CACHE}.part")
        message(FATAL_ERROR "Failed to download vc_redist.x64.exe: ${OPENSPACE_VCREDIST_STATUS}")
      endif ()
      file(RENAME "${OPENSPACE_VCREDIST_CACHE}.part" "${OPENSPACE_VCREDIST_CACHE}")
    endif ()
    file(COPY "${OPENSPACE_VCREDIST_CACHE}" DESTINATION "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}")
  ]] COMPONENT Runtime)
endif()

if (UNIX AND NOT OPENSPACE_USE_SYSTEM_QT)
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/$<IF:$<CONFIG:Debug>,debug/,>lib/" DESTINATION bin COMPONENT Runtime FILES_MATCHING PATTERN "libQt6*.so*")
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/$<IF:$<CONFIG:Debug>,debug/,>Qt6/plugins/" DESTINATION bin COMPONENT Runtime)
endif()

if (WIN32 AND OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  # The build tree gets the CEF runtime through a post-build copy, which install() knows
  # nothing about. Resources/ holds the .pak files and icudtl.dat, and the locales folder
  install(DIRECTORY "${CEF_ROOT}/$<CONFIG>/" DESTINATION bin COMPONENT Runtime
    PATTERN "*.lib" EXCLUDE
    PATTERN "*.pdb" EXCLUDE
  )
  install(DIRECTORY "${CEF_ROOT}/Resources/" DESTINATION bin COMPONENT Runtime)
endif ()

if (UNIX AND OPENSPACE_MODULE_WEBBROWSER AND CEF_ROOT)
  # Same as above
  install(DIRECTORY "${CEF_ROOT}/$<CONFIG>/" DESTINATION bin COMPONENT Runtime)
  if (TARGET OpenSpace_Helper)
    install(PROGRAMS "$<TARGET_FILE:OpenSpace_Helper>" DESTINATION bin COMPONENT Runtime)
    install(CODE [[
      file(RPATH_SET
        FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/OpenSpace_Helper"
        NEW_RPATH "$ORIGIN"
      )
    ]] COMPONENT Runtime)
  endif ()
endif()

if (UNIX)
  install(PROGRAMS "${OPENSPACE_LAUNCHER_SCRIPT}" DESTINATION bin COMPONENT Runtime)
  install(FILES "${OPENSPACE_PACKAGE_DESKTOP_FILE}" DESTINATION share/applications)
  install(FILES "${OPENSPACE_ICON_FILE}"
    DESTINATION share/icons/hicolor/512x512/apps
  )

  install(CODE "set(OPENSPACE_STRIP_EXECUTABLE \"${CMAKE_STRIP}\")" COMPONENT Runtime)
  install(CODE [[
    if (CMAKE_INSTALL_DO_STRIP AND OPENSPACE_STRIP_EXECUTABLE)
      file(GLOB_RECURSE OPENSPACE_INSTALLED_LIBRARIES
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/*.so"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/*.so.*"
      )
      list(APPEND OPENSPACE_INSTALLED_LIBRARIES
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/OpenSpace_Helper"
        "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/calibrator"
      )
      foreach (library ${OPENSPACE_INSTALLED_LIBRARIES})
        if (EXISTS "${library}")
          # --strip-unneeded and not --strip-all: the dynamic symbol table is what the
          # loader resolves the bundled Qt against, and removing it breaks every one of
          # them. ERROR_QUIET because the glob also picks up the odd data file that only
          # looks like a library, and strip refusing it is not a packaging failure.
          execute_process(
            COMMAND "${OPENSPACE_STRIP_EXECUTABLE}" --strip-unneeded "${library}"
            ERROR_QUIET
          )
        endif ()
      endforeach ()
    endif ()
  ]] COMPONENT Runtime)
endif()

# findConfiguration() starts at ${BIN} and walks up until it finds this file, and the
# directory it lives in becomes ${BASE}. Without it an installed OpenSpace cannot start
install(FILES "${PROJECT_SOURCE_DIR}/openspace.cfg" DESTINATION .)

install(DIRECTORY "${PROJECT_SOURCE_DIR}/data/" DESTINATION data)
install(DIRECTORY "${PROJECT_SOURCE_DIR}/config/" DESTINATION config)
install(DIRECTORY "${PROJECT_SOURCE_DIR}/modules/" DESTINATION modules)
install(DIRECTORY "${PROJECT_SOURCE_DIR}/scripts/" DESTINATION scripts)
install(DIRECTORY "${PROJECT_SOURCE_DIR}/shaders/" DESTINATION shaders)

install(FILES "${PROJECT_SOURCE_DIR}/README.md" "${PROJECT_SOURCE_DIR}/LICENSE.md" "${PROJECT_SOURCE_DIR}/CREDITS.md" DESTINATION .)

if (WIN32)
  install(FILES "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.ico" DESTINATION .)
  install(FILES "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.png" DESTINATION .)
  install(FILES "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.rc" DESTINATION .)
else()
  install(FILES "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.png" DESTINATION .)
endif()

if (OPENSPACE_MODULE_SPOUT)
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/bin/" DESTINATION bin COMPONENT Runtime FILES_MATCHING PATTERN "*Spout*.dll")
endif()

if (WIN32 AND OPENSPACE_MODULE_VIDEO)
  if (EXISTS "${LIBMPV_ROOT}/libmpv/bin/mpv-2.dll")
    install(FILES "${LIBMPV_ROOT}/libmpv/bin/mpv-2.dll" "${LIBMPV_ROOT}/libmpv/bin/libopenh264.dll" DESTINATION bin COMPONENT Runtime)
  endif()
endif()

install(FILES "${PROJECT_SOURCE_DIR}/THIRD_PARTY_LICENSES.md" DESTINATION .)

include(CPack)
