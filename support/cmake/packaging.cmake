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
set(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/LICENSE.md")
set(CPACK_PACKAGE_VERSION_MAJOR "${OPENSPACE_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${OPENSPACE_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${OPENSPACE_VERSION_PATCH}")
set(OPENSPACE_VERSION_NUMBER
  "${OPENSPACE_VERSION_MAJOR}.${OPENSPACE_VERSION_MINOR}.${OPENSPACE_VERSION_PATCH}"
)
set(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenSpace-${OPENSPACE_VERSION_NUMBER}")
set(CPACK_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}"
)

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
  set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS "CreateShortCut \'$DESKTOP\\\\OpenSpace.lnk\' \'$INSTDIR\\\\bin\\\\OpenSpace.exe\'")
  set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS "Delete \'$DESKTOP\\\\OpenSpace.lnk\'")
  set(CPACK_NSIS_DEFINES "!define MUI_HEADERIMAGE \\\"${PROJECT_SOURCE_DIR}\\\\apps\\\\OpenSpace\\\\openspace.png\\\"")
else()
  set(CPACK_GENERATOR "ZIP" "TGZ" "DEB" "RPM")
  if (EXISTS "/etc/os-release")
    file(READ "/etc/os-release" OS_RELEASE)
    if (OS_RELEASE MATCHES "ubuntu")
      set(LINUX_DISTRO "ubuntu")
      string(REGEX MATCH "VERSION_ID=\\\"([0-9.]+)\\\"" _ ${OS_RELEASE})
      set(LINUX_DISTRO_VERSION "${CMAKE_MATCH_1}")
      string(REPLACE "." "" LINUX_DISTRO_VERSION ${LINUX_DISTRO_VERSION})
    elseif (OS_RELEASE MATCHES "fedora")
      set(LINUX_DISTRO "fedora")
      string(REGEX MATCH "VERSION_ID=\\\"([0-9]+)\\\"" _ ${OS_RELEASE})
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
  set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
  set(CPACK_DEBIAN_PACKAGE_DEPENDS "libqt6core6 (>= 6.5), libqt6gui6 (>= 6.5), libqt6widgets6 (>= 6.5), libgl1, libglu1-mesa | libglx0, libx11-6, libxext6, libxrender1, libxi6, ibxkbcommon0, libxkbcommon-x11-0, libwayland-client0, libmpv1, libnss3, libnspr4, libdbus-1-3, libatspi2.0-0, libc6 (>= 2.31)")
  set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://openspaceproject.com/")
  set(CPACK_DEBIAN_PACKAGE_SECTION "graphics")
  set(CPACK_RPM_PACKAGE_ARCHITECTURE "x86_64")
  set(CPACK_RPM_PACKAGE_REQUIRES "qt6-core >= 6.5, qt6-gui >= 6.5, qt6-widgets >= 6.5, mesa-libGLU, libX11, libXext, libXrender, libXi, libxkbcommon, libxkbcommon-x11, ibwayland-client, mpv, nss, nspr, dbus-libs, at-spi2-core, glibc >= 2.31")
  set(CPACK_RPM_PACKAGE_URL "https://openspaceproject.com/")
  set(CPACK_RPM_PACKAGE_LICENSE "MIT")
endif()
set(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenSpace-${CPACK_PACKAGE_VERSION}")
set(CPACK_PACKAGE_ICON "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.png")
set(CPACK_STRIP_FILES 1)
set(CPACK_PACKAGE_VENDOR "OpenSpace Project")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://openspaceproject.com/")
set(CPACK_PACKAGE_CONTACT "support@openspaceproject.com")

install(TARGETS OpenSpace RUNTIME DESTINATION bin COMPONENT Runtime)

if (WIN32)
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/bin/" DESTINATION bin COMPONENT Runtime FILES_MATCHING PATTERN "*.dll")
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/$<IF:$<CONFIG:Debug>,debug/,>Qt6/plugins/" DESTINATION plugins COMPONENT Runtime)
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/bin/" DESTINATION bin COMPONENT Runtime FILES_MATCHING PATTERN "msvcp*.dll" PATTERN "vcruntime*.dll")
endif()

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

if (OPENSPACE_OPENVR_SUPPORT)
  install(FILES "${PROJECT_SOURCE_DIR}/ext/sgct/additional_includes/openvr/bin/win64/openvr_api.dll" DESTINATION bin COMPONENT Runtime)
endif()

if (OPENSPACE_MODULE_SPOUT)
  install(DIRECTORY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/bin/" DESTINATION bin COMPONENT Runtime FILES_MATCHING PATTERN "*Spout*.dll")
endif()

if (WIN32 AND OPENSPACE_MODULE_VIDEO)
  if (EXISTS "${PROJECT_SOURCE_DIR}/modules/video/ext/libmpv/bin/libmpv-2.dll")
    install(FILES "${PROJECT_SOURCE_DIR}/modules/video/ext/libmpv/bin/libmpv-2.dll" "${PROJECT_SOURCE_DIR}/modules/video/ext/libmpv/bin/libopenh264.dll" DESTINATION bin COMPONENT Runtime)
  endif()
endif()

install(FILES "${PROJECT_SOURCE_DIR}/THIRD_PARTY_LICENSES.md" DESTINATION .)

include(CPack)
