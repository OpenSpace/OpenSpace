##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2025                                                                #
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

if (WIN32 OR APPLE)
set(CPACK_PACKAGE_NAME "OpenSpace")
endif ()

set(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/LICENSE.md")
set(CPACK_PACKAGE_VERSION_MAJOR "${OPENSPACE_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${OPENSPACE_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${OPENSPACE_VERSION_PATCH}")

set(OPENSPACE_VERSION_NUMBER
  "${OPENSPACE_VERSION_MAJOR}.${OPENSPACE_VERSION_MINOR}.${OPENSPACE_VERSION_PATCH}"
)
set(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenSpace ${OPENSPACE_VERSION_NUMBER}")
set(CPACK_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION_NUMBER}"
)
set(CPACK_STRIP_FILES 1)

if (WIN32 OR APPLE)
if (EXISTS "${PROJECT_SOURCE_DIR}/bin/${CMAKE_BUILD_TYPE}")
  message(STATUS "Directory '${PROJECT_SOURCE_DIR}/bin/${CMAKE_BUILD_TYPE}' exists. Taking the binaries from there.")
  install(DIRECTORY
    ${PROJECT_SOURCE_DIR}/bin/${CMAKE_BUILD_TYPE}/
    DESTINATION bin
    USE_SOURCE_PERMISSIONS
  )
else ()
  message(STATUS "Directory '${PROJECT_SOURCE_DIR}/bin/${CMAKE_BUILD_TYPE}' does not exist. Taking the binaries from '${PROJECT_SOURCE_DIR}/bin'.")
  install(DIRECTORY
    ${PROJECT_SOURCE_DIR}/bin/
    DESTINATION bin
    USE_SOURCE_PERMISSIONS
  )
endif () # if EXISTS

endif () # WIN32 OR APPLE

if (WIN32)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/config/ DESTINATION config)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/data/ DESTINATION data)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/modules/
  DESTINATION modules
  FILES_MATCHING
  PATTERN "*.glsl"
  PATTERN "*.hglsl"
  PATTERN "*.fs"
  PATTERN "*.vs"
  PATTERN "*.lua"
)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/scripts/ DESTINATION scripts)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/shaders/ DESTINATION shaders)

install(FILES
  ${PROJECT_SOURCE_DIR}/openspace.cfg
  ${PROJECT_SOURCE_DIR}/CREDITS.md
  ${PROJECT_SOURCE_DIR}/LICENSE.md
  ${PROJECT_SOURCE_DIR}/README.md
  DESTINATION .
)

endif ()

if (WIN32)
  set(CPACK_GENERATOR ZIP)
  # Need backslash for correct subdirectory paths
  set(CPACK_PACKAGE_ICON "${PROJECT_SOURCE_DIR}\\\\apps\\\\OpenSpace\\\\openspace.png")
  set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}\\\\${OPENSPACE_VERSION_NUMBER}")
else ()
  set(CPACK_GENERATOR "DEB")
  set(CPACK_PACKAGE_ICON "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.png")
  set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}/${OPENSPACE_VERSION_NUMBER}")
endif ()

option(OPENSPACE_CREATE_INSTALLER "Create an OpenSpace installer from the package" OFF)

if (OPENSPACE_CREATE_INSTALLER)
  set(CPACK_PACKAGE_EXECUTABLES "openspace;OpenSpace")
  if (WIN32)
    set(CPACK_GENERATOR "ZIP;NSIS")
    # OpenSpace does NOT seem to handle C:/Program Files/ without crashing
    set(CPACK_NSIS_INSTALL_ROOT "C:")
    # There is a bug in NSI that does not handle full unix paths properly. Make
    # sure there is at least one set of four (4) backlashes.
    set(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_FILE_NAME}")
    # Create the desktop link
    set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS "CreateShortCut '$DESKTOP\\\\${CPACK_NSIS_DISPLAY_NAME}.lnk' '$INSTDIR\\\\bin\\\\OpenSpace.exe' ")
    # Delete the desktop link
    set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS "Delete '$DESKTOP\\\\${CPACK_NSIS_DISPLAY_NAME}.lnk' ")
    # The icon to start the application.
    set(CPACK_NSIS_MUI_ICON "${PROJECT_SOURCE_DIR}\\\\apps\\\\OpenSpace\\\\openspace.ico")
    # Add a link to the application website in the startup menu.
    set(CPACK_NSIS_MENU_LINKS "http://openspaceproject.com/" "OpenSpace Homepage")
    # Set the icon for the application in the Add/Remove programs section.
    set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\OpenSpace.exe")
    # The mail address for the maintainer of the application in the Add/Remove programs section
    set(CPACK_NSIS_CONTACT alexander.bock@liu.se)
    # The url of the application in the Add/Remove programs section
    set(CPACK_NSIS_URL_INFO_ABOUT "http://openspaceproject.com/")
    # Help URL
    set(CPACK_NSIS_HELP_LINK "http://openspaceproject.com/")
  endif ()
endif ()

if (UNIX AND NOT APPLE)

  # for Debian convention, package name should be in lowercase, and docdir path should be as below.
  set(CPACK_PACKAGE_NAME "openspace")
  set(CPACK_PACKAGE_VERSION "0.21.2")           # upstream version only
  set(CPACK_DEBIAN_PACKAGE_RELEASE "1~ubuntu24.04.1")  # or "1~noble1"
  set(CPACK_DEBIAN_FILE_NAME "DEB-DEFAULT")     # get Package_Version-Release_Arch.deb
  set(CMAKE_INSTALL_DATADIR share)
  set(CMAKE_INSTALL_DOCDIR "${CMAKE_INSTALL_DATADIR}/doc/${CPACK_PACKAGE_NAME}" CACHE PATH "" FORCE)

  # Binary
    install(TARGETS OpenSpace RUNTIME DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace/bin)
    install(TARGETS OpenSpace_Helper RUNTIME DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace/bin)

    # man page
    install(FILES support/deb/openspace.1 DESTINATION share/man/man1)

    # ------------------------------------------------------------------------------
    # Bundle Chromium Embedded Framework (CEF) runtime
    # ------------------------------------------------------------------------------
    
    # Path to CEF binary dir - is set as CEF_ROOT in modules/webbrowser/cmake/cef_support.cmake
    # set(CEF_BINARY_DIR "${CMAKE_SOURCE_DIR}/build/modules/webbrowser/ext/cef/cef_binary_127")

    # strip in place
    execute_process(
    COMMAND ${CMAKE_STRIP} --strip-unneeded ${CEF_ROOT}/Release/libcef.so
    )

    
    # Main CEF shared library
    install(FILES
        ${CEF_ROOT}/Release/libcef.so
        DESTINATION lib/openspace
    )
    
    # Resources and locales (required for CEF to work properly)
    install(DIRECTORY
        ${CEF_ROOT}/Resources/
        DESTINATION share/openspace/cef_resources
    )

    # Install all CEF .so files from the Release directory
    file(GLOB CEF_SHARED_LIBS
        "${CEF_BINARY_DIR}/Release/lib*.so"
    )
    
    if(CEF_SHARED_LIBS)
        install(FILES ${CEF_SHARED_LIBS} DESTINATION lib/openspace)
    endif()
    
    install(DIRECTORY
        ${CEF_ROOT}/Resources/locales/
        DESTINATION share/openspace/cef_resources/locales
    )
    
    # ------------------------------------------------------------------------------
    # Ensure executable can find private libcef.so at runtime
    # ------------------------------------------------------------------------------
    # Ensure no build-tree RPATH, and set an install-time RUNPATH to find bundled CEF
    set(CMAKE_SKIP_BUILD_RPATH TRUE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)
    
    set(_openspace_privlib_rpath "$ORIGIN/../../../lib/openspace")
    
    set_target_properties(OpenSpace PROPERTIES
      INSTALL_RPATH "${_openspace_privlib_rpath}"
    )
    
    # helper binaries that also need CEF:
    set_target_properties(OpenSpace_Helper PROPERTIES
      INSTALL_RPATH "${_openspace_privlib_rpath}"
    )

    # After install step, fix RPATH to remove stray '.'
    install(CODE [[
      file(GLOB_RECURSE _bins
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/openspace/bin/OpenSpace*"
      )
      foreach(_bin IN LISTS _bins)
        execute_process(
          COMMAND patchelf --print-rpath "\${_bin}"
          OUTPUT_VARIABLE _rpath
          OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        if("\${_rpath}" MATCHES ".:")
          # Replace '.' with only our intended RPATH
          execute_process(
            COMMAND patchelf --set-rpath "$ORIGIN/../../../lib/openspace" "\${_bin}"
          )
        endif()
      endforeach()
    ]])



    
    # Required assets
    install(DIRECTORY config/ DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)
    install(DIRECTORY modules/ DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace
                FILES_MATCHING
              PATTERN "*.glsl"
              PATTERN "*.hglsl"
              PATTERN "*.fs"
              PATTERN "*.vs"
              PATTERN "*.lua")
    install(DIRECTORY data/ DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)
    install(DIRECTORY scripts/ DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)
    install(DIRECTORY shaders/ DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)
    
    # Documentation
    install(DIRECTORY documentation/ 
        DESTINATION ${CMAKE_INSTALL_DOCDIR})
    install(FILES ACKNOWLEDGMENTS.md CREDITS.md LICENSE.md README.md
        DESTINATION ${CMAKE_INSTALL_DOCDIR})
    
    # Config
    install(FILES openspace.cfg DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)

    # Patch to be applied post-install or during packaging
    install(FILES support/cmake/openspacecfg.patch DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)

  if (DEFINED OPENSPACE_DISTRO AND OPENSPACE_DISTRO STREQUAL "ubuntu24.04")
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "libglew2.2, libpng16-16t64, libglut3.12, libjack0, libxrandr2, libxinerama1, libx11-6, libxcursor1, libcurl4t64, libxi6, libasound2t64, libgdal34t64, libmpv2, libvulkan1")
  else ()
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "libstdc++6 (>= 13), libglew2.2, libpng16-16, freeglut3, libjack0, libxrandr2, libxinerama1, libx11-6, libxcursor1, libcurl4, libxi6, libasound2, libgdal30, libboost1.74-dev, qt6-base-dev, libmpv1, libvulkan1")
  endif ()
  
  # Map CMAKE_SYSTEM_PROCESSOR to Debian arch names
  if (CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|amd64")
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
    elseif (CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "arm64")
    elseif (CMAKE_SYSTEM_PROCESSOR MATCHES "armv7l|armhf")
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "armhf")
    elseif (CMAKE_SYSTEM_PROCESSOR MATCHES "i686|i386")
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "i386")
    else ()
      # Fallback to auto-detected value
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "${CMAKE_SYSTEM_PROCESSOR}")
      message(WARNING "Unknown architecture '${CMAKE_SYSTEM_PROCESSOR}', using as-is for DEB package.")
  endif ()
  
  set(CPACK_DEBIAN_PACKAGE_SECTION "science")
  set(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
  set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)  # Automatic dependency analysis should append to CPACK_DEBIAN_PACKAGE_DEPENDS
  set(CPACK_DEBIAN_PACKAGE_MAINTAINER "OpenSpace team <info@openspaceproject.com>")
  # Short description (synopsis) — do NOT include package name, no trailing period,
  # keep it concise (< ~80 chars)
  set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
        "interactive 3D visualization of space and planetary data")
    
  # Extended (long) description — full sentences; paragraphs allowed
  set(CPACK_DEBIAN_PACKAGE_DESCRIPTION
"OpenSpace is an open-source, real-time 3D application for visualizing astronomy
 and spaceflight data. It renders celestial bodies, star catalogs, and spacecraft
 trajectories, and can be used on desktops and in dome theaters.

 It supports time navigation, high-resolution tiled planetary textures, and
 loading mission ephemerides. The application can be scripted for presentations
 and live shows, and is suitable for education, outreach, and research
 visualizations.")

  # Post-install script to patch cfg file
  ########
  # configure_file(<input> <output> @ONLY): What this does is:
  # Copies the <input> file to <output>.
  # Substitutes variables inside the input that are written as @VAR@ with their current CMake values.
  # With @ONLY, only @VAR@ forms are replaced (not ${VAR}).
  ##########
  configure_file(
      ${CMAKE_SOURCE_DIR}/support/deb/postinst.in
      ${CMAKE_BINARY_DIR}/support/deb/postinst
      @ONLY
    )
    # postinst permissions should be 0755
    file(CHMOD ${CMAKE_BINARY_DIR}/support/deb/postinst
     PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                 GROUP_READ GROUP_EXECUTE
                 WORLD_READ WORLD_EXECUTE)
  
  
  # Adding a script in bin which will set the env vars OPENSPACE_USER & OPENSPACE_GLOBEBROWSING
  # since /usr/share would normally be owned by root and not writable by normal users.
  
  configure_file(
      ${CMAKE_SOURCE_DIR}/support/deb/openspace.in
      ${CMAKE_BINARY_DIR}/openspace
      @ONLY
    )
    
    install(
      PROGRAMS ${CMAKE_BINARY_DIR}/openspace
      DESTINATION bin
      RENAME openspace
    )

  # Adding mandatory deb files
  string(TIMESTAMP CPACK_DEBIAN_CHANGELOG_DATE "%a, %d %b %Y %H:%M:%S %z")

  configure_file(${CMAKE_SOURCE_DIR}/support/deb/changelog.in
               ${CMAKE_BINARY_DIR}/support/deb/changelog @ONLY)

  set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA
    "${CMAKE_BINARY_DIR}/support/deb/changelog"
    "${CMAKE_BINARY_DIR}/support/deb/copyright"
    "${CMAKE_BINARY_DIR}/support/deb/postinst"
  )

  install(FILES ${CMAKE_BINARY_DIR}/support/deb/changelog
        DESTINATION ${CMAKE_INSTALL_DOCDIR}
        RENAME changelog.Debian)
  install(FILES ${CMAKE_BINARY_DIR}/support/deb/copyright
        DESTINATION ${CMAKE_INSTALL_DOCDIR})


  
    # --------------------------------------------------------------------------
    # Remove unwanted developer files (headers, pkgconfig, etc.) from the staged install
    # --------------------------------------------------------------------------
    install(CODE [[
      message(STATUS "Pruning developer files from install tree...")
      file(REMOVE_RECURSE
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/include"
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/glbinding"
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/Tracy"
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/SoLoud"
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/pkgconfig"
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/pkgconfig"
      )
    
      # Static libs
      file(GLOB_RECURSE _static_libs
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/*.a")
      file(REMOVE \${_static_libs})
    
      # Git leftovers
      file(GLOB_RECURSE _gitfiles
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/*.git")
      file(REMOVE \${_gitfiles})
    
      file(GLOB_RECURSE _gitignores
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/.gitignore")
      file(REMOVE \${_gitignores})
    
      # CMake fragments
      file(GLOB_RECURSE _cmakefiles
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/*.cmake")
      file(REMOVE \${_cmakefiles})
    
      # zlib man page
      file(GLOB _zlib_man3
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/man/man3/zlib*")
      file(REMOVE \${_zlib_man3})
    
      # Remove development-related files for runtime-only package
      file(GLOB_RECURSE unwanted_pkgconfigs
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/**/pkgconfig"
      )
    
      file(GLOB_RECURSE unwanted_cmake_dirs
        "\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/**/cmake"
      )
    
      file(REMOVE_RECURSE \${unwanted_pkgconfigs} \${unwanted_cmake_dirs})
    ]])

    
endif () # if UNIX and not APPLE

include (CPack)
