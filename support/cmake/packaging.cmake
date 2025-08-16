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

set(CPACK_PACKAGE_NAME "OpenSpace")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/LICENSE.md")
set(CPACK_PACKAGE_VERSION_MAJOR "${OPENSPACE_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${OPENSPACE_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${OPENSPACE_VERSION_PATCH}")
# required for deb and tgz
set(CPACK_PACKAGE_VERSION "${OPENSPACE_VERSION_MAJOR}.${OPENSPACE_VERSION_MINOR}.${OPENSPACE_VERSION_PATCH}")

set(OPENSPACE_VERSION_NUMBER
  "${OPENSPACE_VERSION_MAJOR}.${OPENSPACE_VERSION_MINOR}.${OPENSPACE_VERSION_PATCH}"
)
set(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenSpace ${OPENSPACE_VERSION_NUMBER}")
set(CPACK_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION_NUMBER}"
)
set(CPACK_STRIP_FILES 1)

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
endif ()

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

if(UNIX AND NOT APPLE)

  # Binary
    install(TARGETS OpenSpace RUNTIME DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace/bin)

    # ------------------------------------------------------------------------------
    # Bundle Chromium Embedded Framework (CEF) runtime
    # ------------------------------------------------------------------------------
    
    # Path to CEF binary dir - is set as CEF_ROOT in modules/webbrowser/cmake/cef_support.cmake
    # set(CEF_BINARY_DIR "${CMAKE_SOURCE_DIR}/build/modules/webbrowser/ext/cef/cef_binary_127")
    
    # Main CEF shared library
    install(FILES
        ${CEF_ROOT}/Release/libcef.so
        DESTINATION lib
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
        install(FILES ${CEF_SHARED_LIBS} DESTINATION lib)
    endif()
    
    install(DIRECTORY
        ${CEF_ROOT}/Resources/locales/
        DESTINATION share/openspace/cef_resources/locales
    )
    
    # ------------------------------------------------------------------------------
    # Ensure executable can find private libcef.so at runtime
    # ------------------------------------------------------------------------------
    set_target_properties(OpenSpace PROPERTIES
        INSTALL_RPATH "$ORIGIN/../lib"
    )

    
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
    install(DIRECTORY documentation/ DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/openspace)
    install(FILES ACKNOWLEDGMENTS.md CREDITS.md LICENSE.md README.md DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/openspace)
    
    # Config
    install(FILES openspace.cfg DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)

    # Patch to be applied post-install or during packaging
    install(FILES support/cmake/openspacecfg.patch DESTINATION ${CMAKE_INSTALL_DATADIR}/openspace)

  if (DEFINED OPENSPACE_DISTRO AND OPENSPACE_DISTRO STREQUAL "ubuntu24.04")
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "libglew2.2, libpng16-16t64, freeglut3-dev, libxrandr2, libxinerama1, libx11-6, libxcursor1, libcurl4t64, libxi6, libasound2t64, libgdal34t64, libboost-all-dev, qt6-base-dev, libmpv2, libvulkan1")
  else ()
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "libstdc++6 (>= 13), libglew2.2, libpng16-16, freeglut3, libxrandr2, libxinerama1, libx11-6, libxcursor1, libcurl4, libxi6, libasound2, libgdal30, libboost1.74-dev, qt6-base-dev, libmpv1, libvulkan1")
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
  set(CPACK_DEBIAN_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION}")
  set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
  set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "OpenSpace: interactive data visualization tool")
  set(CPACK_DEBIAN_PACKAGE_MAINTAINER "OpenSpace team <info@openspaceproject.com>")
  set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${CMAKE_SOURCE_DIR}/support/deb/postinst")
endif ()

include (CPack)
