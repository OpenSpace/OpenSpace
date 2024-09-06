##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2024                                                                #
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
set(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenSpace ${OPENSPACE_VERSION_NUMBER}")
set(CPACK_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION_NUMBER} ${OPENSPACE_VERSION_STRING}"
)
set(CPACK_STRIP_FILES 1)

install(DIRECTORY
  ${PROJECT_SOURCE_DIR}/bin/${CMAKE_BUILD_TYPE}/
  DESTINATION bin
  USE_SOURCE_PERMISSIONS
)
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

if (WIN32)
  set(CPACK_GENERATOR ZIP)
  # Need backslash for correct subdirectory paths
  set(CPACK_PACKAGE_ICON "${PROJECT_SOURCE_DIR}\\\\apps\\\\OpenSpace\\\\openspace.png")
  set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}\\\\${OPENSPACE_VERSION_NUMBER} ${OPENSPACE_VERSION_STRING}")
else ()
  set(CPACK_GENERATOR TGZ)
  set(CPACK_PACKAGE_ICON "${PROJECT_SOURCE_DIR}/apps/OpenSpace/openspace.png")
  set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}/${OPENSPACE_VERSION_NUMBER} ${OPENSPACE_VERSION_STRING}")
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

include (CPack)
