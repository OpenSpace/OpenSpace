#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2017                                                               #
#                                                                                       #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this  #
# software and associated documentation files (the "Software"), to deal in the Software #
# without restriction, including without limitation the rights to use, copy, modify,    #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    #
# permit persons to whom the Software is furnished to do so, subject to the following   #
# conditions:                                                                           #
#                                                                                       #
# The above copyright notice and this permission notice shall be included in all copies #
# or substantial portions of the Software.                                              #
#                                                                                       #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         #
#########################################################################################

set(CPACK_MONOLITHIC_INSTALL FALSE)
include(InstallRequiredSystemLibraries)
set(CPACK_PACKAGE_NAME "OpenSpace")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${OPENSPACE_BASE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${OPENSPACE_BASE_DIR}/LICENSE.md")
set(CPACK_PACKAGE_VERSION_MAJOR "${OPENSPACE_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${OPENSPACE_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${OPENSPACE_VERSION_PATCH}")
set(OPENSPACE_VERSION "${OPENSPACE_VERSION_MAJOR}.${OPENSPACE_VERSION_MINOR}.${OPENSPACE_VERSION_PATCH} ${OPENSPACE_VERSION_STRING}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenSpace ${OPENSPACE_VERSION}")

if(WIN32)
	# Need backslash for correct subdirectory paths with NSIS
	if(CMAKE_SIZEOF_VOID_P EQUAL 8 )
		set(CPACK_PACKAGE_FILE_NAME         "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION} 64bit")
		set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}\\\\${OPENSPACE_VERSION}\\\\64bit")
		set(CPACK_NSIS_DISPLAY_NAME         "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION} 64-bit")
	else()
		set(CPACK_PACKAGE_FILE_NAME         "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION} 32bit")
		set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}\\\\${OPENSPACE_VERSION}\\\\32bit")
		set(CPACK_NSIS_DISPLAY_NAME         "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION} 32-bit")
	endif()
else()
	set(CPACK_PACKAGE_FILE_NAME         "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION}")
	set(CPACK_PACKAGE_INSTALL_DIRECTORY "CPACK_PACKAGE_NAME/${OPENSPACE_VERSION}")
endif()

set(CPACK_PACKAGE_EXECUTABLES "openspace;OpenSpace")
set(CPACK_GENERATOR ZIP)
set(CPACK_STRIP_FILES 1)

install(DIRECTORY ${OPENSPACE_BASE_DIR}/bin/openspace/${CMAKE_BUILD_TYPE}/ DESTINATION bin COMPONENT Application)
install(DIRECTORY ${OPENSPACE_BASE_DIR}/config/ DESTINATION config COMPONENT Config)
install(DIRECTORY ${OPENSPACE_BASE_DIR}/data/ DESTINATION data COMPONENT Data)
install(DIRECTORY ${OPENSPACE_BASE_DIR}/documentation/ DESTINATION documentation COMPONENT Documentation)
install(DIRECTORY ${OPENSPACE_BASE_DIR}/modules/
    DESTINATION modules COMPONENT Modules
    FILES_MATCHING
	PATTERN "*.glsl"
)
install(DIRECTORY ${OPENSPACE_BASE_DIR}/scripts/ DESTINATION scripts COMPONENT Scripts)
install(DIRECTORY ${OPENSPACE_BASE_DIR}/shaders/ DESTINATION shaders COMPONENT Application)
install(DIRECTORY ${OPENSPACE_BASE_DIR}/tests/ DESTINATION tests COMPONENT Tests)
install(FILES ${OPENSPACE_BASE_DIR}/openspace.cfg DESTINATION . COMPONENT Application)

#if(WIN32 AND NOT UNIX)
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  #set(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  #set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\OpenSpace.exe")
  #set(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} OpenSpace")
  #set(CPACK_NSIS_URL_INFO_ABOUT "http://openspace.itn.liu.se/")
  #set(CPACK_NSIS_MODIFY_PATH ON)
#endif(WIN32 AND NOT UNIX)
include(CPack)