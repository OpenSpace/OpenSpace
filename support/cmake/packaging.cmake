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

# The Windows zip file does not have a top level directory.
# Also, we're bundling vc_redist.x64.exe instead of bundling system libraries on Windows.
if (WIN32)
  set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY OFF)
else ()
  include(InstallRequiredSystemLibraries)
endif ()

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
  "${CPACK_PACKAGE_NAME} ${OPENSPACE_VERSION_NUMBER}"
)
set(CPACK_STRIP_FILES 1)

# Detect architecture and download appropriate VC Redistributable
if(WIN32)
    # only 64-bit builds are supported
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "ARM64|aarch64")
        set(VC_REDIST_URL "https://aka.ms/vs/17/release/vc_redist.arm64.exe")
        set(VC_REDIST_FILENAME "vc_redist.arm64.exe")
    else()
        set(VC_REDIST_URL "https://aka.ms/vs/17/release/vc_redist.x64.exe")
        set(VC_REDIST_FILENAME "vc_redist.x64.exe")
    endif()
        
    set(VC_REDIST_PATH "${CMAKE_BINARY_DIR}/${VC_REDIST_FILENAME}")
    
    if(NOT EXISTS "${VC_REDIST_PATH}")
        message(STATUS "Downloading Visual C++ Redistributable for ${CMAKE_SYSTEM_PROCESSOR}...")
        file(DOWNLOAD
            "${VC_REDIST_URL}"
            "${VC_REDIST_PATH}"
            SHOW_PROGRESS
            STATUS download_status
            TIMEOUT 60
        )
        
        list(GET download_status 0 status_code)
        list(GET download_status 1 status_message)
        
        if(NOT status_code EQUAL 0)
            message(FATAL_ERROR "Failed to download VC Redistributable: ${status_message}")
        endif()
        
        message(STATUS "Downloaded VC Redistributable successfully")
    endif()
    
    # Install it with your package
    install(FILES "${VC_REDIST_PATH}" 
            DESTINATION .
            COMPONENT Runtime)
endif()

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
  PATTERN "*.py"
  PATTERN "*.json"
  PATTERN "*.js"
)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/scripts/ DESTINATION scripts)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/shaders/ DESTINATION shaders)

install(FILES
  ${PROJECT_SOURCE_DIR}/openspace.cfg
  ${PROJECT_SOURCE_DIR}/CREDITS.md
  ${PROJECT_SOURCE_DIR}/LICENSE.md
  ${PROJECT_SOURCE_DIR}/README.md
  ${PROJECT_SOURCE_DIR}/ACKNOWLEDGMENTS.md
  ${PROJECT_SOURCE_DIR}/CITATION.cff
  ${PROJECT_SOURCE_DIR}/CODE_OF_CONDUCT.md
  ${PROJECT_SOURCE_DIR}/COMMIT.md
  DESTINATION .
)

if (WIN32)
    # Install all Qt plugins at once
    # Use environment variable QT6_DIR, or fallback to CMake Qt6_DIR
    # Use environment variable QT6_DIR
    if(DEFINED ENV{QT6_DIR})
        set(QT_INSTALL_DIR "$ENV{QT6_DIR}")
        message(STATUS "Using QT6_DIR from environment: ${QT_INSTALL_DIR}")
    else()
        # Fallback for local builds
        get_filename_component(QT_INSTALL_DIR "${Qt6_DIR}/../../.." ABSOLUTE)
        message(STATUS "Using Qt6_DIR from CMake: ${QT_INSTALL_DIR}")
    endif()
    
    install(FILES "${QT_INSTALL_DIR}/bin/opengl32sw.dll"
        DESTINATION bin COMPONENT Runtime OPTIONAL)
    
    install(FILES "${QT_INSTALL_DIR}/plugins/generic/qtuiotouchplugin.dll"
        DESTINATION bin/generic COMPONENT Runtime OPTIONAL)
    
    install(FILES "${QT_INSTALL_DIR}/plugins/platforms/qwindows.dll"
        DESTINATION bin/platforms COMPONENT Runtime OPTIONAL)
    
    install(FILES "${QT_INSTALL_DIR}/plugins/styles/qmodernwindowsstyle.dll"
        DESTINATION bin/styles COMPONENT Runtime OPTIONAL)
    
    # Fix: OPTIONAL must come before FILES_MATCHING
    install(DIRECTORY "${QT_INSTALL_DIR}/plugins/tls/"
        DESTINATION bin/tls
        COMPONENT Runtime
        OPTIONAL
        FILES_MATCHING PATTERN "*.dll"
    )
    
    # Install DirectX Shader Compiler DLLs from Windows SDK
    if(WIN32)
        # Determine target architecture
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            if(CMAKE_SYSTEM_PROCESSOR MATCHES "ARM64|aarch64")
                set(DX_ARCH "arm64")
            else()
                set(DX_ARCH "x64")
            endif()
        else()
            set(DX_ARCH "x86")
        endif()
        
        message(STATUS "DirectX DLL architecture: ${DX_ARCH}")
        
        set(WINDOWS_KITS_DIR "C:/Program Files (x86)/Windows Kits/10")
        
        # Find SDK versions - look for directories that match version pattern (e.g., 10.0.xxxxx.x)
        file(GLOB SDK_VERSIONS 
            "${WINDOWS_KITS_DIR}/bin/10.0.*"
        )
        
        if(SDK_VERSIONS)
            # Sort and get the latest version
            list(SORT SDK_VERSIONS)
            list(REVERSE SDK_VERSIONS)
            list(GET SDK_VERSIONS 0 LATEST_SDK_VERSION)
            get_filename_component(SDK_VERSION "${LATEST_SDK_VERSION}" NAME)
            
            message(STATUS "Using Windows SDK version: ${SDK_VERSION}")
            
            set(DX_COMPILER_PATH "${WINDOWS_KITS_DIR}/bin/${SDK_VERSION}/${DX_ARCH}/dxcompiler.dll")
            set(DX_IL_PATH "${WINDOWS_KITS_DIR}/bin/${SDK_VERSION}/${DX_ARCH}/dxil.dll")
            
            if(EXISTS "${DX_COMPILER_PATH}" AND EXISTS "${DX_IL_PATH}")
                message(STATUS "Found dxcompiler.dll: ${DX_COMPILER_PATH}")
                message(STATUS "Found dxil.dll: ${DX_IL_PATH}")
                
                install(FILES 
                    "${DX_COMPILER_PATH}"
                    "${DX_IL_PATH}"
                    DESTINATION bin
                    COMPONENT Runtime
                )
            else()
                message(WARNING "DirectX Shader Compiler DLLs not found for ${DX_ARCH} architecture")
                message(WARNING "  Expected: ${DX_COMPILER_PATH}")
                message(WARNING "  Expected: ${DX_IL_PATH}")
            endif()
        else()
            message(WARNING "Windows SDK not found at ${WINDOWS_KITS_DIR}/bin/10.0.*")
        endif()
    endif()
    
    # Install node.exe
    install(FILES "${CMAKE_SOURCE_DIR}/modules/webgui/ext/nodejs/node.exe"
        DESTINATION modules/webgui/ext/nodejs
        COMPONENT Runtime
        OPTIONAL
    )
endif ()

if (WIN32)
  set(CPACK_GENERATOR ZIP)
  # Need backslash for correct subdirectory paths
  set(CPACK_PACKAGE_ICON "${PROJECT_SOURCE_DIR}\\\\apps\\\\OpenSpace\\\\openspace.png")
  set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}\\\\${OPENSPACE_VERSION_NUMBER}")
else ()
  set(CPACK_GENERATOR TGZ)
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

include (CPack)
