#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2015                                                               #
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

set(APPLICATION_NAME Launcher)
set(APPLICATION_LINK_TO_OPENSPACE ON)

set(SOURCE_FILES
    ${OPENSPACE_APPS_DIR}/Launcher/main.cpp
    ${OPENSPACE_APPS_DIR}/Launcher/mainwindow.cpp
    ${OPENSPACE_APPS_DIR}/Launcher/shortcutwidget.cpp
    ${OPENSPACE_APPS_DIR}/Launcher/syncwidget.cpp
)

set(HEADER_FILES
    ${OPENSPACE_APPS_DIR}/Launcher/mainwindow.h
    ${OPENSPACE_APPS_DIR}/Launcher/shortcutwidget.h
    ${OPENSPACE_APPS_DIR}/Launcher/syncwidget.h
) 

find_package(Qt5Widgets)
find_package(Qt5Network)

qt5_wrap_cpp(MOC_FILES ${HEADER_FILES})
qt5_add_resources(RESOURCE_FILES ${OPENSPACE_APPS_DIR}/Launcher/files.qrc)

add_executable(${APPLICATION_NAME} MACOSX_BUNDLE
    ${SOURCE_FILES}
    ${HEADER_FILES}
    ${MOC_FILES}
    ${RESOURCE_FILES}
)

target_link_libraries(${APPLICATION_NAME}
    Qt5::Widgets
    Qt5::Network
)

if (APPLE)
INSTALL(CODE "
   include(BundleUtilities)
   fixup_bundle(\"/Users/alex/Development/OpenSpace/bin/openspace/Debug/Launcher.app/Contents/MacOS/Launcher\"   \"/Users/alex/Development/OpenSpace/bin/openspace/Debug/TimelineView.app/Contents/plugins/platforms/libqcocoa.dylib\"   \"\")
   " COMPONENT Runtime)
endif ()
