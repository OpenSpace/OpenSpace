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

include (${OPENSPACE_CMAKE_EXT_DIR}/handle_external_library.cmake)

set(application_path ${OPENSPACE_APPS_DIR}/Launcher)

set(SOURCE_FILES
    ${application_path}/main.cpp
    ${application_path}/infowidget.cpp
    ${application_path}/mainwindow.cpp
    ${application_path}/shortcutwidget.cpp
    ${application_path}/syncwidget.cpp
)

set(HEADER_FILES
    ${application_path}/infowidget.h
    ${application_path}/mainwindow.h
    ${application_path}/shortcutwidget.h
    ${application_path}/syncwidget.h
) 

find_package(Qt5Widgets)
find_package(Qt5Network)

qt5_wrap_cpp(MOC_FILES ${HEADER_FILES})
qt5_add_resources(RESOURCE_FILES ${application_path}/files.qrc)

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

# Libtorrent
include_external_library(${APPLICATION_NAME} libtorrent ${application_path}/ext/libtorrent)
target_include_directories(${APPLICATION_NAME} PUBLIC SYSTEM ${application_path}/ext/libtorrent/include)
