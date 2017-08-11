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

include(${OPENSPACE_CMAKE_EXT_DIR}/application_definition.cmake)
include(${GHOUL_BASE_DIR}/support/cmake/handle_external_library.cmake)

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/infowidget.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mainwindow.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/shortcutwidget.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/syncwidget.cpp
)

set(HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/infowidget.h
    ${CMAKE_CURRENT_SOURCE_DIR}/mainwindow.h
    ${CMAKE_CURRENT_SOURCE_DIR}/shortcutwidget.h
    ${CMAKE_CURRENT_SOURCE_DIR}/syncwidget.h
)

find_package(Qt5 COMPONENTS Core Widgets Network REQUIRED)

qt5_wrap_cpp(MOC_FILES ${HEADER_FILES})
qt5_add_resources(RESOURCE_FILES ${CMAKE_CURRENT_SOURCE_DIR}/files.qrc)

create_new_application(
    Launcher
    MACOSX_BUNDLE
    ${SOURCE_FILES}
    ${HEADER_FILES}
    ${OPENSPACE_APPS_DIR}/Launcher/openspace.rc
    ${MOC_FILES}
    ${RESOURCE_FILES}
)

target_link_libraries(
    Launcher
    libOpenSpace
    Qt5::Widgets
    Qt5::Network
)

#####
# Libtorrent
#####
include_external_library(
    Launcher
    libtorrent
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/libtorrent
)
target_include_directories(
    Launcher
    SYSTEM PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/libtorrent/include
)
