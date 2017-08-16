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

set(APPLICATION_NAME TimelineView)
set(APPLICATION_LINK_TO_OPENSPACE OFF)

set(SOURCE_FILES
    ${OPENSPACE_APPS_DIR}/TimelineView/main.cpp
    ${OPENSPACE_APPS_DIR}/TimelineView/mainwindow.cpp
    ${OPENSPACE_APPS_DIR}/TimelineView/configurationwidget.cpp
    ${OPENSPACE_APPS_DIR}/TimelineView/informationwidget.cpp
    ${OPENSPACE_APPS_DIR}/TimelineView/controlwidget.cpp
    ${OPENSPACE_APPS_DIR}/TimelineView/timelinewidget.cpp
)

set(HEADER_FILES
    ${OPENSPACE_APPS_DIR}/TimelineView/mainwindow.h
    ${OPENSPACE_APPS_DIR}/TimelineView/configurationwidget.h
    ${OPENSPACE_APPS_DIR}/TimelineView/informationwidget.h
    ${OPENSPACE_APPS_DIR}/TimelineView/controlwidget.h
    ${OPENSPACE_APPS_DIR}/TimelineView/timelinewidget.h
) 

find_package(Qt5Widgets)
find_package(Qt5Network)

set(MOC_FILES "")
qt5_wrap_cpp(MOC_FILES ${HEADER_FILES})

create_new_application(${APPLICATION_NAME} MACOSX_BUNDLE ${SOURCE_FILES} ${HEADER_FILES} ${MOC_FILES})

target_link_libraries(${APPLICATION_NAME}
    Qt5::Widgets
    Qt5::Network
)

#if (APPLE)
#    INSTALL(CODE "
#       include(BundleUtilities)
#       fixup_bundle(\"${application_path}/bin/openspace/Debug/Launcher.app/Contents/MacOS/Launcher\"   \"${application_path}/bin/openspace/Debug/TimelineView.app/Contents/plugins/platforms/libqcocoa.dylib\"   \"\")
#       " COMPONENT Runtime)
#endif ()
