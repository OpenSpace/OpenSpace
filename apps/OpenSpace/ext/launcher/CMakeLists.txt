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

include(${PROJECT_SOURCE_DIR}/support/cmake/set_openspace_compile_settings.cmake)

set(HEADER_FILES
  include/filesystemaccess.h
  include/launcherwindow.h
  include/settingsdialog.h
  include/profile/actiondialog.h
  include/profile/additionalscriptsdialog.h
  include/profile/assetsdialog.h
  include/profile/assetedit.h
  include/profile/assettreeitem.h
  include/profile/assettreemodel.h
  include/profile/cameradialog.h
  include/profile/deltatimesdialog.h
  include/profile/horizonsdialog.h
  include/profile/scriptlogdialog.h
  include/profile/line.h
  include/profile/marknodesdialog.h
  include/profile/metadialog.h
  include/profile/modulesdialog.h
  include/profile/timedialog.h
  include/profile/profileedit.h
  include/profile/propertiesdialog.h
  include/sgctedit/displaywindowunion.h
  include/sgctedit/monitorbox.h
  include/sgctedit/orientationdialog.h
  include/sgctedit/settingswidget.h
  include/sgctedit/sgctedit.h
  include/sgctedit/windowcontrol.h
)

set(SOURCE_FILES
  src/launcherwindow.cpp
  src/filesystemaccess.cpp
  src/settingsdialog.cpp
  src/profile/actiondialog.cpp
  src/profile/additionalscriptsdialog.cpp
  src/profile/assetsdialog.cpp
  src/profile/assetedit.cpp
  src/profile/assettreeitem.cpp
  src/profile/assettreemodel.cpp
  src/profile/cameradialog.cpp
  src/profile/deltatimesdialog.cpp
  src/profile/horizonsdialog.cpp
  src/profile/scriptlogdialog.cpp
  src/profile/line.cpp
  src/profile/marknodesdialog.cpp
  src/profile/metadialog.cpp
  src/profile/modulesdialog.cpp
  src/profile/timedialog.cpp
  src/profile/profileedit.cpp
  src/profile/propertiesdialog.cpp
  src/sgctedit/sgctedit.cpp
  src/sgctedit/displaywindowunion.cpp
  src/sgctedit/monitorbox.cpp
  src/sgctedit/orientationdialog.cpp
  src/sgctedit/settingswidget.cpp
  src/sgctedit/windowcontrol.cpp
)

find_package(QT NAMES Qt6 Qt5 COMPONENTS Core Widgets Network REQUIRED)
set(MOC_FILES "")
set(RESOURCE_FILES "")
set(LIBRARIES "")

if (${QT_VERSION_MAJOR} EQUAL 5)
  find_package(Qt5 COMPONENTS Core Widgets Network)
  qt5_wrap_cpp(
    MOC_FILES
    ${HEADER_FILES}
  )
  qt5_add_resources(RESOURCE_FILES resources/resources.qrc)
  set(LIBRARIES )
elseif (${QT_VERSION_MAJOR} EQUAL 6)
  find_package(Qt6 COMPONENTS Core Widgets Network REQUIRED)

  qt6_wrap_cpp(
    MOC_FILES
    ${HEADER_FILES}
  )
  qt6_add_resources(RESOURCE_FILES resources/resources.qrc)
elseif (NOT DEFINED QT_VERSION_MAJOR)
  message(FATAL_ERROR "Unable to find Qt version")
else ()
  message(FATAL_ERROR "Unsupported Qt version")
endif()

add_library(openspace-ui-launcher STATIC ${HEADER_FILES} ${SOURCE_FILES} ${MOC_FILES} ${RESOURCE_FILES})
set_openspace_compile_settings(openspace-ui-launcher)
target_include_directories(
  openspace-ui-launcher
  PUBLIC
  include
)
target_link_libraries(
  openspace-ui-launcher
  PUBLIC
    openspace-core
    openspace-module-collection
    Qt${QT_VERSION_MAJOR}::Core
    Qt${QT_VERSION_MAJOR}::Gui
    Qt${QT_VERSION_MAJOR}::Widgets
    Qt${QT_VERSION_MAJOR}::Network
  PRIVATE
    sgct
)

target_precompile_headers(openspace-ui-launcher PRIVATE
  <openspace/scene/profile.h>
  <ghoul/glm.h>
  <QAbstractItemDelegate>
  <QAbstractItemModel>
  <QDialog>
  <QLineEdit>
  <QObject>
  <QStyleOption>
  <QTextCursor>
  <QWidget>
  <filesystem>
)

if (MSVC)
  set(MSVC_WARNINGS
    "/wd4619"   # #pragma warning: there is no warning number (raised by Qt headers)
    "/wd4946"   # reinterpret_cast used between related classes: 
  )
  target_compile_options(openspace-ui-launcher INTERFACE ${MSVC_WARNINGS})
endif ()
