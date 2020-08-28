cmake_minimum_required(VERSION 3.10)

project(profileGUIw LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt5 COMPONENTS Widgets REQUIRED)
target_include_directories(
  OpenSpace PRIVATE
  ${Qt5_DIR}/../../../include
  ${Qt5_DIR}/../../../include/QtWidgets
  ${Qt5_DIR}/../../../include/QtCore
  ${Qt5_DIR}/../../../include/QtGui
)

set(HEADER_FILES
  assettreeitem.h
  assettreemodel.h
  editorwindow.h
  ui_editorwindow.h
  filesystemaccess.h
  launcherwindow.h
  ui_launcherwindow.h
)

set(SOURCE_FILES
  assettreeitem.cpp
  assettreemodel.cpp
  editorwindow.cpp
  filesystemaccess.cpp
  launcherwindow.cpp
)

add_library(profileGUIw STATIC ${HEADER_FILES} ${SOURCE_FILES})
target_link_libraries(profileGUIw PUBLIC Qt5::Widgets)

