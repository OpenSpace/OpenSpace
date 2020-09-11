cmake_minimum_required(VERSION 3.10)

project(profileGUIw LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

#set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
#set(CMAKE_AUTORCC ON)

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
  profileedit.h
  ui_editorwindow.h
  filesystemaccess.h
  launcherwindow.h
  ui_launcherwindow.h
)

set(SOURCE_FILES
  assettreeitem.cpp
  assettreemodel.cpp
  profileedit.cpp
  filesystemaccess.cpp
  launcherwindow.cpp
)

add_library(profileGUIw STATIC ${HEADER_FILES} ${SOURCE_FILES})
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/include)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/include)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/ext/glm)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/ext/cppformat/include)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/ext/lua/src)
target_link_libraries(profileGUIw PUBLIC Qt5::Widgets)

