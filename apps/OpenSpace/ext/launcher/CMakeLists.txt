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
  include
  ${Qt5_DIR}/../../../include
  ${Qt5_DIR}/../../../include/QtWidgets
  ${Qt5_DIR}/../../../include/QtCore
  ${Qt5_DIR}/../../../include/QtGui
)

set(HEADER_FILES
  include/addedscripts.h
  include/assets.h
  include/assettreeitem.h
  include/assettreemodel.h
  include/camera.h
  include/deltatimes.h
  include/filesystemaccess.h
  include/keybindings.h
  include/launcherwindow.h
  include/marknodes.h
  include/meta.h
  include/osmodules.h
  include/ostime.h
  include/profileedit.h
  include/properties.h
  include/ui_addedscripts.h
  include/ui_assets.h
  include/ui_camera.h
  include/ui_deltatimes.h
  include/ui_keybindings.h
  include/ui_launcherwindow.h
  include/ui_marknodes.h
  include/ui_meta.h
  include/ui_osmodules.h
  include/ui_ostime.h
  include/ui_profileedit.h
  include/ui_properties.h
)

set(SOURCE_FILES
  src/addedscripts.cpp
  src/assets.cpp
  src/assettreeitem.cpp
  src/assettreemodel.cpp
  src/camera.cpp
  src/deltatimes.cpp
  src/filesystemaccess.cpp
  src/keybindings.cpp
  src/launcherwindow.cpp
  src/marknodes.cpp
  src/meta.cpp
  src/osmodules.cpp
  src/ostime.cpp
  src/profileedit.cpp
  src/properties.cpp
)

add_library(profileGUIw STATIC ${HEADER_FILES} ${SOURCE_FILES})
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/include)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/include)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/ext/glm)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/ext/cppformat/include)
target_include_directories(profileGUIw PRIVATE ${OPENSPACE_BASE_DIR}/ext/ghoul/ext/lua/src)
target_link_libraries(profileGUIw PUBLIC Qt5::Widgets)

