cmake_minimum_required(VERSION 2.8.11)

project(LuascriptExternalControl)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

#set(QT_PATH_OPTION "C:/Qt/5.1.0/5.1.0/msvc2012_64_opengl/lib" CACHE PATH "Path to Qt folder")
#set(QT_PATH_OPTION "C:\\Qt\\Qt5\\5.3\\msvc2013_64_opengl\\lib\\cmake\\Qt5Widgets")
#option(QT_PATH_OPTION "Path to the Qt5 folder" "C:/Qt/5.1.0/5.1.0/msvc2012_64_opengl/lib")
#message("Option: ${QT_PATH_OPTION}")

#set(CMAKE_PREFIX_PATH "${CMAKE_PREFIX_PATH} ${QT_PATH_OPTION}")

#set(CMAKE_PREFIX_PATH $ENV{QTDIR})

#message("Prefix: ${CMAKE_PREFIX_PATH}")

set(CMAKE_AUTOMOC ON)
find_package(Qt5Widgets)
find_package(Qt5Network)
add_executable(LuascriptExternalControl WIN32 main.cpp mainwindow.cpp)
#qt5_use_modules(LuascriptExternalControl, Widgets)
target_link_libraries(LuascriptExternalControl Qt5::Widgets Qt5::Network)