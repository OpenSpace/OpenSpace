cmake_minimum_required(VERSION 2.8.11)

project(LuascriptExternalControl)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOMOC ON)
find_package(Qt5Widgets)
find_package(Qt5Network)
add_executable(LuascriptExternalControl WIN32 main.cpp mainwindow.cpp)
target_link_libraries(LuascriptExternalControl Qt5::Widgets Qt5::Network)