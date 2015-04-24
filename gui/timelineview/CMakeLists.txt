cmake_minimum_required(VERSION 2.8.11)

project(TimelineView)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOMOC ON)
find_package(Qt5Widgets)
find_package(Qt5Network)
add_executable(TimelineView main.cpp mainwindow.cpp configurationwidget.cpp informationwidget.cpp timecontrolwidget.cpp timelinewidget.cpp)
target_link_libraries(TimelineView Qt5::Widgets Qt5::Network)