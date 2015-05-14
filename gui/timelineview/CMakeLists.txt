cmake_minimum_required(VERSION 2.8.11)

project(TimelineView)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOMOC ON)
find_package(Qt5Widgets)
find_package(Qt5Network)
if (APPLE)
add_executable(TimelineView MACOSX_BUNDLE main.cpp mainwindow.cpp configurationwidget.cpp informationwidget.cpp controlwidget.cpp timelinewidget.cpp)
else (APPLE)
add_executable(TimelineView main.cpp mainwindow.cpp configurationwidget.cpp informationwidget.cpp controlwidget.cpp timelinewidget.cpp)
endif ()

target_link_libraries(TimelineView Qt5::Widgets Qt5::Network)

if (APPLE)
INSTALL(CODE "
   include(BundleUtilities)
   fixup_bundle(\"/Users/alex/Development/OpenSpace/bin/openspace/Debug/TimelineView.app/Contents/MacOS/TimelineView\"   \"/Users/alex/Development/OpenSpace/bin/openspace/Debug/TimelineView.app/Contents/plugins/platforms/libqcocoa.dylib\"   \"\")
   " COMPONENT Runtime)
endif ()
