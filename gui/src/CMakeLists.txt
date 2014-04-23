# FIXME: ugly hack for linking qt5
set (CMAKE_PREFIX_PATH "/home/hhellteg/Qt/5.2.1/gcc_64")

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed.
set(CMAKE_AUTOMOC ON)

# Find the Qt libraries
find_package(Qt5Widgets)
find_package(Qt5Network)

# Create ui_mainwindow.h file
qt5_wrap_ui(QT_UI_HEADERS mainwindow.ui)

# Tell CMake to create the executable
add_executable(gui main.cpp mainwindow.cpp ${QT_UI_HEADERS})

# Use the needed modules from Qt 5.
target_link_libraries(gui Qt5::Widgets Qt5::Network)