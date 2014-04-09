cmake_minimum_required(VERSION 2.8.11)

project(GUI)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${GUI_SOURCE_DIR}/build-debug)

set(CMAKE_BUILD_TYPE Debug)

add_subdirectory(src)