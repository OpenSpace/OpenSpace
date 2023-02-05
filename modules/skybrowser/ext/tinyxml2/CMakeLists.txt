add_library(tinyxml2-skybrowser STATIC tinyxml2.h tinyxml2.cpp)
target_include_directories(tinyxml2-skybrowser PUBLIC ".")
target_compile_options(tinyxml2-skybrowser PRIVATE "-w")
