#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2015                                                               #
#                                                                                       #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this  #
# software and associated documentation files (the "Software"), to deal in the Software #
# without restriction, including without limitation the rights to use, copy, modify,    #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    #
# permit persons to whom the Software is furnished to do so, subject to the following   #
# conditions:                                                                           #
#                                                                                       #
# The above copyright notice and this permission notice shall be included in all copies #
# or substantial portions of the Software.                                              #
#                                                                                       #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         #
#########################################################################################

include (${OPENSPACE_CMAKE_EXT_DIR}/module_common.cmake)

function (create_new_module
        module_name
                        # Sources
    )
    set(sources ${ARGN})
    project(${module_name})
    # Create a library name of the style: openspace-module-${name}
    create_library_name(${module_name} library_name)

    message(STATUS "Configuring module ${MODULE_NAME}: ${library_name}")

    add_module_files()

    add_library(${library_name} ${sources})
    set_common_compile_settings(${library_name})
    set_openspace_includes(${library_name})

    handle_dependencies(${library_name} ${module_name})

    write_module_name(${module_name})
endfunction ()

# I couldn't make adding the module files to the ${sources} variable to work with a function ---abock
# Adds the <name>module.h and <name>module.cpp files to the list of sources and provides
# Them with a source group
macro (add_module_files)
    string(TOLOWER ${module_name} module_name_lower)
    set(module_files
        ${CMAKE_CURRENT_SOURCE_DIR}/${module_name_lower}module.h
        ${CMAKE_CURRENT_SOURCE_DIR}/${module_name_lower}module.cpp
    )
    source_group("Module Files" FILES ${module_files})
    list(APPEND sources ${module_files})
endmacro ()

# Set the compiler settings that are common to all modules
function (set_common_compile_settings target_name)
    if (MSVC)
        target_compile_options(${library_name} PUBLIC
            "/MP"       # Enabling multi-threaded compilation
            "/wd4100"   # Unreferenced formal parameter [too frequent in external libs]
            "/wd4127"   # constant conditional expression [used for do/while semicolon swallowing]
            "/wd4201"   # nameless struct/union  [standard is ubiquitous]
            "/wd4505"   # Unreferenced function was removed
            "/W4"
        )
        if (OPENSPACE_WARNINGS_AS_ERRORS)
            target_compile_options(${library_name} PUBLIC "/Wx")
        endif ()
    endif ()    
endfunction ()

# Propagate the include directives from the libOpenSpace target into this module
function (set_openspace_includes target_name)
    get_property(
        OPENSPACE_INCLUDE_DIR
        TARGET libOpenSpace
        PROPERTY INTERFACE_INCLUDE_DIRECTORIES
    )
    target_include_directories(${target_name} PUBLIC 
        "${OPENSPACE_BASE_DIR}"
        ${OPENSPACE_INCLUDE_DIR}
    )

    get_property(
        OPENSPACE_DEFINES
        TARGET libOpenSpace
        PROPERTY INTERFACE_COMPILE_DEFINITIONS
    )
    target_compile_definitions(${target_name} PUBLIC ${OPENSPACE_DEFINES})

    target_link_libraries(${target_name} libOpenSpace)
endfunction ()

# Loads the dependencies from 'include.cmake' and deals with them
function (handle_dependencies target_name module_name)
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/include.cmake")
        include(${CMAKE_CURRENT_SOURCE_DIR}/include.cmake)

        # if (NOT ${module_name} STREQUAL "Base")
        #     list(APPEND OPENSPACE_DEPENDENCIES "base")
        # endif ()

        # Handle OpenSpace dependencies
        foreach (dep ${OPENSPACE_DEPENDENCIES})
            create_library_name(${dep} dep_library)
            target_link_libraries(${target_name} ${dep_library})

            get_property(
                DEP_INCLUDE_DIR
                TARGET ${dep_library}
                PROPERTY INTERFACE_INCLUDE_DIRECTORIES
            )
            target_include_directories(${target_name} PUBLIC ${DEP_INCLUDE_DIR})
        endforeach ()

        # Handle extenal dependencies
        foreach (dep ${EXTERNAL_DEPENDENCIES})
            string(TOUPPER ${dep} dep_upper)
            find_package(${dep} REQUIRED)
            target_include_directories(${target_name} PUBLIC
                ${${dep_upper}_INCLUDE_DIR} ${${dep_upper}_INCLUDE_DIRS}
            )
            target_link_libraries(${target_name}  ${${dep_upper}_LIBRARIES})
        endforeach ()
    endif ()
endfunction ()

function (write_module_name module_name)
    string(TOLOWER ${module_name} module_name_lower)

    file(WRITE ${CMAKE_BINARY_DIR}/modules/${module_name}/modulename.cmake
        "set(MODULE_NAME ${module_name}Module)\n"
        "set(MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/${module_name_lower}module.h)"
    )
endfunction ()