##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2023                                                                #
#                                                                                        #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#                                                                                        #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#                                                                                        #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

include(${PROJECT_SOURCE_DIR}/support/cmake/module_common.cmake)
include(${PROJECT_SOURCE_DIR}/support/cmake/set_openspace_compile_settings.cmake)

include(GenerateExportHeader)

# Creates a library for the module with name <module_name>. The name of the library is
# returned in <output_library_name> for outside configuration
# The library will have the name openspace-module-<name> and has all of their
# dependencies set correctly.
# The 'library_mode' determines whether the module is linked STATIC or SHARED
# Dependencies will have to be set in a file called "include.cmake"
function (create_new_module module_name output_library_name library_mode)
  # Create a library name of the style: openspace-module-${name}
  create_library_name(${module_name} library_name)

  # Add the module files to the list of sources
  get_module_files(${module_name} module_files)

  # Create the library
  add_library(${library_name} ${library_mode} ${module_files} ${ARGN})

  # Set compile settings that are common to all modules
  set_openspace_compile_settings(${library_name})

  target_include_directories(${library_name} PUBLIC ${PROJECT_SOURCE_DIR})
  create_define_name(${module_name} define_name)
  target_compile_definitions(${library_name} PUBLIC "${define_name}")

  handle_module_dependencies(${library_name} ${module_name})

  if ("${library_mode}" STREQUAL "SHARED")
    # If it is a shared library, we want to generate the export header
    string(TOLOWER ${module_name} lower_module_name)
    generate_export_header(
      ${library_name}
      EXPORT_FILE_NAME
      ${CMAKE_BINARY_DIR}/_generated/include/${lower_module_name}_export.h
    )
  endif ()

  # This is an ugly hack as we can't inject a variable into a scope two parents above
  # would love to: set(${module_class_name} "${module_name}Module" PARENT_PARENT_SCOPE)
  # instead
  # This value is used in handle_modules.cmake::handle_modules
  set_property(GLOBAL PROPERTY CurrentModuleClassName "${module_name}Module")

  set(${output_library_name} ${library_name} PARENT_SCOPE)
endfunction ()



# Gets and returns the <name>module.h and <name>module.cpp files and provides them with a
# source group
function (get_module_files module_name module_files)
  string(TOLOWER ${module_name} module_name_lower)
  set(module_files
    ${CMAKE_CURRENT_SOURCE_DIR}/${module_name_lower}module.h
    ${CMAKE_CURRENT_SOURCE_DIR}/${module_name_lower}module.cpp
    PARENT_SCOPE
  )
  source_group("Module Files" FILES ${module_files})
endfunction ()


# Loads the dependencies from 'include.cmake' and deals with them
# OpenSpace dependencies are added using target_link_libraries as the handle_modules
# function takes care that they are added to the project
# External dependencies are found using the find_package function and then linked
function (handle_module_dependencies target_name module_name)
  # We always want to link against Ghoul and the core library
  target_link_libraries(${library_name} PRIVATE Ghoul openspace-core)
  target_precompile_headers(${library_name} PRIVATE
    [["ghoul/format.h"]]
    [["ghoul/glm.h"]]
    [["ghoul/misc/assert.h"]]
    [["ghoul/misc/boolean.h"]]
    [["ghoul/misc/exception.h"]]
    [["ghoul/misc/invariants.h"]]
    [["ghoul/misc/profiling.h"]]
    [["ghoul/opengl/ghoul_gl.h"]]
    <array>
    <filesystem>
    <memory>
    <string>
    <string_view>
    <variant>
    <vector>
  )


  if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/include.cmake")
    include(${CMAKE_CURRENT_SOURCE_DIR}/include.cmake)

    # Handle OpenSpace dependencies
    foreach (dep ${OPENSPACE_DEPENDENCIES})
      create_library_name(${dep} dep_library)
      message(STATUS "Link: ${target_name} <- ${dep_library}")
      target_link_libraries(${target_name} PRIVATE ${dep_library})
    endforeach ()

    # Handle external dependencies
    foreach (dep ${EXTERNAL_DEPENDENCIES})
      string(TOUPPER ${dep} dep_upper)
      find_package(${dep} REQUIRED)
      target_include_directories(${target_name} PUBLIC
        ${${dep_upper}_INCLUDE_DIR} ${${dep_upper}_INCLUDE_DIRS}
      )
      message(STATUS "Link: ${target_name} <- ${${dep_upper}_LIBRARIES}")
      target_link_libraries(${target_name} PRIVATE ${${dep_upper}_LIBRARIES})
    endforeach ()
  endif ()
endfunction ()
