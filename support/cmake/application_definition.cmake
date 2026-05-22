##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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

include(${PROJECT_SOURCE_DIR}/support/cmake/global_variables.cmake)

function (create_new_application application_name)
  add_executable(${application_name} MACOSX_BUNDLE ${ARGN})
  set_openspace_compile_settings(${application_name})
  if (WIN32)
    get_external_library_dependencies(ext_lib)
    # Register one global copy target so copy commands are defined exactly once
    if (NOT TARGET openspace_copy_external_dependencies)
      add_custom_target(openspace_copy_external_dependencies ALL)
      add_custom_command(
        TARGET openspace_copy_external_dependencies
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory "$<TARGET_FILE_DIR:${application_name}>"
      )
      foreach (file_i ${ext_lib})
        if (IS_DIRECTORY "${file_i}")
          get_filename_component(folder ${file_i} NAME)
          add_custom_command(
            TARGET openspace_copy_external_dependencies
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_directory "${file_i}" "$<TARGET_FILE_DIR:${application_name}>/${folder}"
          )
        else ()
          add_custom_command(
            TARGET openspace_copy_external_dependencies
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different "${file_i}" $<TARGET_FILE_DIR:${application_name}>
          )
        endif ()
      endforeach ()
    endif ()

    add_dependencies(${application_name} openspace_copy_external_dependencies)
  endif ()

  target_link_libraries(${application_name} PUBLIC openspace-module-base)
endfunction ()
