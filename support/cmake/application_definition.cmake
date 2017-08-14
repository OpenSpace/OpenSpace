##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2017                                                                #
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

include(${OPENSPACE_CMAKE_EXT_DIR}/global_variables.cmake)

function (create_new_application application_name)
    message(STATUS "Configuring application ${application_name}")

    add_executable(${application_name} ${ARGN})
    set_openspace_compile_settings(${application_name})

    if (WIN32)
        get_external_library_dependencies(ext_lib)
        ghl_copy_files(
            ${application_name}
            "${CURL_ROOT_DIR}/lib/libcurl.dll"
            "${CURL_ROOT_DIR}/lib/libeay32.dll"
            "${CURL_ROOT_DIR}/lib/ssleay32.dll"
            ${ext_lib}
        )
        ghl_copy_shared_libraries(${application_name} ${OPENSPACE_EXT_DIR}/ghoul)
    endif ()
endfunction ()
