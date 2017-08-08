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


function (handle_applications)
    set(applications "")
    set(applications_link_to_openspace "")

    file(GLOB appDirs RELATIVE ${OPENSPACE_APPS_DIR} ${OPENSPACE_APPS_DIR}/*)
    list(REMOVE_ITEM appDirs ".DS_Store") # Removing the .DS_Store present on Mac

    set(DEFAULT_APPLICATIONS
        "OpenSpace"
        "Launcher"
    )
    mark_as_advanced(DEFAULT_APPLICATIONS)

    foreach (app ${appDirs})
        string(TOUPPER ${app} upper_app)
        list (FIND DEFAULT_APPLICATIONS "${app}" _index)
        if (${_index} GREATER -1)
            # App is a default application
            option(OPENSPACE_APPLICATION_${upper_app} "${app} Application" ON)
        else ()
            option(OPENSPACE_APPLICATION_${upper_app} "${app} Application" OFF)
        endif()
        if (OPENSPACE_APPLICATION_${upper_app})
            unset(APPLICATION_NAME)
            unset(APPLICATION_LINK_TO_OPENSPACE)
            include(${OPENSPACE_APPS_DIR}/${app}/CMakeLists.txt)
            set_openspace_compile_settings(${APPLICATION_NAME})

            if (APPLICATION_LINK_TO_OPENSPACE)
                    get_property(
                        OPENSPACE_INCLUDE_DIR
                        TARGET libOpenSpace
                        PROPERTY INTERFACE_INCLUDE_DIRECTORIES
                    )
                    target_include_directories(${APPLICATION_NAME} PUBLIC 
                        "${OPENSPACE_BASE_DIR}"
                        ${OPENSPACE_INCLUDE_DIR}
                    )

                    get_property(
                        OPENSPACE_DEFINES
                        TARGET libOpenSpace
                        PROPERTY INTERFACE_COMPILE_DEFINITIONS
                    )
                    target_compile_definitions(${APPLICATION_NAME} PUBLIC ${OPENSPACE_DEFINES})

                    target_link_libraries(${APPLICATION_NAME} Ghoul)
                    target_link_libraries(${APPLICATION_NAME} libOpenSpace)

                    if (MSVC)
                        set_target_properties(${APPLICATION_NAME} PROPERTIES LINK_FLAGS
                            "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
                        )
                    endif ()


                    if (WIN32)
                        ghl_copy_files(
                            ${APPLICATION_NAME}
                            "${CURL_ROOT_DIR}/lib/libcurl.dll"
                            "${CURL_ROOT_DIR}/lib/libeay32.dll"
                            "${CURL_ROOT_DIR}/lib/ssleay32.dll"

                        )
                        ghl_copy_shared_libraries(${APPLICATION_NAME} ${OPENSPACE_EXT_DIR}/ghoul)
                    endif ()
            endif ()

            list(APPEND applications ${APPLICATION_NAME})
            list(APPEND applications_link_to_openspace ${APPLICATION_LINK_TO_OPENSPACE})
            unset(APPLICATION_NAME)
            unset(APPLICATION_LINK_TO_OPENSPACE)
        endif ()
    endforeach ()


    # option(OPENSPACE_APPLICATION_OPENSPACE "Main OpenSpace Application" ON)
    # if (OPENSPACE_APPLICATION_OPENSPACE)
    #     include(${OPENSPACE_APPS_DIR}/OpenSpace/CMakeLists.txt)
    #     list(APPEND applications "OpenSpace")
    # endif ()
    set(OPENSPACE_APPLICATIONS ${applications} PARENT_SCOPE)
    set(OPENSPACE_APPLICATIONS_LINK_REQUEST ${applications_link_to_openspace} PARENT_SCOPE)

    message(STATUS "Applications:")
    list(LENGTH applications len1)
    math(EXPR len2 "${len1} - 1")

    foreach(val RANGE ${len2})
      list(GET applications ${val} val1)
      list(GET applications_link_to_openspace ${val} val2)
      message(STATUS "\t${val1} (Link: ${val2})")
    endforeach()
endfunction()