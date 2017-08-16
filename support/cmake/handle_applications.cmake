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
    # Get a list of all of the application directories
    file(GLOB appDirs RELATIVE ${OPENSPACE_APPS_DIR} ${OPENSPACE_APPS_DIR}/*)
    # Remove the .DS_Store present on Mac
    list(REMOVE_ITEM appDirs ".DS_Store")

    message(STATUS "Configuration application")
    foreach (app ${appDirs})
        set(app_dir "${OPENSPACE_APPS_DIR}/${app}")
        string(TOUPPER ${app} upper_app)

        option(OPENSPACE_APPLICATION_${upper_app} "${app} Application" OFF)

        get_application_attribute_default(${app_dir} is_default_application)
        if (${is_default_application})
            set(OPENSPACE_APPLICATION_${upper_app} ON CACHE BOOL "Build ${app} application" FORCE)
        endif ()

        if (OPENSPACE_APPLICATION_${upper_app})
            message(STATUS "Adding application ${app}")
            add_subdirectory(${app_dir})
        endif ()
    endforeach ()
endfunction()


# Returns whether the application located at 'path' is a default application or not
function (get_application_attribute_default path result)
    set(${result} OFF PARENT_SCOPE)
    if (EXISTS "${path}/include.cmake")
        unset(DEFAULT_APPLICATION)
        include(${path}/include.cmake)
        if (DEFINED DEFAULT_APPLICATION)
            set(${result} ${DEFAULT_APPLICATION} PARENT_SCOPE)
        endif ()
    endif ()
endfunction()
