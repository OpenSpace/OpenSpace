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

# TODO:
# 1. Get a recursive list of all modules and store in list
# 2. Create a list of default modules + manually selected modules
# 3. Go through list and select dependencies
# 4. add_subdirectory selected dependencies
# 5. Create module_registration.h file


function (handle_modules internal_module_path external_modules_paths)
    # Remove all of the module information files that were created the last time
    file(REMOVE_RECURSE "${CMAKE_BINARY_DIR}/_generated/modules")

    # First handle the internal module
    handle_individual_module(${internal_module_path})

    # Then handle all the external modules)
    foreach (path ${external_modules_paths})
        message(STATUS "Adding modules from external path ${path}")

        # Get the parent directory
        get_filename_component(parent "${path}/.." ABSOLUTE)
        get_filename_component(parent "${parent}" NAME)

        message(STATUS "Registering ${parent} as an include path")
        target_include_directories(libOpenSpace PUBLIC ${parent})

        handle_individual_module(${path})
    endforeach ()
endfunction ()

function (handle_individual_module path)
    # Get all modules in the correct order based on their dependencies
    file(GLOB moduleDirs RELATIVE ${path} ${path}/*)
    set(sortedModules ${moduleDirs})
    foreach (dir ${moduleDirs})
        if (IS_DIRECTORY ${path}/${dir})
            set(defaultModule OFF)
            if (EXISTS "${path}/${dir}/include.cmake")
                unset(OPENSPACE_DEPENDENCIES)
                unset(EXTERNAL_LIBRAY)
                unset(DEFAULT_MODULE)
                include(${path}/${dir}/include.cmake)

                if (DEFINED DEFAULT_MODULE)
                    set(defaultModule ${DEFAULT_MODULE})
                endif ()
                if (OPENSPACE_DEPENDENCIES)
                    foreach (dependency ${OPENSPACE_DEPENDENCIES})
                        create_library_name(${dependency} library)
                        if (TARGET ${library})
                            # already registered
                            list(REMOVE_ITEM OPENSPACE_DEPENDENCIES ${dependency})
                        endif ()
                    endforeach ()

                    list(APPEND OPENSPACE_DEPENDENCIES ${dir})
                    list(FIND sortedModules ${dir} dir_index)
                    # if (NOT dir STREQUAL "base")
                    #     list(INSERT OPENSPACE_DEPENDENCIES 0 "base")
                    # endif ()
                    list(INSERT sortedModules ${dir_index} ${OPENSPACE_DEPENDENCIES})
                    list(REMOVE_DUPLICATES sortedModules)
                endif ()
            endif ()
            create_option_name(${dir} optionName)
            option(${optionName} "Build ${dir} Module" ${defaultModule})
            # create_library_name(${module} ${library})
        else ()
            list(REMOVE_ITEM sortedModules ${dir})
        endif ()
    endforeach ()

    # Automatically set dependent modules to ON
    set(dir_list ${sortedModules})
    set(dll_list "")
    list(REVERSE dir_list)
    foreach (dir ${dir_list})
        create_option_name(${dir} optionName)
        if (${optionName})
            if (EXISTS "${path}/${dir}/include.cmake")
                unset(OPENSPACE_DEPENDENCIES)
                unset(EXTERNAL_LIBRAY)
                unset(DEFAULT_MODULE)
                include(${path}/${dir}/include.cmake)

                if (OPENSPACE_DEPENDENCIES)
                    foreach (dependency ${OPENSPACE_DEPENDENCIES})
                        create_option_name(${dependency} dependencyOptionName)
                        if (NOT ${dependencyOptionName})
                            set(${dependencyOptionName} ON CACHE BOOL "ff" FORCE)
                            message(STATUS "${dependencyOptionName} was set to build, due to dependency towards ${optionName}")
                        endif ()
                    endforeach ()
                endif ()
            endif ()
        endif ()
    endforeach ()

    set(MODULE_HEADERS "")
    set(MODULE_CLASSES "")

    message(STATUS "Included modules:")
    foreach (module ${sortedModules})
        create_option_name(${module} optionName)
        if (${optionName})
            message(STATUS "\t${module}")
        endif ()
    endforeach ()

    # Add subdirectories in the correct order
    foreach (module ${sortedModules})
        create_option_name(${module} optionName)
        if (${optionName})
            create_library_name(${module} libraryName)
            add_subdirectory(${path}/${module})

            list(LENGTH OPENSPACE_APPLICATIONS len1)
            math(EXPR len2 "${len1} - 1")

            foreach(val RANGE ${len2})
                list(GET OPENSPACE_APPLICATIONS ${val} val1)
                list(GET OPENSPACE_APPLICATIONS_LINK_REQUEST ${val} val2)
                if (${val2})
                    target_link_libraries(${app} ${libraryName})
                endif ()
            endforeach()

            # Only link libOpenSpace against the library if it has been set STATIC
            get_target_property(libType ${libraryName} TYPE)
            if (NOT ${libType} STREQUAL  "SHARED_LIBRARY")
                target_link_libraries(libOpenSpace ${libraryName})
            endif()

            create_define_name(${module} defineName)
            target_compile_definitions(libOpenSpace PUBLIC "${defineName}")

            # Create registration file
            string(TOUPPER ${module} module_upper)
            string(TOLOWER ${module} module_lower)
            unset(MODULE_NAME)
            unset(MODULE_PATH)
            unset(MODULE_HEADER_PATH)
            include(${CMAKE_BINARY_DIR}/_generated/modules/${module_lower}.cmake)

            list(APPEND MODULE_HEADERS "#include <${MODULE_HEADER_PATH}>\n")
            list(APPEND MODULE_CLASSES "        new ${MODULE_NAME},\n")

            if (EXTERNAL_LIBRARY)
                foreach (library ${EXTERNAL_LIBRARY})
                    get_filename_component(lib ${library} ABSOLUTE)
                    list(APPEND dll_list ${lib})
                endforeach()
            endif ()
        endif ()
    endforeach ()

    if (NOT "${MODULE_HEADERS}" STREQUAL "")
        string(REPLACE ";" "" MODULE_HEADERS ${MODULE_HEADERS})
    endif ()

    if (NOT "${MODULE_CLASSES}" STREQUAL "")
        string(REPLACE ";" "" MODULE_CLASSES ${MODULE_CLASSES})
    endif ()

    configure_file(
        ${OPENSPACE_CMAKE_EXT_DIR}/module_registration.template
        ${CMAKE_BINARY_DIR}/_generated/include/openspace/moduleregistration.h
    )

    list(REMOVE_DUPLICATES dll_list)

    if (WIN32)
    foreach (application ${OPENSPACE_APPLICATIONS})
        foreach (dll ${dll_list})
            ghl_copy_files(${application} ${dll})
        endforeach ()
    endforeach ()
    endif ()
endfunction ()
