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
    #
    # Step 1:  Get a list of all modules
    #
    # First get all the internal module
    get_individual_modules(${internal_module_path} all_module_names all_module_paths)

    # Then get all external modules
    foreach (path ${external_modules_paths})
        get_individual_modules(${path} names paths)

        # @TODO: Check for duplicates here; modules in different ext folders with the same name ---abock
        set(all_module_names ${all_module_names} ${names})
        set(all_module_paths ${all_module_paths} ${paths})
    endforeach ()

    list(LENGTH all_module_names len1)
    math(EXPR len2 "${len1} - 1")

    # Debug:  List all modules
    # message(STATUS "All modules:")
    # foreach (val RANGE ${len2})
    #     list(GET all_module_names ${val} name)
    #     list(GET all_module_paths ${val} path)
    #     message(STATUS "\t${name}        (${path})")
    # endforeach ()

    #
    # Step 2:  Create options for all modules with correct default values
    #

    set(enabled_module_names "")
    set(enabled_module_paths "")
    foreach(val RANGE ${len2})
        list(GET all_module_names ${val} name)
        list(GET all_module_paths ${val} path)

        get_module_attribute_default(${path} is_default_module)
        create_option_name(${name} optionName)
        option(${optionName} "Build ${dir} Module" ${is_default_module})

        if (${optionName})
            list(APPEND enabled_module_names ${name})
            list(APPEND enabled_module_paths ${path})
        endif ()
    endforeach ()


    #
    # Step 3:  For each module that is default or enabled by the user, get the dependencies
    #
    set(dependencies "")
    list(LENGTH enabled_module_names len1)
    math(EXPR len2 "${len1} - 1")
    foreach (val RANGE ${len2})
        list(GET enabled_module_names ${val} name)
        list(GET enabled_module_paths ${val} path)

        get_recursive_dependencies(
            ${name} ${path}
            "${all_module_names}" "${all_module_paths}"
            deps
        )

        set(dependencies ${dependencies} ${deps})
    endforeach()
    # We can remove the duplicates here.  We constructed the list such that nested 
    # dependencies are order left to right.  REMOVE_DUPLICATES will keep the left most
    # value in the case of duplicates, so that will still work
    list(REMOVE_DUPLICATES dependencies)


    #
    # Step 4:  Check each dependency and set it, if necessary
    #
    foreach (dep ${dependencies})
        create_option_name(${dep} optionName)
        if (NOT ${optionName})
            set(${optionName} ON CACHE BOOL "Build ${dep} Module" FORCE)
            message(STATUS "Due to dependencies, the '${dep}' was enabled")

            find_path_for_module(${dep} "${all_module_names}" "${all_module_paths}" path)
            list(APPEND enabled_module_names ${dep})
            list(APPEND enabled_module_paths ${path})
        endif ()
    endforeach ()

    # Print all the enabled modules
    message(STATUS "Enabled modules:")
    list(LENGTH enabled_module_names len1)
    math(EXPR len2 "${len1} - 1")
    foreach (val RANGE ${len2})
        list(GET enabled_module_names ${val} name)
        list(GET enabled_module_paths ${val} path)

        message(STATUS "\t${name}    (${path})")
    endforeach ()

    #
    # Step 5:  Add the subdirectories of all the enabled modules
    #
    set(module_class_names "")
    set(module_external_libraries "")
    foreach (val RANGE ${len2})
        list(GET enabled_module_names ${val} name)
        list(GET enabled_module_paths ${val} path)

        create_library_name(${name} library_name)
        add_subdirectory(${path})

        # Only link libOpenSpace against the library if it has been set STATIC
        get_target_property(library_type ${library_name} TYPE)
        if (NOT ${library_type} STREQUAL "SHARED_LIBRARY")
            target_link_libraries(libOpenSpace ${library_name})
        endif()

        create_define_name(${name} define_name)
        target_compile_definitions(libOpenSpace PUBLIC "${define_name}")

        get_property(class_name GLOBAL PROPERTY CurrentModuleClassName)
        list(APPEND module_class_names ${class_name})

        get_property(ext_lib GLOBAL PROPERTY CurrentModuleExternalLibraries)
        list(APPEND module_external_libraries ${ext_lib})
    endforeach ()
    list(REMOVE_DUPLICATES module_external_libraries)


    #         list(LENGTH OPENSPACE_APPLICATIONS len1)
    #         math(EXPR len2 "${len1} - 1")

    #         foreach(val RANGE ${len2})
    #             list(GET OPENSPACE_APPLICATIONS ${val} val1)
    #             list(GET OPENSPACE_APPLICATIONS_LINK_REQUEST ${val} val2)
    #             if (${val2})
    #                 target_link_libraries(${app} ${libraryName})
    #             endif ()
    #         endforeach()

    #         if (EXTERNAL_LIBRARY)
    #             foreach (library ${EXTERNAL_LIBRARY})
    #                 get_filename_component(lib ${library} ABSOLUTE)
    #                 list(APPEND dll_list ${lib})
    #             endforeach()
    #         endif ()
    #     endif ()
    # endforeach ()

    #
    # Step 6:  Create the moduleregistration.h file
    #
    set(MODULE_HEADERS "")
    set(MODULE_CLASSES "")
    foreach (val RANGE ${len2})
        list(GET enabled_module_names ${val} name)
        list(GET enabled_module_paths ${val} path)
        list(GET module_class_names ${val} class_name)

        string(TOUPPER ${name} module_upper)
        string(TOLOWER ${name} module_lower)

        create_module_header_filepath(${name} ${path} header_filepath)

        list(APPEND MODULE_HEADERS "#include <${header_filepath}>\n")
        list(APPEND MODULE_CLASSES "        new ${class_name},\n")
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


    # @TODO Add external paths for use in modulePath() function

    #
    # Step 7:  On Windows, copy the DLLs into the application folders
    #
    if (WIN32)
        message(STATUS "Copying DLLs:")
        foreach (dll ${module_external_libraries})
            message(STATUS "\t${dll}")
            foreach (application ${OPENSPACE_APPLICATIONS})
                ghl_copy_files(${application} ${dll})
            endforeach ()
        endforeach ()
    endif ()
endfunction()


# Returns whether the module located at 'path' is a default module or not
function(get_module_attribute_default path result)
    set(${result} OFF PARENT_SCOPE)
    if (EXISTS "${path}/include.cmake")
        unset(DEFAULT_MODULE)
        include(${path}/include.cmake)
        if (DEFINED DEFAULT_MODULE)
            set(${result} ${DEFAULT_MODULE} PARENT_SCOPE)
        endif ()
    endif ()
endfunction()


# Returns the list of dependencies of the module located at 'path'
function(get_module_attribute_dependencies path result)
    set(${result} "" PARENT_SCOPE)
    if (EXISTS "${path}/include.cmake")
        unset(OPENSPACE_DEPENDENCIES)
        include(${path}/${dir}/include.cmake)
        if (DEFINED OPENSPACE_DEPENDENCIES)
            set(${result} ${OPENSPACE_DEPENDENCIES} PARENT_SCOPE)
        endif ()
    endif ()
endfunction()


# Returns the path for the 'module_name'. If the module has not been seen before by 
# get_individual_modules, an empty string is returned
function(find_path_for_module module_name module_names module_paths result)
    list(FIND module_names ${module_name} i)
    if (i EQUAL -1)
        # Did not find the name in the list
        set(${result} "" PARENT_SCOPE)
    else ()
        # We found the name in the list, so the path is at the same location
        list(GET module_paths ${i} path)
        set(${result} ${path} PARENT_SCOPE)
    endif ()
endfunction ()


# Gets the names of the dependencies of 'module_name' recursively and returns them in
# 'result'.  For a dependency chain of a -> b -> c   get_recursive_depdencies(a) will
# return "b,c"
function(get_recursive_dependencies module_name module_path module_names module_paths result)
    set(result_aggregate "")
    get_module_attribute_dependencies(${module_path} deps)
    if (deps)
        foreach (name ${deps})
            find_path_for_module(${name} "${module_names}" "${module_paths}" path)
            if (path)
                get_recursive_dependencies(
                    ${name} ${path}
                    "${module_names}" "${module_paths}"
                    res
                )
                # 1. We add "base" to the list of dependencies as we always want it 
                # 2. We add dependencies in this order such that when we later traverse
                #    this list, we automatically get them in the correct order (meaning
                #    that we include a dependency first)
                set(result_aggregate ${result_aggregate} "base" ${res} ${name})
            else ()
                message(FATAL_ERROR "Could not find dependency ${name} for module ${module_name}")
            endif ()
        endforeach ()
    endif ()
    set(${result} ${result_aggregate} PARENT_SCOPE)
endfunction()


# Returns a list of all modules contained in the folder 'path'
function(get_individual_modules path module_names module_paths)
    file(GLOB moduleDirs RELATIVE ${path} ${path}/*)

    set(names "")
    set(paths "")
    foreach (dir ${moduleDirs})
        if (EXISTS "${path}/${dir}/CMakeLists.txt")
            list(APPEND names ${dir})
            list(APPEND paths ${path}/${dir})
        else ()
            message(STATUS "Skipping ${dir} as ${dir}/CMakeLists.txt does not exist")
        endif ()
    endforeach ()

    set(${module_names} ${names} PARENT_SCOPE)
    set(${module_paths} ${paths} PARENT_SCOPE)
endfunction ()


function (handle_individual_module_old path)
    # Get all modules in the correct order based on their dependencies
    file(GLOB moduleDirs RELATIVE ${path} ${path}/*)
    set(sortedModules ${moduleDirs})
    foreach (dir ${moduleDirs})
        if (IS_DIRECTORY ${path}/${dir})
            set(defaultModule OFF)
            if (EXISTS "${path}/${dir}/include.cmake")
                unset(OPENSPACE_DEPENDENCIES)
                unset(EXTERNAL_LIBRARY)
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
            if (NOT ${libType} STREQUAL "SHARED_LIBRARY")
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

            if (EXTERNAL_LIBRARY)
                foreach (library ${EXTERNAL_LIBRARY})
                    get_filename_component(lib ${library} ABSOLUTE)
                    list(APPEND dll_list ${lib})
                endforeach()
            endif ()


    list(REMOVE_DUPLICATES dll_list)

    if (WIN32)
    foreach (application ${OPENSPACE_APPLICATIONS})
        foreach (dll ${dll_list})
            ghl_copy_files(${application} ${dll})
        endforeach ()
    endforeach ()
    endif ()
endfunction ()
