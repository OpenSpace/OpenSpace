##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2018                                                                #
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

function (handle_modules internal_module_path external_modules_paths)
    #
    # Step 1:  Get a list of all modules
    #
    # First get all the internal module
    get_individual_modules(${internal_module_path} all_module_names all_module_paths)

    # Then get all external modules
    foreach (path ${external_modules_paths})
        get_individual_modules(${path} names paths)

        foreach (n ${names})
            if (${n} IN_LIST all_module_names)
                message(FATAL_ERROR "Module name ${n} is not unique among the external directories")
            endif ()
        endforeach ()

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

        get_module_attribute_supported(${path} is_module_supported)
        if (NOT ${is_module_supported})
            message(STATUS "Skipping module ${path} as it is not supported on this machine")
            continue()
        endif ()

        get_module_attribute_default(${path} is_default_module)
        create_option_name(${name} optionName)
        option(${optionName} "Build ${path} Module" ${is_default_module})

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
    add_external_library_dependencies("${module_external_libraries}")

    #
    # Step 6:  Create the moduleregistration.h file with the header file paths, class
    #          names and the external module paths
    #
    set(MODULE_HEADERS "")
    set(MODULE_CLASSES "")
    set(MODULE_PATHS "")
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

    get_unique_include_paths(
        "${enabled_module_paths}"
        "${internal_module_path}"
        module_paths
    )

    foreach (path ${module_paths})
        list(APPEND MODULE_PATHS "    \"${path}\",\n")
        # The module path should include the 'modules' directory, which is removed for the
        # include path to make all of the includes look the same
        list(APPEND MODULE_PATHS "    \"${path}/modules\",\n")
        target_include_directories(libOpenSpace PUBLIC ${path})
    endforeach ()

    if (NOT "${MODULE_HEADERS}" STREQUAL "")
        string(REPLACE ";" "" MODULE_HEADERS ${MODULE_HEADERS})
    endif ()

    if (NOT "${MODULE_CLASSES}" STREQUAL "")
        string(REPLACE ";" "" MODULE_CLASSES ${MODULE_CLASSES})
    endif ()

    if (NOT "${MODULE_PATHS}" STREQUAL "")
        string(REPLACE ";" "" MODULE_PATHS ${MODULE_PATHS})
        string(REPLACE "\\" "/" MODULE_PATHS ${MODULE_PATHS})
    endif ()

    configure_file(
        ${OPENSPACE_CMAKE_EXT_DIR}/module_registration.template
        ${CMAKE_BINARY_DIR}/_generated/include/openspace/moduleregistration.h
    )

    configure_file(
        ${OPENSPACE_CMAKE_EXT_DIR}/module_path.template
        ${CMAKE_BINARY_DIR}/_generated/include/openspace/modulepath.h
    )
endfunction()


# This function takes a list of module paths and returns the list of include paths that
# need to be added to the moduleregistration file. Each module files parent directory is
# considered, but with the internal module path ignored
function (get_unique_include_paths module_paths internal_module_path result)
    set(res "")
    foreach (p ${all_module_paths})
        get_filename_component(base_dir_parent "${p}/.." ABSOLUTE)
        get_filename_component(base_dir_grandparent "${p}/../.." ABSOLUTE)
        get_filename_component(int_dir "${internal_module_path}/.." ABSOLUTE)
        if (NOT ${base_dir_grandparent} STREQUAL ${int_dir})
            # We need to add both parent and grantparent directory:
            # The grandparent has to be added if we have an external module in the path
            # x/modules/name and we need to add 'x' to the module search path such that
            # people can write #include <modules/name/namemodule.h>
            #
            # The parent needs to be included for modules in subdirectories of the form
            # openspace/modules/dir/name such that 'dir' is included in the search path
            list(APPEND res ${base_dir_parent})
            list(APPEND res ${base_dir_grandparent})
        endif ()
    endforeach ()

    list(REMOVE_DUPLICATES res)
    set(${result} ${res} PARENT_SCOPE)
endfunction ()


# Returns whether the module located at 'path' is a default module or not
function (get_module_attribute_default path result)
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
function (get_module_attribute_dependencies path result)
    set(${result} "" PARENT_SCOPE)
    if (EXISTS "${path}/include.cmake")
        unset(OPENSPACE_DEPENDENCIES)
        include(${path}/${dir}/include.cmake)
        if (DEFINED OPENSPACE_DEPENDENCIES)
            set(${result} ${OPENSPACE_DEPENDENCIES} PARENT_SCOPE)
        endif ()
    endif ()
endfunction()

# Returns the state whether the module located at 'path' is supported on this machine
function (get_module_attribute_supported path result)
    set(${result} ON PARENT_SCOPE)
    if (EXISTS "${path}/include.cmake")
        unset(SUPPORTED)
        include(${path}/${dir}/include.cmake)
        if (DEFINED SUPPORTED)
            set(${result} ${SUPPORTED} PARENT_SCOPE)
        endif ()
    endif ()
endfunction()


# Returns the path for the 'module_name'. If the module has not been seen before by 
# get_individual_modules, an empty string is returned
function (find_path_for_module module_name module_names module_paths result)
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
function (get_recursive_dependencies module_name module_path module_names module_paths result)
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
function (get_individual_modules path module_names module_paths)
    file(GLOB moduleDirs RELATIVE ${path} ${path}/*)

    set(names "")
    set(paths "")
    foreach (dir ${moduleDirs})
        if (EXISTS "${path}/${dir}/CMakeLists.txt")
            list(APPEND names ${dir})
            list(APPEND paths ${path}/${dir})
        else ()
            # The CMakeLists.txt does not exist, so it might be a group of subdirectories
            get_individual_modules(${path}/${dir} _mod_names _mod_paths)
            list(APPEND names ${_mod_names})
            list(APPEND paths ${_mod_paths})
            # message(STATUS "Skipping ${dir} as ${dir}/CMakeLists.txt does not exist")
        endif ()
    endforeach ()

    set(${module_names} ${names} PARENT_SCOPE)
    set(${module_paths} ${paths} PARENT_SCOPE)
endfunction ()
