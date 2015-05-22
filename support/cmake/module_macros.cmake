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

macro(first_case_upper retval in_value)
    string(TOLOWER ${in_value} value)
    string(SUBSTRING ${value} 0 1 first_letter)
    string(TOUPPER ${first_letter} first_letter)
    string(REGEX REPLACE "^.(.*)" "${first_letter}\\1" result "${value}")
    set(${retval} ${result})
endmacro()

macro(openspace_mod_name_to_dir retval)
    set(the_list "")
    foreach(item ${ARGN})
        string(REGEX MATCH "(^OpenSpace.*.Module$)" found_item ${item})
        if(found_item)
            string(REGEX REPLACE "(^OpenSpace)|(Module$)" "" new_item ${item})
            string(TOLOWER ${new_item} l_new_item)
            list(APPEND the_list ${l_new_item})
        endif()
    endforeach()
    set(${retval} ${the_list})
endmacro()

macro(openspace_dir_to_mod_prefix retval)
    set(the_list "")
    foreach(item ${ARGN})
        string(TOUPPER ${item} u_item)
        list(APPEND the_list OPENSPACE_MODULE_${u_item})
    endforeach()
    set(${retval} ${the_list})
endmacro()


macro(generate_unset_mod_options_and_depend_sort module_root_path retval)
    file(GLOB sub-dir RELATIVE ${module_root_path} ${module_root_path}/*)
    set(sorted_dirs ${sub-dir})
    foreach(dir ${sub-dir})
        if(IS_DIRECTORY ${module_root_path}/${dir})
            if(EXISTS "${module_root_path}/${dir}/depends.cmake")
                include(${module_root_path}/${dir}/depends.cmake)
                foreach(dependency ${dependencies})
                    list(FIND OPENSPACE_MODULE_PACKAGE_NAMES ${dependency} module_index)
                    if(NOT module_index EQUAL -1)
                       list(REMOVE_ITEM dependencies ${dependency})
                    endif()
                endforeach()
                openspace_mod_name_to_dir(depend_folders ${dependencies})
                list(APPEND depend_folders ${dir})
                list(FIND sorted_dirs ${dir} dir_index)
                list(INSERT sorted_dirs ${dir_index} ${depend_folders})
                list(REMOVE_DUPLICATES sorted_dirs)
            endif()
            openspace_dir_to_mod_prefix(mod_name ${dir})
            if(NOT DEFINED ${mod_name})
                first_case_upper(dir_name_cap ${dir})
                option(${mod_name} "Build ${dir_name_cap} Module" OFF)
            endif()
         else()
                list(REMOVE_ITEM sorted_dirs ${dir})
        endif()
    endforeach()
    set(${retval} ${sorted_dirs})
endmacro()

macro(resolve_module_dependencies module_root_path)   
    #Reverse list (as it is depend sorted) and go over dependencies one more time
    #If build is ON, then switch dependencies ON
    set(dir_list ${ARGN})
    list(REVERSE dir_list)
    foreach(dir ${dir_list})
        openspace_dir_to_mod_prefix(mod_name ${dir})
        if(${mod_name})
            if(EXISTS "${module_root_path}/${dir}/depends.cmake")
                include(${module_root_path}/${dir}/depends.cmake)
                openspace_mod_name_to_dir(depend_folders ${dependencies})
                foreach(depend_folder ${depend_folders})
                    openspace_dir_to_mod_prefix(depend_mod_name ${depend_folder})
                    first_case_upper(depend_name_cap ${depend_folder})
                    if(NOT ${depend_mod_name})
                        set(${depend_mod_name} ON CACHE BOOL "Build ${depend_name_cap} Module" FORCE)
                        message(STATUS "${depend_mod_name} was set to build, due to dependency towards ${mod_name}")
                    endif()
                endforeach()
            endif()
        endif()
    endforeach()
endmacro()