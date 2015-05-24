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

function (test_compiler_compatibility)
    if (MSVC)
        if (MSVC_VERSION LESS 1800)
            message(FATAL_ERROR "OpenSpace requires at least Visual Studio 2013")
        endif ()
    endif ()
endfunction ()



macro (cleanup_project)
    # Remove MinSizeRel build option
    set(CMAKE_CONFIGURATION_TYPES Debug Release RelWithDebInfo CACHE TYPE INTERNAL FORCE)
    mark_as_advanced(CMAKE_CONFIGURATION_TYPES)
    mark_as_advanced(CMAKE_INSTALL_PREFIX)

    set_property(GLOBAL PROPERTY USE_FOLDERS On)
    set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER CMake)
endmacro ()


macro (set_build_output_directories)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${OPENSPACE_CMAKE_EXT_DIR})
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${OPENSPACE_BASE_DIR}/bin/lib)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${OPENSPACE_BASE_DIR}/bin/lib)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OPENSPACE_BASE_DIR}/bin/openspace)
endmacro ()



function (configure_openspace_version major_version minor_version patch_version string_version)
    set(OPENSPACE_MAJOR_VERSION ${major_version})
    set(OPENSPACE_MINOR_VERSION ${minor_version})
    set(OPENSPACE_PATCH_VERSION ${patch_version})
    set(OPENSPACE_VERSION_STRING ${string_version})
    message(STATUS "Version: ${OPENSPACE_MAJOR_VERSION}.${OPENSPACE_MINOR_VERSION}.${OPENSPACE_PATCH_VERSION} (${OPENSPACE_VERSION_STRING})")
    mark_as_advanced(OPENSPACE_MAJOR_VERSION, OPENSPACE_MINOR_VERSION, OPENSPACE_PATCH_VERSION, OPENSPACE_VERSION_STRING)
    configure_file(${OPENSPACE_CMAKE_EXT_DIR}/version.template ${CMAKE_BINARY_DIR}/_generated/include/openspace/version.h)
endfunction ()



function (create_openspace_targets)
    add_library(libOpenSpace STATIC ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
    target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_BASE_DIR}/include)
    target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_BASE_DIR})
    target_include_directories(libOpenSpace PUBLIC ${CMAKE_BINARY_DIR}/_generated/include)

    add_executable(OpenSpace ${OPENSPACE_MAIN})
    target_include_directories(OpenSpace PUBLIC ${OPENSPACE_BASE_DIR}/include)
    target_link_libraries(OpenSpace libOpenSpace)
endfunction ()



function (set_compile_settings)
    if (MSVC)
        target_compile_options(libOpenSpace PUBLIC "/MP" "/wd4201" "/wd4127")
        if (OPENSPACE_WARNINGS_AS_ERRORS)
            if (MSVC)
                target_compile_options(libOpenSpace PUBLIC "/WX")
                target_compile_options(OpenSpace PUBLIC "/WX")
            else ()
                target_compile_options(libOpenSpace PUBLIC "-Werror")
                target_compile_options(OpenSpace PUBLIC "-Werror")
            endif ()
        endif ()

        set_target_properties(OpenSpace PROPERTIES LINK_FLAGS
            "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
        )
    elseif (APPLE)
        target_compile_definitions(libOpenSpace PUBLIC "__APPLE__")

        include (CheckCXXCompilerFlag)
        CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
        CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
        mark_as_advanced(COMPILER_SUPPORTS_CXX11, COMPILER_SUPPORTS_CXX0X)
        if (COMPILER_SUPPORTS_CXX11)
            target_compile_options(libOpenSpace PUBLIC "-std=c++11")
        elseif (COMPILER_SUPPORTS_CXX0X)
            target_compile_options(libOpenSpace PUBLIC "-std=c++0x")
        else ()
          message(FATAL_ERROR "Compiler does not have C++11 support")
        endif ()

        target_compile_options(libOpenSpace PUBLIC "-stdlib=libc++")

        target_include_directories(libOpenSpace PUBLIC "/Developer/Headers/FlatCarbon")
        find_library(COREFOUNDATION_LIBRARY CoreFoundation)
        find_library(CARBON_LIBRARY Carbon)
        find_library(COCOA_LIBRARY Carbon)
        find_library(APP_SERVICES_LIBRARY ApplicationServices)
        mark_as_advanced(CARBON_LIBRARY COCOA_LIBRARY APP_SERVICES_LIBRARY)
        target_link_libraries(libOpenSpace
            ${CARBON_LIBRARY}
            ${COREFOUNDATION_LIBRARY}
            ${COCOA_LIBRARY}
            ${APP_SERVICES_LIBRARY}
        )
    elseif (UNIX)
          include (CheckCXXCompilerFlag)
          CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
          CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
          mark_as_advanced(COMPILER_SUPPORTS_CXX11, COMPILER_SUPPORTS_CXX0X)
          if (COMPILER_SUPPORTS_CXX11)
            target_compile_options(libOpenSpace PUBLIC "-std=c++11")
          elseif (COMPILER_SUPPORTS_CXX0X)
            target_compile_options(libOpenSpace PUBLIC "-std=c++0x")
          else ()
            message(FATAL_ERROR "Compiler does not have C++11 support")
          endif ()

        target_compile_options(libOpenSpace PUBLIC "-ggdb")
    endif ()
endfunction ()



function (add_external_dependencies)
    # Ghoul
    add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul)
    target_link_libraries(libOpenSpace Ghoul)
    get_property(GHOUL_INCLUDE_DIR TARGET Ghoul PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
    target_include_directories(libOpenSpace PUBLIC ${GHOUL_INCLUDE_DIR})
    get_property(GHOUL_DEFINITIONS TARGET Ghoul PROPERTY INTERFACE_COMPILE_DEFINITIONS)
    target_compile_definitions(libOpenSpace PUBLIC ${GHOUL_DEFINITIONS})
    set_property(TARGET Lua PROPERTY FOLDER "External")
    set_property(TARGET lz4 PROPERTY FOLDER "External")
    set_property(TARGET tinyobjloader PROPERTY FOLDER "External")

    # # SGCT
    find_package(SGCT REQUIRED)
    target_include_directories(libOpenSpace SYSTEM PUBLIC ${SGCT_INCLUDE_DIRECTORIES})
    target_link_libraries(libOpenSpace ${SGCT_LIBRARIES})
    if (UNIX AND (NOT APPLE))
        target_link_libraries(libOpenSpace Xcursor Xinerama)
    endif ()

    # Spice
    set(SPICE_ROOT_DIR "${OPENSPACE_EXT_DIR}/spice")
    find_package(Spice REQUIRED)
    target_include_directories(libOpenSpace SYSTEM PUBLIC ${SPICE_INCLUDE_DIRS})
    target_link_libraries(libOpenSpace ${SPICE_LIBRARIES})

    # # Kameleon
    option(KAMELEON_LIBRARY_ONLY "Build with Kameleon as library only" ON)
    if (WIN32)
        option(BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
    else ()
        option(BUILD_SHARED_LIBS "Build Shared Libraries" ON)
    endif ()
    mark_as_advanced(BUILD_SHARED_LIBS) # Change to set instead of option
    option(KAMELEON_USE_HDF5 "Kameleon use HDF5" OFF)
    set(KAMELEON_ROOT_DIR ${OPENSPACE_EXT_DIR}/kameleon)
    set(KAMELEON_INCLUDES ${KAMELEON_ROOT_DIR}/src)
    set(BOOST_ROOT "${OPENSPACE_EXT_DIR}/ghoul/ext/boost")
    add_subdirectory(${KAMELEON_ROOT_DIR})
    target_include_directories(libOpenSpace SYSTEM PUBLIC ${KAMELEON_INCLUDES})
    target_link_libraries(libOpenSpace ccmc)
    if (OPENSPACE_DISABLE_EXTERNAL_WARNINGS)
        if (MSVC)
            target_compile_options(ccmc PUBLIC "/W0" "/MP")
        else ()
            target_compile_options(ccmc PUBLIC "-w")
        endif ()
        target_compile_definitions(ccmc PUBLIC "_SCL_SECURE_NO_WARNINGS")
    endif ()
    set_property(TARGET ccmc PROPERTY FOLDER "External")
    if (TARGET cdf)
        if (OPENSPACE_DISABLE_EXTERNAL_WARNINGS)
            if (MSVC)
                target_compile_options(cdf PUBLIC "/W0" "/MP")
            else ()
                target_compile_options(ccmc PUBLIC "-w")
            endif ()
        endif ()
        set_property(TARGET cdf PROPERTY FOLDER "External")
    endif ()

    # Imgui
    add_subdirectory(${OPENSPACE_EXT_DIR}/imgui)
    get_property(IMGUI_INCLUDE_DIR TARGET Imgui PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
    target_link_libraries(libOpenSpace Imgui)
    target_include_directories(libOpenSpace PUBLIC ${IMGUI_INCLUDE_DIR})
    set_property(TARGET Imgui PROPERTY FOLDER "External")
    if (OPENSPACE_DISABLE_EXTERNAL_WARNINGS)
        if (MSVC)
            target_compile_options(Imgui PUBLIC "/W0" "/MP")
        else ()
            target_compile_options(ccmc PUBLIC "-w")
        endif ()
    endif ()
endfunction ()



function (handle_option_vld)
    if (OPENSPACE_ENABLE_VLD)
        target_link_libraries(libOpenSpace ${OPENSPACE_EXT_DIR}/vld/lib/vld.lib)
        target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_EXT_DIR}/vld)
    endif ()
endfunction ()



function(handle_option_gui)
    if (OPENSPACE_BUILD_GUI_APPLICATIONS)
        add_subdirectory(gui)
    endif ()
endfunction ()



function (handle_option_tests)
    if (OPENSPACE_HAVE_TESTS)
        if (NOT TARGET gtest)
            add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul/ext/gtest)
        endif ()

        file(GLOB_RECURSE OPENSPACE_TEST_FILES ${OPENSPACE_BASE_DIR}/tests/*.inl)

        add_executable(OpenSpaceTest ${OPENSPACE_BASE_DIR}/tests/main.cpp ${OPENSPACE_TEST_FILES})
        target_include_directories(OpenSpaceTest PUBLIC
            "${OPENSPACE_BASE_DIR}/include"
            "${OPENSPACE_BASE_DIR}/tests"
            "${OPENSPACE_EXT_DIR}/ghoul/ext/gtest/include"
        )
        target_link_libraries(OpenSpaceTest gtest libOpenSpace)

        if (MSVC)
            set_target_properties(OpenSpaceTest PROPERTIES LINK_FLAGS
                "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
            )
        endif ()
        set_property(TARGET OpenSpaceTest PROPERTY FOLDER "Unit Tests")
    endif (OPENSPACE_HAVE_TESTS)
    if (TARGET GhoulTest)
        set_property(TARGET GhoulTest PROPERTY FOLDER "Unit Tests")
    endif ()
endfunction ()


function (handle_internal_modules)
# Get all modules in the correct order based on their dependencies
    file(GLOB moduleDirs RELATIVE ${OPENSPACE_MODULE_DIR} ${OPENSPACE_MODULE_DIR}/*)
    set(sortedModules ${moduleDirs})
    foreach (dir ${moduleDirs})
        if(IS_DIRECTORY ${OPENSPACE_MODULE_DIR}/${dir})
            set(defaultModule OFF)
            if (EXISTS "${OPENSPACE_MODULE_DIR}/${dir}/include.cmake")
                unset(OPENSPACE_DEPENDENCIES)
                unset(DEFAULT_MODULE)
                include(${OPENSPACE_MODULE_DIR}/${dir}/include.cmake)

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
    list(REVERSE dir_list)
    foreach (dir ${dir_list})
        create_option_name(${dir} optionName)
        if (${optionName})
            if (EXISTS "${OPENSPACE_MODULE_DIR}/${dir}/include.cmake")
                unset(OPENSPACE_DEPENDENCIES)
                unset(DEFAULT_MODULE)
                include(${OPENSPACE_MODULE_DIR}/${dir}/include.cmake)

                if (OPENSPACE_DEPENDENCIES)
                    foreach (dependency ${OPENSPACE_DEPENDENCIES})
                        create_option_name(${dependency} dependencyOptionName)
                        set(${dependencyOptionName} ON CACHE BOOL "ff" FORCE)
                        message(STATUS "${dependencyOptionName} was set to build, due to dependency towards ${optionName}")
                    endforeach ()
                endif ()
            endif ()
        endif ()
    endforeach ()

    set(MODULE_HEADERS "")
    set(MODULE_CLASSES "")

    # Add subdirectories in the correct order
    foreach (module ${sortedModules})
        create_option_name(${module} optionName)
        if (${optionName})
            create_library_name(${module} libraryName)
            add_subdirectory(${OPENSPACE_MODULE_DIR}/${module})
            target_link_libraries(OpenSpace ${libraryName})
            target_link_libraries(libOpenSpace ${libraryName})
            create_define_name(${module} defineName)
            target_compile_definitions(libOpenSpace PUBLIC "${defineName}")

            # Create registration file
            string(TOUPPER ${module} module_upper)
            string(TOLOWER ${module} module_lower)
            unset(MODULE_NAME)
            unset(MODULE_PATH)
            include(${CMAKE_BINARY_DIR}/modules/${module_lower}/modulename.cmake)

            list(APPEND MODULE_HEADERS
                #"#ifdef REGISTRATION_OPENSPACE${module_upper}MODULE\n"
                "#include <${MODULE_PATH}>\n"
                #"#endif\n\n"
            )

            list(APPEND MODULE_CLASSES
                "new ${MODULE_NAME},\n"
            )

        endif ()
    endforeach ()

    string(REPLACE ";" "" MODULE_HEADERS ${MODULE_HEADERS})
    string(REPLACE ";" "" MODULE_CLASSES ${MODULE_CLASSES})

    configure_file(
        ${OPENSPACE_CMAKE_EXT_DIR}/module_registration.template
        ${CMAKE_BINARY_DIR}/_generated/include/openspace/moduleregistration.h
    )
endfunction ()

function (copy_dynamic_libraries)
    if (WIN32)
        # Copy DLLs needed by Ghoul into the executable directory
        ghl_copy_shared_libraries(OpenSpace ${OPENSPACE_EXT_DIR}/ghoul)

        if (TARGET OpenSpaceTest)
            ghl_copy_shared_libraries(OpenSpaceTest ${OPENSPACE_EXT_DIR}/ghoul)
        endif ()
    endif ()
endfunction ()