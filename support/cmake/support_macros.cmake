#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2017                                                               #
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
        if (MSVC_VERSION LESS 1900)
            message(FATAL_ERROR "OpenSpace requires at least Visual Studio 2015")
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



function (create_openspace_target)
    add_library(libOpenSpace STATIC ${OPENSPACE_HEADER} ${OPENSPACE_SOURCE})
    target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_BASE_DIR}/include)
    target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_BASE_DIR})
    target_include_directories(libOpenSpace PUBLIC ${CMAKE_BINARY_DIR}/_generated/include)

    configure_file(
        ${OPENSPACE_CMAKE_EXT_DIR}/openspace_header.template 
        ${CMAKE_BINARY_DIR}/_generated/include/openspace/openspace.h 
        @ONLY IMMEDIATE
    )

    set_compile_settings(libOpenSpace)
endfunction ()



function (set_compile_settings project)
    set_property(TARGET ${project} PROPERTY CXX_STANDARD 14)
    set_property(TARGET ${project} PROPERTY CXX_STANDARD_REQUIRED On)

    if (MSVC)
        target_compile_options(${project} PRIVATE
            "/MP"       # Multi-threading support
            "/W4"       # Enable all warnings
            "/ZI"       # Edit and continue support
            "/wd4201"   # Disable "nameless struct" warning
            "/wd4127"   # Disable "conditional expression is constant" warning
        )
        if (OPENSPACE_WARNINGS_AS_ERRORS)
            target_compile_options(${project} PRIVATE "/WX")
        endif ()
    elseif (APPLE)
        target_compile_definitions(${project} PRIVATE "__APPLE__")

        if (OPENSPACE_WARNINGS_AS_ERRORS)
            target_compile_options(${project} PRIVATE "-Werror")
        endif ()

        target_compile_options(${project} PRIVATE "-stdlib=libc++")

        target_include_directories(${project} PUBLIC "/Developer/Headers/FlatCarbon")
        find_library(COREFOUNDATION_LIBRARY CoreFoundation)
        find_library(CARBON_LIBRARY Carbon)
        find_library(COCOA_LIBRARY Carbon)
        find_library(APP_SERVICES_LIBRARY ApplicationServices)
        mark_as_advanced(CARBON_LIBRARY COCOA_LIBRARY APP_SERVICES_LIBRARY)
        target_link_libraries(${project}
            ${CARBON_LIBRARY}
            ${COREFOUNDATION_LIBRARY}
            ${COCOA_LIBRARY}
            ${APP_SERVICES_LIBRARY}
        )
    elseif (UNIX)
        target_compile_options(${project} PRIVATE "-ggdb" "-Wall" "-Wno-long-long" "-pedantic" "-Wextra")

        if (OPENSPACE_WARNINGS_AS_ERRORS)
            target_compile_options(${project} PRIVATE "-Werror")
        endif ()
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

    # SGCT
    set(SGCT_TEXT OFF CACHE BOOL "" FORCE)
    set(SGCT_BUILD_CSHARP_PROJECTS OFF CACHE BOOL "" FORCE)
    set(SGCT_LIGHT_ONLY ON CACHE BOOL "" FORCE)
    set(SGCT_CUSTOMOUTPUTDIRS OFF CACHE BOOL "" FORCE)
    
    add_subdirectory(${OPENSPACE_EXT_DIR}/sgct)
    target_include_directories(libOpenSpace SYSTEM PUBLIC ${OPENSPACE_EXT_DIR}/sgct/include)
    target_link_libraries(
        libOpenSpace
        # sgct
        sgct_light glew glfw png16_static quat tinyxml2static turbojpeg-static
        vrpn
        ${GLFW_LIBRARIES}
    )

    set_property(TARGET sgct_light PROPERTY FOLDER "External")
    set_property(TARGET glew PROPERTY FOLDER "External/SGCT")
    set_property(TARGET glfw PROPERTY FOLDER "External/SGCT")
    set_property(TARGET png16_static PROPERTY FOLDER "External/SGCT")
    set_property(TARGET quat PROPERTY FOLDER "External/SGCT")
    set_property(TARGET simd PROPERTY FOLDER "External/SGCT")
    set_property(TARGET tinyxml2static PROPERTY FOLDER "External/SGCT")
    set_property(TARGET turbojpeg-static PROPERTY FOLDER "External/SGCT")
    set_property(TARGET vrpn PROPERTY FOLDER "External/SGCT")
    set_property(TARGET zlibstatic PROPERTY FOLDER "External/SGCT")

    if (UNIX AND (NOT APPLE))
        target_link_libraries(libOpenSpace Xcursor Xinerama X11)
    endif ()

    # Spice
    add_subdirectory(${OPENSPACE_EXT_DIR}/spice)
    target_link_libraries(libOpenSpace Spice)
    get_property(SPICE_INCLUDE_DIR TARGET Spice PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
    target_include_directories(libOpenSpace PUBLIC ${SPICE_INCLUDE_DIR})
    set_property(TARGET Spice PROPERTY FOLDER "External")

    # Curl
    if (WIN32)
        set(CURL_ROOT_DIR "${OPENSPACE_EXT_DIR}/curl")
        set(CURL_ROOT_DIR "${OPENSPACE_EXT_DIR}/curl" PARENT_SCOPE)
        target_include_directories(libOpenSpace SYSTEM PUBLIC ${CURL_ROOT_DIR}/include)
        target_link_libraries(libOpenSpace ${CURL_ROOT_DIR}/lib/libcurl_imp.lib)
        target_compile_definitions(libOpenSpace PUBLIC "OPENSPACE_CURL_ENABLED" "CURL_STATICLIB")
    else ()
        find_package(CURL)
        if (CURL_FOUND)
            target_include_directories(libOpenSpace SYSTEM PUBLIC ${CURL_INCLUDE_DIRS})
            target_link_libraries(libOpenSpace ${CURL_LIBRARIES})
            target_compile_definitions(libOpenSpace PUBLIC "OPENSPACE_CURL_ENABLED")
        endif ()
    endif()

    # Qt
    # Unfortunately, we have to set this value manually; sigh
    # In the future, if the Qt version is updated, just add to this variable ---abock
    if (APPLE)
        set(CMAKE_PREFIX_PATH
            "~/Qt/5.6/clang_64/lib/cmake"
            "~/Qt/5.7/clang_64/lib/cmake"
            PARENT_SCOPE
        )
    endif ()
endfunction ()



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
            set_compile_settings(${APPLICATION_NAME})

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


function (handle_option_vld)
    if (OPENSPACE_ENABLE_VLD)
        target_compile_definitions(libOpenSpace PUBLIC "OPENSPACE_ENABLE_VLD")
        target_link_libraries(libOpenSpace ${OPENSPACE_EXT_DIR}/vld/lib/vld.lib)
        target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_EXT_DIR}/vld)

        foreach (app ${OPENSPACE_APPLCATIONS})
            ghl_copy_files(${app} "${OPENSPACE_EXT_DIR}/vld/bin/vld_x64.dll")
        endforeach ()
    endif ()
endfunction ()



function (handle_option_tests)
    if (OPENSPACE_HAVE_TESTS)
        if (NOT TARGET gtest)
            set(BUILD_GTEST ON CACHE BOOL "")
            set(BUILD_GMOCK OFF CACHE BOOL "")
            set(gtest_force_shared_crt ON CACHE BOOL "")
            # set(BUILD_GMOCK OFF CACHE BOOL "")
            # option(BUILD_GTEST "Builds the googletest subproject" CACHE ON)
            # option(BUILD_GMOCK "Builds the googlemock subproject" CACHE OFF)
            # option(BUILD_SHARED_LIBS "Build shared libraries (DLLs)." CACHE ON)
            add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul/ext/googletest)
            # add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul/ext/gtest)
            set_property(TARGET gtest PROPERTY FOLDER "External")
            set_property(TARGET gtest_main PROPERTY FOLDER "External")
        endif ()

        file(GLOB_RECURSE OPENSPACE_TEST_FILES ${OPENSPACE_BASE_DIR}/tests/*.inl)

        add_executable(OpenSpaceTest ${OPENSPACE_BASE_DIR}/tests/main.cpp ${OPENSPACE_TEST_FILES})
        target_include_directories(OpenSpaceTest PUBLIC
            "${OPENSPACE_BASE_DIR}/include"
            "${OPENSPACE_BASE_DIR}/tests"
            "${OPENSPACE_EXT_DIR}/ghoul/ext/googletest/googletest/include"
        )
        target_compile_definitions(OpenSpaceTest PUBLIC
            "GHL_THROW_ON_ASSERT"
            "GTEST_HAS_TR1_TUPLE=0"
        )
        target_link_libraries(OpenSpaceTest gtest libOpenSpace)

        if (MSVC)
            set_target_properties(OpenSpaceTest PROPERTIES LINK_FLAGS
                "/NODEFAULTLIB:LIBCMTD.lib /NODEFAULTLIB:LIBCMT.lib"
            )
        endif ()
        set_property(TARGET OpenSpaceTest PROPERTY FOLDER "Unit Tests")
        set_property(TARGET OpenSpaceTest PROPERTY CXX_STANDARD 14)
        set_property(TARGET OpenSpaceTest PROPERTY CXX_STANDARD_REQUIRED On)
    endif (OPENSPACE_HAVE_TESTS)
    if (TARGET GhoulTest)
        if (NOT TARGET gtest)
            set(BUILD_GTEST ON CACHE BOOL "")
            set(BUILD_GMOCK OFF CACHE BOOL "")
            set(gtest_force_shared_crt ON CACHE BOOL "")
            # option(BUILD_GTEST "Builds the googletest subproject" CACHE ON)
            # option(BUILD_GMOCK "Builds the googlemock subproject" CACHE OFF)
            # option(BUILD_SHARED_LIBS "Build shared libraries (DLLs)." CACHE ON)
            add_subdirectory(${OPENSPACE_EXT_DIR}/ghoul/ext/googletest)
        endif ()

        set_property(TARGET gtest PROPERTY FOLDER "External")
        set_property(TARGET GhoulTest PROPERTY FOLDER "Unit Tests")
    endif ()
endfunction ()



function (handle_internal_modules)
# Get all modules in the correct order based on their dependencies
    file(GLOB moduleDirs RELATIVE ${OPENSPACE_MODULE_DIR} ${OPENSPACE_MODULE_DIR}/*)
    set(sortedModules ${moduleDirs})
    foreach (dir ${moduleDirs})
        if (IS_DIRECTORY ${OPENSPACE_MODULE_DIR}/${dir})
            set(defaultModule OFF)
            if (EXISTS "${OPENSPACE_MODULE_DIR}/${dir}/include.cmake")
                unset(OPENSPACE_DEPENDENCIES)
                unset(EXTERNAL_LIBRAY)
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
    set(dll_list "")
    list(REVERSE dir_list)
    foreach (dir ${dir_list})
        create_option_name(${dir} optionName)
        if (${optionName})
            if (EXISTS "${OPENSPACE_MODULE_DIR}/${dir}/include.cmake")
                unset(OPENSPACE_DEPENDENCIES)
                unset(EXTERNAL_LIBRAY)
                unset(DEFAULT_MODULE)
                include(${OPENSPACE_MODULE_DIR}/${dir}/include.cmake)

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
            add_subdirectory(${OPENSPACE_MODULE_DIR}/${module})

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
            include(${CMAKE_BINARY_DIR}/modules/${module_lower}/modulename.cmake)

            list(APPEND MODULE_HEADERS
                #"#ifdef REGISTRATION_OPENSPACE${module_upper}MODULE\n"
                "#include <${MODULE_PATH}>\n"
                #"#endif\n\n"
            )
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



function (copy_dynamic_libraries)
    if (WIN32)
        ghl_copy_files(OpenSpace "${CURL_ROOT_DIR}/lib/libcurl.dll")

        # Copy DLLs needed by Ghoul into the executable directory
        ghl_copy_shared_libraries(OpenSpace ${OPENSPACE_EXT_DIR}/ghoul)

        if (TARGET OpenSpaceTest)
            ghl_copy_shared_libraries(OpenSpaceTest ${OPENSPACE_EXT_DIR}/ghoul)
        endif ()
    endif ()
endfunction ()
