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

include(${OPENSPACE_CMAKE_EXT_DIR}/set_openspace_compile_settings.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/handle_applications.cmake)
include(${OPENSPACE_CMAKE_EXT_DIR}/handle_modules.cmake)

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

    set_openspace_compile_settings(libOpenSpace)
endfunction ()



function (add_external_dependencies)
    # System libraries
    if (APPLE)
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
    endif()

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
    set(JPEG_TURBO_WITH_SIMD OFF CACHE BOOL "" FORCE)

    add_subdirectory(${OPENSPACE_EXT_DIR}/sgct)
    target_include_directories(libOpenSpace SYSTEM PUBLIC ${OPENSPACE_EXT_DIR}/sgct/include)
    target_link_libraries(
        libOpenSpace
        sgct_light glew glfw png16_static quat tinyxml2static turbojpeg-static
        vrpn
        ${GLFW_LIBRARIES}
    )

    set_property(TARGET sgct_light PROPERTY FOLDER "External")
    if (TARGET glew)
        set_property(TARGET glew PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET glfw)
        set_property(TARGET glfw PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET png16_static)
        set_property(TARGET png16_static PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET quat)
        set_property(TARGET quat PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET simd)
        set_property(TARGET simd PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET tinyxml2static)
        set_property(TARGET tinyxml2static PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET turbojpeg-static)
        set_property(TARGET turbojpeg-static PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET vrpn)
        set_property(TARGET vrpn PROPERTY FOLDER "External/SGCT")
    endif ()
    if (TARGET zlibstatic)
        set_property(TARGET zlibstatic PROPERTY FOLDER "External/SGCT")
    endif ()

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
            "~/Qt/5.8/clang_64/lib/cmake"
            PARENT_SCOPE
        )
    endif ()
endfunction ()



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
        set_openspace_compile_settings(OpenSpaceTest)
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



function (copy_dynamic_libraries)
    if (WIN32)
        ghl_copy_files(OpenSpace "${CURL_ROOT_DIR}/lib/libcurl.dll")

        # Copy DLLs needed by Ghoul into the executable directory
        ghl_copy_shared_libraries(OpenSpace ${OPENSPACE_EXT_DIR}/ghoul)

        if (TARGET OpenSpaceTest)
            ghl_copy_shared_libraries(OpenSpaceTest ${OPENSPACE_EXT_DIR}/ghoul)
            ghl_copy_files(OpenSpaceTest "${CURL_ROOT_DIR}/lib/libcurl.dll")
        endif ()
    endif ()
endfunction ()
