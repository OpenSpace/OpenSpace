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
    # In order to be able to include libOpenSpace files
    target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_BASE_DIR}/include)
    # In order to be able to include module files
    target_include_directories(libOpenSpace PUBLIC ${OPENSPACE_BASE_DIR})
    # In order to be able to include the module_registration file
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
