# Find the library
if(WIN32)
    # Check for visual studio
    if(NOT MSVC)
        message(FATAL_ERROR "Only visual studio supported!")
    endif(NOT MSVC)

    # make sure sgct knows about windows
    add_definitions(-D__WIN32__)

    # search for SGCT, 64-bit and 32-bit
    if (CMAKE_CL_64)
        set(SGCT_PATH "C:/Program Files/SGCT")
    else (CMAKE_CL_64)
        set(SGCT_PATH "C:/Program Files (x86)/SGCT")
    endif (CMAKE_CL_64)
    file(GLOB SGCT_WINDOWS_PATHS "${SGCT_PATH}/SGCT_*")
    
    # sort and reverse the list to find the most recent (a.k.a. highest number) version
    list(SORT SGCT_WINDOWS_PATHS)
    list(REVERSE SGCT_WINDOWS_PATHS)
    
    foreach(path ${SGCT_WINDOWS_PATHS})
        find_path(SGCT_ROOT_DIR include/sgct.h HINTS 
            "${path}"
        )
        if (SGCT_ROOT_DIR)
            break()
        endif ()
    endforeach(path)
    
    # Check if found the SGCT root directory
    if(NOT SGCT_ROOT_DIR)
        message(FATAL_ERROR "Could not locate SGCT in ${SGCT_PATH}!")
    endif(NOT SGCT_ROOT_DIR)


    find_path(SGCT_ROOT_DIR include/sgct.h HINTS 
        "${SGCT_WINDOWS_PATHS}"
    )
    
    # construct the correct library path
    if (MSVC10)
        set(SGCT_LIBRARY_FOLDER "${SGCT_ROOT_DIR}/lib/msvc10")
    elseif (MSVC11)
        set(SGCT_LIBRARY_FOLDER "${SGCT_ROOT_DIR}/lib/msvc11")
    elseif (MSVC12)
        set(SGCT_LIBRARY_FOLDER "${SGCT_ROOT_DIR}/lib/msvc12")
    elseif (MSVC14)
        set(SGCT_LIBRARY_FOLDER "${SGCT_ROOT_DIR}/lib/msvc14")
    endif (MSVC10)
    
    if (CMAKE_CL_64)
        set(SGCT_LIBRARY_FOLDER "${SGCT_LIBRARY_FOLDER}_x64")
    endif (CMAKE_CL_64)
    
    # Find the sgct library
    set(SGCT_LIBRARY
        "ws2_32.lib"
        optimized "${SGCT_LIBRARY_FOLDER}/sgct.lib"
        debug "${SGCT_LIBRARY_FOLDER}/sgctd.lib"
    )
else()
    find_path(SGCT_ROOT_DIR include/sgct.h HINTS 
        "/opt/local/include/sgct"
        "/usr/local/include/sgct"
        "/usr/include/sgct"
        "/opt/local"
        "/usr/local"
        "/usr"
    )
    if(NOT SGCT_ROOT_DIR)
        message(FATAL_ERROR "Could not locate SGCT!")
    endif(NOT SGCT_ROOT_DIR)

    # OS X has cpp11 support
    if (APPLE)
        set(SGCT_NAME "sgct_cpp11")
    else(APPLE)
        set(SGCT_NAME "sgct")
    endif(APPLE)

    # check if debug or release version of sgct should be used
    if(CMAKE_BUILD_TYPE MATCHES Debug)
        set(SGCT_NAME "${SGCT_NAME}d")
    endif(CMAKE_BUILD_TYPE MATCHES Debug)
    
    # Find the sgct library
    find_library(SGCT_LIBRARY NAMES ${CMAKE_STATIC_LIBRARY_PREFIX}${SGCT_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}
                 HINTS "${SGCT_ROOT_DIR}/lib")

endif()

# make sure glew is static
add_definitions(-DGLEW_STATIC)

if (APPLE)
    find_library(FRAMEWORK_IOKit
        NAMES IOKit
        PATHS ${CMAKE_OSX_SYSROOT}/System/Library
        PATH_SUFFIXES Frameworks
        NO_DEFAULT_PATH
    )
    find_library(FRAMEWORK_CoreVideo
        NAMES CoreVideo
        PATHS ${CMAKE_OSX_SYSROOT}/System/Library
        PATH_SUFFIXES Frameworks
        NO_DEFAULT_PATH
    )
    find_library(FRAMEWORK_Cocoa
        NAMES Cocoa
        PATHS ${CMAKE_OSX_SYSROOT}/System/Library
        PATH_SUFFIXES Frameworks
        NO_DEFAULT_PATH
    )
    set(SGCT_DEPENDENCIES ${SGCT_DEPENDENCIES} ${FRAMEWORK_IOKit} ${FRAMEWORK_CoreVideo} ${FRAMEWORK_Cocoa})
endif (APPLE)

if(UNIX AND NOT APPLE)
    find_package(XRandR)
    find_package(Xi)
    find_package(Threads)
    if(NOT XRANDR_FOUND)
        message(FATAL_ERROR "XRandR not found!")
    endif(NOT XRANDR_FOUND)
    if(NOT XI_FOUND)
        message(FATAL_ERROR "Xi not found!")
    endif(NOT XI_FOUND)
    if(NOT THREADS_FOUND)
        message(FATAL_ERROR "Threads not found!")
    endif(NOT THREADS_FOUND)
    set(SGCT_DEPENDENCIES ${SGCT_DEPENDENCIES} ${XRANDR_LIBRARIES} ${XI_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} Xxf86vm )
endif(UNIX AND NOT APPLE)

# includes
set(SGCT_INCLUDES "${SGCT_ROOT_DIR}/include")
include_directories(${SGCT_INCLUDE_DIRS})

# libraries
set(SGCT_INCLUDE_DIRECTORIES ${SGCT_INCLUDES})
set(SGCT_LIBRARIES ${SGCT_LIBRARY} ${SGCT_DEPENDENCIES})


# handle the QUIETLY and REQUIRED arguments and set SGCT_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(SGCT  DEFAULT_MSG
                                  SGCT_LIBRARY SGCT_INCLUDES)

mark_as_advanced(SGCT_INCLUDE_DIR SGCT_LIBRARY )

if(SGCT_FOUND) 
    MESSAGE(STATUS "SGCT found: ${SGCT_INCLUDES}/sgct.h")
else()
    MESSAGE(FATAL_ERROR "SGCT not found!")
endif(SGCT_FOUND)

