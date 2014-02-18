



# Find the library
if(WIN32)
    add_definitions(-D__WIN32__)
    file(GLOB SGCT_WINDOWS_PATHS "C:/Program Files/SGCT/SGCT_*")
    find_path(SGCT_ROOT_DIR include/sgct.h HINTS 
    	"${SGCT_WINDOWS_PATHS}"
	)
	set(DEPENDENT_LIBS
	    ${DEPENDENT_LIBS}
        "Ws2_32.lib"
        optimized "${SGCT_ROOT_DIR}/lib/msvc11_x64/sgct.lib"
        debug "${SGCT_ROOT_DIR}/lib/msvc11_x64/sgctd.lib"
    )
else()
	find_path(SGCT_ROOT_DIR include/sgct.h HINTS 
    	"/opt/local/include/sgct"
    	"/usr/local/include/sgct"
    	"/usr/include/sgct"
    	"/opt/local"
    	"/usr/local"
	)
	find_library(SGCT_LIBRARY NAMES ${CMAKE_STATIC_LIBRARY_PREFIX}sgct_cpp11${CMAKE_STATIC_LIBRARY_SUFFIX}
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

# includes
set(SGCT_INCLUDES "${SGCT_ROOT_DIR}/include")
include_directories(${SGCT_INCLUDE_DIRS})

# libraries
set(SGCT_INCLUDE_DIRECTORIES ${SGCT_INCLUDES})
set(SGCT_LIBRARIES ${SGCT_DEPENDENCIES} ${SGCT_LIBRARY})
#set(SGCT_LIBRARIES "${SGCT_LIBRARY}")

# handle the QUIETLY and REQUIRED arguments and set SGCT_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(SGCT  DEFAULT_MSG
                                  SGCT_LIBRARY SGCT_INCLUDES)

mark_as_advanced(SGCT_INCLUDE_DIR SGCT_LIBRARY )

if(SGCT_FOUND) 
    MESSAGE(STATUS "SGCT found: ${SGCT_INCLUDES}/sgct.h")
    MESSAGE(STATUS "SGCT SGCT_LIBRARY: ${SGCT_LIBRARY}")
else()
	MESSAGE(FATAL_ERROR "SGCT not found!")
endif(SGCT_FOUND)






# OLD STUFF. REMOVE WHEN ABOVE WORKS
#set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} ${SGCT_ROOT_DOR}/bin/lib/msvc11)
#set(DEPENDENT_LIBS
#    ${DEPENDENT_LIBS}
#    "Ws2_32.lib"
#    optimized "${SGCT_ROOT_DIR}/lib/msvc11_x64/sgct.lib"
#    debug "${SGCT_ROOT_DIR}/lib/msvc11_x64/sgctd.lib"
#   )

#add_subdirectory(${SGCT_ROOT_DIR})
#set(DEPENDENT_LIBS ${DEPENDENT_LIBS} SGCT)