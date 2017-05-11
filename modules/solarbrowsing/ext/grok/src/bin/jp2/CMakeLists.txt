# Build the demo app, small examples

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# First thing define the common source:
set(common_SRCS
  convert.cpp
  convert.h
  convertbmp.cpp
  ${GROK_SOURCE_DIR}/src/bin/common/color.cpp
  ${GROK_SOURCE_DIR}/src/bin/common/color.h
  ${GROK_SOURCE_DIR}/src/bin/common/opj_getopt.c
  ${GROK_SOURCE_DIR}/src/bin/common/opj_getopt.h
  ${GROK_SOURCE_DIR}/src/bin/common/opj_string.h

  )

if(OPJ_HAVE_LIBTIFF)
	list(APPEND common_SRCS converttif.cpp)
endif()
if(OPJ_HAVE_LIBPNG)
	list(APPEND common_SRCS convertpng.cpp)
endif()

# Headers file are located here:
include_directories(
  ${GROK_BINARY_DIR}/src/lib/openjp2 # opj_config.h
  ${GROK_BINARY_DIR}/src/bin/common # opj_apps_config.h
  ${GROK_SOURCE_DIR}/src/lib/openjp2
  ${GROK_SOURCE_DIR}/src/bin/common
  ${LCMS_INCLUDE_DIRNAME}
  ${Z_INCLUDE_DIRNAME}
  ${PNG_INCLUDE_DIRNAME}
  ${TIFF_INCLUDE_DIRNAME}
  )

if(WIN32)
  if(BUILD_SHARED_LIBS)
    add_definitions(-DOPJ_EXPORTS)
  else()
    add_definitions(-DOPJ_STATIC)
  endif()
endif()

# Loop over all executables:
foreach(exe opj_decompress opj_compress opj_dump)
  add_executable(${exe} ${exe}.cpp ${common_SRCS})
  target_link_libraries(${exe} ${GROK_LIBRARY_NAME}
    ${PNG_LIBNAME} ${TIFF_LIBNAME} ${LCMS_LIBNAME} ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT}
    )
  # To support universal exe:
  if(ZLIB_FOUND AND APPLE)
    target_link_libraries(${exe} z)
  else(ZLIB_FOUND AND APPLE)
    target_link_libraries(${exe} ${Z_LIBNAME})
  endif()

  # On unix you need to link to the math library:
  if(UNIX)
    target_link_libraries(${exe} m)
  endif()
  # Install exe
  install(TARGETS ${exe}
    EXPORT OpenJPEGTargets
    DESTINATION ${GROK_INSTALL_BIN_DIR} COMPONENT Applications
  )
  if(OPJ_USE_DSYMUTIL)
    add_custom_command(TARGET ${exe} POST_BUILD
    COMMAND "dsymutil" "$<TARGET_FILE:${exe}>"
    COMMENT "dsymutil $<TARGET_FILE:${exe}>"
    DEPENDS ${exe})
  endif()
endforeach()

if(BUILD_DOC)
# Install man pages
install(
  FILES       ${GROK_SOURCE_DIR}/doc/man/man1/opj_compress.1
              ${GROK_SOURCE_DIR}/doc/man/man1/opj_decompress.1
              ${GROK_SOURCE_DIR}/doc/man/man1/opj_dump.1
  DESTINATION ${GROK_INSTALL_MAN_DIR}/man1)
#
endif()
