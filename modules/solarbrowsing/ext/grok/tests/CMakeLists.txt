# Tests

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)


include_directories(
  ${GROK_BINARY_DIR}/src/lib/openjp2 # opj_config.h
  ${GROK_BINARY_DIR}/src/bin/common # opj_apps_config.h
  ${GROK_SOURCE_DIR}/src/lib/openjp2
  ${GROK_SOURCE_DIR}/src/bin/jp2
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

# First thing define the common source:
set(compare_images_SRCS compare_images.cpp
  ${GROK_SOURCE_DIR}/src/bin/common/color.cpp
  ${GROK_SOURCE_DIR}/src/bin/common/color.h
  ${GROK_SOURCE_DIR}/src/bin/jp2/convert.cpp
  ${GROK_SOURCE_DIR}/src/bin/jp2/convertpng.cpp
  ${GROK_SOURCE_DIR}/src/bin/jp2/converttif.cpp
  ${GROK_SOURCE_DIR}/src/bin/common/opj_getopt.c
  )

set(compare_dump_files_SRCS compare_dump_files.cpp
  ${GROK_SOURCE_DIR}/src/bin/common/opj_getopt.c)

set(compare_raw_files_SRCS compare_raw_files.cpp
  ${GROK_SOURCE_DIR}/src/bin/common/opj_getopt.c)

add_executable(compare_images ${compare_images_SRCS})
target_link_libraries(compare_images
  ${GROK_LIBRARY_NAME}
  ${PNG_LIBNAME} ${TIFF_LIBNAME} ${LCMS_LIBNAME} ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT}
  )
# To support universal exe:
if(ZLIB_FOUND AND APPLE)
  target_link_libraries(compare_images z)
else(ZLIB_FOUND AND APPLE)
  target_link_libraries(compare_images ${Z_LIBNAME})
endif()

add_executable(compare_dump_files ${compare_dump_files_SRCS})

add_executable(j2k_random_tile_access j2k_random_tile_access.cpp)
target_link_libraries(j2k_random_tile_access ${GROK_LIBRARY_NAME} ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT} )

add_executable(compare_raw_files ${compare_raw_files_SRCS})

add_executable(test_tile_encoder test_tile_encoder.cpp)
target_link_libraries(test_tile_encoder ${GROK_LIBRARY_NAME} ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT})

# Let's try a couple of possibilities:
add_test(NAME tte0 COMMAND test_tile_encoder)
add_test(NAME tte1 COMMAND test_tile_encoder 3 2048 2048 1024 1024 8 1 tte1.j2k)
add_test(NAME tte2 COMMAND test_tile_encoder 3 2048 2048 1024 1024 8 1 tte2.jp2)
add_test(NAME tte3 COMMAND test_tile_encoder 1 2048 2048 1024 1024 8 1 tte3.j2k)
add_test(NAME tte4 COMMAND test_tile_encoder 1  256  256  128  128 8 0 tte4.j2k)
add_test(NAME tte5 COMMAND test_tile_encoder 1  512  512  256  256 8 0 tte5.j2k)
#add_test(NAME tte6 COMMAND test_tile_encoder 1 8192 8192  512  512 8 0 tte6.j2k)
#add_test(NAME tte7 COMMAND test_tile_encoder 1 32768 32768 512  512 8 0 tte7.jp2)

add_executable(test_tile_decoder test_tile_decoder.cpp)
target_link_libraries(test_tile_decoder ${GROK_LIBRARY_NAME} ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT})

add_test(NAME ttd0 COMMAND test_tile_decoder)
set_property(TEST ttd0 APPEND PROPERTY DEPENDS tte0)
add_test(NAME ttd1 COMMAND test_tile_decoder 0 0 1024 1024 tte1.j2k)
set_property(TEST ttd1 APPEND PROPERTY DEPENDS tte1)
add_test(NAME ttd2 COMMAND test_tile_decoder 0 0 1024 1024 tte2.jp2)
set_property(TEST ttd2 APPEND PROPERTY DEPENDS tte2)
#add_test(NAME ttd6 COMMAND test_tile_decoder 0 0  512  512 tte6.j2k)
#set_property(TEST ttd6 APPEND PROPERTY DEPENDS tte6)
#add_test(NAME ttd7 COMMAND test_tile_decoder 0 0  512  512 tte7.jp2)
#set_property(TEST ttd7 APPEND PROPERTY DEPENDS tte7)

add_test(NAME rta1 COMMAND j2k_random_tile_access tte1.j2k)
set_property(TEST rta1 APPEND PROPERTY DEPENDS tte1)
add_test(NAME rta2 COMMAND j2k_random_tile_access tte2.jp2)
set_property(TEST rta2 APPEND PROPERTY DEPENDS tte2)
add_test(NAME rta3 COMMAND j2k_random_tile_access tte3.j2k)
set_property(TEST rta3 APPEND PROPERTY DEPENDS tte3)
add_test(NAME rta4 COMMAND j2k_random_tile_access tte4.j2k)
set_property(TEST rta4 APPEND PROPERTY DEPENDS tte4)
add_test(NAME rta5 COMMAND j2k_random_tile_access tte5.j2k)
set_property(TEST rta5 APPEND PROPERTY DEPENDS tte5)

# No image send to the dashboard if lib PNG is not available.
if(NOT OPJ_HAVE_LIBPNG)
  message(WARNING "Lib PNG seems to be not available: if you want run the non-regression tests with images reported to the dashboard, you need it (try BUILD_THIRDPARTY)")
endif()

add_subdirectory(conformance)
add_subdirectory(nonregression)
add_subdirectory(unit)

add_executable(ppm2rgb3 ppm2rgb3.cpp)
#add_executable(pdf2jp2 pdf2jp2.c)
