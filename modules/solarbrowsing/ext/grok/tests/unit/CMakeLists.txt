# UNIT TESTS

include_directories(
  ${GROK_BINARY_DIR}/src/lib/openjp2 # opj_config.h
  ${GROK_SOURCE_DIR}/src/lib/openjp2
)

set(unit_test
  testempty0
  testempty1
  testempty2
)
foreach(ut ${unit_test})
  add_executable(${ut} ${ut}.cpp)
  target_link_libraries(${ut} ${GROK_LIBRARY_NAME} ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT})
  add_test(NAME ${ut} COMMAND ${ut})
endforeach()
