cmake_minimum_required(VERSION 3.20)
project(tuio LANGUAGES CXX)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(TUIO_SOURCE
  TUIO/WebSockSender.cpp
  TUIO/UdpSender.cpp
  TUIO/UdpReceiver.cpp
  TUIO/TuioTime.cpp
  TUIO/TuioServer.cpp
  TUIO/TuioPoint.cpp
  TUIO/TuioObject.cpp
  TUIO/TuioManager.cpp
  TUIO/TuioDispatcher.cpp
  TUIO/TuioCursor.cpp
  TUIO/TuioContainer.cpp
  TUIO/TuioClient.cpp
  TUIO/TuioBlob.cpp
  TUIO/TcpSender.cpp
  TUIO/TcpReceiver.cpp
  TUIO/OscReceiver.cpp
  TUIO/OneEuroFilter.cpp
  TUIO/FlashSender.cpp

  oscpack/ip/IpEndpointName.cpp

  oscpack/osc/OscTypes.cpp
  oscpack/osc/OscReceivedElements.cpp
  oscpack/osc/OscPrintReceivedElements.cpp
  oscpack/osc/OscOutboundPacketStream.cpp
)

if(WIN32)
  list(APPEND TUIO_SOURCE
    oscpack/ip/win32/NetworkingUtils.cpp
    oscpack/ip/win32/UdpSocket.cpp
  )
else()
  list(APPEND TUIO_SOURCE
    oscpack/ip/posix/NetworkingUtils.cpp
    oscpack/ip/posix/UdpSocket.cpp
  )
endif()

# Always static: LibExport.h's LIBDECL only becomes __declspec(dllexport) when LIB_EXPORT is
# defined, which nothing here defines, so a shared build would export no symbols on Windows.
add_library(tuio STATIC ${TUIO_SOURCE})
add_library(tuio::tuio ALIAS tuio)

target_include_directories(tuio PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/TUIO>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/oscpack>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/tuio>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/tuio/TUIO>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/tuio/oscpack>
)

if(WIN32)
  target_link_libraries(tuio PUBLIC winmm wininet ws2_32)
endif()

install(
  TARGETS tuio
  EXPORT tuioTargets
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

install(
  DIRECTORY TUIO/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/tuio/TUIO
  FILES_MATCHING PATTERN "*.h"
)
install(
  DIRECTORY oscpack/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/tuio/oscpack
  FILES_MATCHING PATTERN "*.h"
)

install(
  EXPORT tuioTargets
  FILE tuio-targets.cmake
  NAMESPACE tuio::
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/tuio
)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/tuioConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/tuioConfig.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/tuio
)
install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/tuioConfig.cmake
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/tuio
)
