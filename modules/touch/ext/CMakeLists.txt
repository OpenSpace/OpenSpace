##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2024                                                                #
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

project(libTUIO11)
message(STATUS "Generating libTUIO11 project")

set(TUIO_SOURCE 
  libTUIO11/TUIO/WebSockSender.cpp
  libTUIO11/TUIO/UdpSender.cpp
  libTUIO11/TUIO/UdpReceiver.cpp
  libTUIO11/TUIO/TuioTime.cpp
  libTUIO11/TUIO/TuioServer.cpp
  libTUIO11/TUIO/TuioPoint.cpp
  libTUIO11/TUIO/TuioObject.cpp
  libTUIO11/TUIO/TuioManager.cpp
  libTUIO11/TUIO/TuioDispatcher.cpp
  libTUIO11/TUIO/TuioCursor.cpp
  libTUIO11/TUIO/TuioContainer.cpp
  libTUIO11/TUIO/TuioClient.cpp
  libTUIO11/TUIO/TuioBlob.cpp
  libTUIO11/TUIO/TcpSender.cpp
  libTUIO11/TUIO/TcpReceiver.cpp
  libTUIO11/TUIO/OscReceiver.cpp 
  libTUIO11/TUIO/OneEuroFilter.cpp
  libTUIO11/TUIO/FlashSender.cpp
  
  libTUIO11/oscpack/ip/IpEndpointName.cpp
  
  libTUIO11/oscpack/osc/OscTypes.cpp
  libTUIO11/oscpack/osc/OscReceivedElements.cpp
  libTUIO11/oscpack/osc/OscPrintReceivedElements.cpp
  libTUIO11/oscpack/osc/OscOutboundPacketStream.cpp
)

if (WIN32)
  set(TUIO_SOURCE 
    ${TUIO_SOURCE}
    libTUIO11/oscpack/ip/win32/NetworkingUtils.cpp
    libTUIO11/oscpack/ip/win32/UdpSocket.cpp
  )
endif ()

if (UNIX)
  set(TUIO_SOURCE 
    ${TUIO_SOURCE}
    libTUIO11/oscpack/ip/posix/NetworkingUtils.cpp
    libTUIO11/oscpack/ip/posix/UdpSocket.cpp
  )
endif ()

add_library(libTUIO11 ${TUIO_SOURCE})
target_include_directories(libTUIO11 SYSTEM
  PUBLIC
    "${PROJECT_SOURCE_DIR}/libTUIO11/TUIO"
    "${PROJECT_SOURCE_DIR}/libTUIO11/"
    "${PROJECT_SOURCE_DIR}/libTUIO11/oscpack"
)
target_precompile_headers(libTUIO11 PRIVATE
  <iostream>
  <istream>
  <ostream>
  <stdexcept>
)
#   # [["tuiopoint.h"]]
#   # [["tuiocontainer.h"]]
#   # [["tuiotime.h"]]
#   # [["tuioobject.h"]]
#   # [["tuiodispatcher.h"]]
#   # [["tuiolistener.h"]]
#   # [["tuioclient.h"]]
#   # [["oscreceiver.h"]]
#   <stdexcept>
#   <math.h>
# )

if (WIN32)
  # Tuio dependencies
  target_link_libraries(libTUIO11 PRIVATE winmm.lib wininet.lib ws2_32.lib)
endif ()
