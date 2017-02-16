#########################################################################################
#                                                                                       #
# OpenSpace                                                                             #
#                                                                                       #
# Copyright (c) 2014-2016                                                               #
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

project(libTUIO2)
message(STATUS "Generating libTUIO2 project")

add_library(libTUIO2
    ${PROJECT_SOURCE_DIR}/TUIO2/WebSockSender.cpp
    ${PROJECT_SOURCE_DIR}/TUIO2/UdpSender.cpp
    ${PROJECT_SOURCE_DIR}/TUIO2/UdpReceiver.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioToken.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioTime.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioSymbol.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioServer.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioPointer.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioPoint.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioObject.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioManager.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioDispatcher.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioComponent.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioClient.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TuioBounds.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TcpSender.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/TcpReceiver.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/OscReceiver.cpp
	${PROJECT_SOURCE_DIR}/TUIO2/FlashSender.cpp
	
	${PROJECT_SOURCE_DIR}/oscpack/ip/IpEndpointName.cpp
	
	#if (WIN32)
	${PROJECT_SOURCE_DIR}/oscpack/ip/win32/NetworkingUtils.cpp
	${PROJECT_SOURCE_DIR}/oscpack/ip/win32/UdpSocket.cpp
	#else (WIN32)
	#${PROJECT_SOURCE_DIR}/oscpack/ip/posix/NetworkingUtils.cpp
	#${PROJECT_SOURCE_DIR}/oscpack/ip/posix/UdpSocket.cpp
	#endif ()
	${PROJECT_SOURCE_DIR}/oscpack/osc/OscTypes.cpp
	${PROJECT_SOURCE_DIR}/oscpack/osc/OscReceivedElements.cpp
	${PROJECT_SOURCE_DIR}/oscpack/osc/OscPrintReceivedElements.cpp
	${PROJECT_SOURCE_DIR}/oscpack/osc/OscOutboundPacketStream.cpp
)
target_include_directories(libTUIO2 PUBLIC ${PROJECT_SOURCE_DIR})