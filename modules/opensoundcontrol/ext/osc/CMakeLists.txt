cmake_minimum_required(VERSION 2.6)
PROJECT(Oscpack)
message(STATUS "Generating oscpack project")

# separate versions of NetworkingUtils.cpp and UdpSocket.cpp are provided for Win32 and POSIX
# the IpSystemTypePath selects the correct ones based on the current platform

IF(WIN32)
	set(IpSystemTypePath ip/win32)
ELSE(WIN32)
	set(IpSystemTypePath ip/posix)
ENDIF(WIN32)

ADD_LIBRARY(oscpack 
	ip/IpEndpointName.h
	ip/IpEndpointName.cpp
	ip/NetworkingUtils.h
	${IpSystemTypePath}/NetworkingUtils.cpp
	ip/UdpSocket.h
	${IpSystemTypePath}/UdpSocket.cpp
	ip/PacketListener.h
	ip/TimerListener.h
	osc/OscTypes.h
	osc/OscTypes.cpp 
	osc/OscHostEndianness.h
	osc/OscException.h
	osc/OscPacketListener.h
	osc/MessageMappingOscPacketListener.h
	osc/OscReceivedElements.h
	osc/OscReceivedElements.cpp
	osc/OscPrintReceivedElements.h
	osc/OscPrintReceivedElements.cpp
	osc/OscOutboundPacketStream.h
	osc/OscOutboundPacketStream.cpp
)

TARGET_INCLUDE_DIRECTORIES(oscpack PUBLIC ${CMAKE_SOURCE_DIR})

IF(WIN32)
	target_link_libraries(oscpack PUBLIC Ws2_32 winmm)
ENDIF(WIN32)

if(MSVC)
	# Force to always compile with W4
	if(CMAKE_CXX_FLAGS MATCHES /W[0-4])
		string(REGEX REPLACE /W[0-4] /W4 CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
	else()
		set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} /W4)
	endif()
elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
	# Update if necessary
	set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-long-long -pedantic)
endif()
