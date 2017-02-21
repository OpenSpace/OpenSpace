/*
 TUIO C++ Library
 Copyright (c) 2005-2016 Martin Kaltenbrunner <martin@tuio.org>
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 3.0 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library.
*/

#include "UdpSender.h"

using namespace TUIO;

UdpSender::UdpSender() {
	try {
		local = true;
		long unsigned int ip = GetHostByName("localhost");
		socket = new UdpTransmitSocket(IpEndpointName(ip, 3333));
		buffer_size = MAX_UDP_SIZE;
		std::cout << "TUIO/UDP messages to " << "127.0.0.1@3333" << std::endl;
	} catch (std::exception &e) { 
		std::cout << "could not create UDP socket" << std::endl;
		socket = NULL;
		throw std::exception();
	}
}

UdpSender::UdpSender(const char *host, int port) {
	try {
		if ((strcmp(host,"127.0.0.1")==0) || (strcmp(host,"localhost")==0)) {
			local = true;
			buffer_size = MAX_UDP_SIZE;
		} else {
			local = false;
			buffer_size = IP_MTU_SIZE;
		}
		long unsigned int ip = GetHostByName(host);
		socket = new UdpTransmitSocket(IpEndpointName(ip, port));
		std::cout << "TUIO/UDP messages to " << host << "@" << port << std::endl;
	} catch (std::exception &e) { 
		std::cout << "could not create UDP socket" << std::endl;
		socket = NULL;
		throw std::exception();
	}
}

UdpSender::UdpSender(const char *host, int port, int size) {
	try {
		if ((strcmp(host,"127.0.0.1")==0) || (strcmp(host,"localhost")==0)) {
			local = true;
		} else local = false;
		long unsigned int ip = GetHostByName(host);
		socket = new UdpTransmitSocket(IpEndpointName(ip, port));
		if (buffer_size>MAX_UDP_SIZE) buffer_size = MAX_UDP_SIZE;
		else if (buffer_size<MIN_UDP_SIZE) buffer_size = MIN_UDP_SIZE;
		std::cout << "TUIO/UDP messages to " << host << "@" << port << std::endl;
	} catch (std::exception &e) { 
		std::cout << "could not create UDP socket" << std::endl;
		socket = NULL;
		throw std::exception();
	}
}

UdpSender::~UdpSender() {
	delete socket;		
}

bool UdpSender::isConnected() { 
	if (socket==NULL) return false; 
	return true;
}

bool UdpSender::sendOscPacket (osc::OutboundPacketStream *bundle) {
	if (socket==NULL) return false; 
	if ( bundle->Size() > buffer_size ) return false;
	if ( bundle->Size() == 0 ) return false;

	socket->Send( bundle->Data(), bundle->Size() );
	return true;
}
