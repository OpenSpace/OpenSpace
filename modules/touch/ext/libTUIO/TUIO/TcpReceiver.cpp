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

#include "TcpReceiver.h"

using namespace TUIO;
using namespace osc;

// workaround for connect method name conflict
int tcp_connect(int socket, const struct sockaddr *address, socklen_t address_len) {
	return connect(socket, address, address_len);
}

#if defined (WIN32) && not defined (int32_t)
typedef	DWORD int32_t;
#endif

#ifdef WIN32
static DWORD WINAPI ClientThreadFunc( LPVOID obj )
#else
static void* ClientThreadFunc( void* obj )
#endif
{
	TcpReceiver *sender = static_cast<TcpReceiver*>(obj);
	char data_buffer[MAX_TCP_SIZE+4];
	char data_size[4];
	int32_t bundle_size;

#ifdef WIN32
	SOCKET client = sender->tcp_client_list.back();
#else
	int client = sender->tcp_client_list.back();
#endif

	int bytes = 1;
	while (bytes) {
		bytes = recv(client, data_buffer, sizeof(data_buffer),0);

		if (bytes>=4) {
			memcpy(&data_size[0],&data_buffer[0], 4);
#ifdef OSC_HOST_LITTLE_ENDIAN
			bundle_size = 0xFF & data_size[3];
			bundle_size |= (0xFF & data_size[2]) << 8;
			bundle_size |= (0xFF & data_size[1]) << 16;
			bundle_size |= (int32_t)(0xFF & data_size[0]) << 24;
#else
			bundle_size = (int32_t)(*data_size);
#endif
			if (bundle_size+4==bytes) {
				sender->ProcessPacket(&data_buffer[4],(int)bundle_size,IpEndpointName());
			}
		}
	}

	sender->tcp_client_list.remove(client);
	std::cout << "closed TUIO/TCP connection" << std::endl;

	//if (sender->tcp_client_list.size()==0) sender->connected=false;
	//std::cout << sender->tcp_client_list.size() << " clients left"<< std::endl;	

	return 0;
};

#ifndef  WIN32
static void* ServerThreadFunc( void* obj )
#else
static DWORD WINAPI ServerThreadFunc( LPVOID obj )
#endif
{
	TcpReceiver *sender = static_cast<TcpReceiver*>(obj);
	struct sockaddr_in client_addr;
	socklen_t len = sizeof(client_addr);

	while ((int)sender->tcp_socket>0) {
#ifdef WIN32
		SOCKET tcp_client = -1;
#else
		int tcp_client = -1;
#endif
		tcp_client = accept(sender->tcp_socket, (struct sockaddr*)&client_addr, &len);

		if (tcp_client>0) { 
			std::cout << "listening to TUIO/TCP messages from " << inet_ntoa(client_addr.sin_addr) << "@" << client_addr.sin_port << std::endl;
			sender->tcp_client_list.push_back(tcp_client);
			//sender->connected=true;
			//std::cout << sender->tcp_client_list.size() << " clients connected"<< std::endl;	
			
#ifndef WIN32
			pthread_t client_thread;
			pthread_create(&client_thread , NULL, ClientThreadFunc,obj);
#else
			DWORD ClientThreadId;
			HANDLE client_thread = CreateThread( 0, 0, ClientThreadFunc, obj, 0, &ClientThreadId );
#endif
		} else break;

	}
	
	return 0;
};

TcpReceiver::TcpReceiver(int port)
: tcp_socket	(-1)
, locked	(false)
{

	tcp_socket = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (tcp_socket < 0) std::cerr << "could not create TUIO/TCP socket" << std::endl;
	
	int optval = 1;
#ifdef  WIN32
	int ret = setsockopt(tcp_socket,SOL_SOCKET,SO_REUSEADDR, (const char *)&optval,  sizeof(int));
#else
	int ret = setsockopt(tcp_socket,SOL_SOCKET,SO_REUSEADDR, (const void *)&optval,  sizeof(int));
#endif
	if (ret < 0) {
		std::cerr << "could not reuse TUIO/TCP socket address" << std::endl;
		return;
	}
	
	struct sockaddr_in tcp_server;
	memset( &tcp_server, 0, sizeof (tcp_server));
	
	tcp_server.sin_family = AF_INET;
	tcp_server.sin_addr.s_addr = htonl(INADDR_ANY);
	tcp_server.sin_port = htons(port);
	
	socklen_t len = sizeof(tcp_server);
	ret = bind(tcp_socket,(struct sockaddr*)&tcp_server,len);
	if (ret < 0) {
		std::cerr << "could not bind to TUIO/TCP socket on port " << port << std::endl;
		return;
	}
	
	ret =  listen(tcp_socket, 1);
	if (ret < 0) {
		std::cerr << "could not start listening to TUIO/TCP socket" << std::endl;
#ifdef WIN32
		closesocket(tcp_socket);
#else
		close(tcp_socket);
#endif
		tcp_socket=-1;
		return;
	}

	std::cout << "TUIO/TCP socket created on port " << port << std::endl;
}

TcpReceiver::TcpReceiver(const char *host, int port)
: tcp_socket	(-1)
, locked		(false)
{

	tcp_socket = socket( AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (tcp_socket < 0) {
		std::cerr << "could not create TUIO/TCP socket" << std::endl;
		return;
	}

	struct sockaddr_in tcp_server;
	memset( &tcp_server, 0, sizeof (tcp_server));
	unsigned long addr = inet_addr(host);
	if (addr != INADDR_NONE) {
		memcpy( (char *)&tcp_server.sin_addr, &addr, sizeof(addr));
	} else {
		struct hostent *host_info = gethostbyname(host);
		if (host_info == NULL) std::cerr << "unknown host name: " << host << std::endl;
		memcpy( (char *)&tcp_server.sin_addr, host_info->h_addr, host_info->h_length );
	}
	
	tcp_server.sin_family = AF_INET;
	tcp_server.sin_port = htons(port);

	int ret = tcp_connect(tcp_socket,(struct sockaddr*)&tcp_server,sizeof(tcp_server));
	if (ret<0) {
#ifdef WIN32
		closesocket(tcp_socket);
#else
		close(tcp_socket);
#endif		
		std::cerr << "could not connect to TUIO/TCP server at " << host << ":"<< port << std::endl;
		tcp_socket=-1;
		return;
	} else {
		tcp_client_list.push_back(tcp_socket);
		std::cout << "listening to TUIO/TCP messages from " << host << ":" << port << std::endl;
	}
}

TcpReceiver::~TcpReceiver() {	
	
}

void TcpReceiver::connect(bool lk) {
	
	if (connected) return;
	if ((int)tcp_socket<0) return;
	locked = lk;
	
	if (tcp_client_list.size()>0) {
		if (!locked) {
#ifndef WIN32
			pthread_create(&server_thread , NULL, ClientThreadFunc, this);
#else
			server_thread = CreateThread( 0, 0, ClientThreadFunc, this, 0, &ServerThreadId );
#endif
		} else ClientThreadFunc(this);		
	} else {
		if (!locked) {
#ifndef WIN32
			pthread_create(&server_thread , NULL, ServerThreadFunc, this);
#else
			server_thread = CreateThread( 0, 0, ServerThreadFunc, this, 0, &ServerThreadId );
#endif
		} else ServerThreadFunc(this);
	}

	connected = true;
}

void TcpReceiver::disconnect() {
	
	if (!connected) return;
	if ((int)tcp_socket<0) return;

#ifdef WIN32
	for (std::list<SOCKET>::iterator client = tcp_client_list.begin(); client!=tcp_client_list.end(); client++)
		closesocket((*client));
	closesocket(tcp_socket);
	if( server_thread ) CloseHandle( server_thread );
#else
	for (std::list<int>::iterator client = tcp_client_list.begin(); client!=tcp_client_list.end(); client++)
		close((*client));
	close(tcp_socket);
	server_thread = 0;
#endif	
	
	tcp_client_list.clear();
	if (!locked) {
#ifdef WIN32
		if( server_thread ) CloseHandle( server_thread );
#endif
		server_thread = 0;
	} else locked = false;

	connected = false;
}


