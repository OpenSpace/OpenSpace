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

#include "TcpSender.h"
using namespace TUIO;

#ifndef  WIN32
static void* ClientThreadFunc( void* obj )
#else
static DWORD WINAPI ClientThreadFunc( LPVOID obj )
#endif
{
	TcpSender *sender = static_cast<TcpSender*>(obj);
	char buf[16];
			
#ifdef WIN32
	SOCKET client = sender->tcp_client_list.back();
#else
	int client = sender->tcp_client_list.back();
#endif
	
	int connected = 1;
	while (connected) {
		connected = recv(client, buf, sizeof(buf),0);
	}
		
	sender->tcp_client_list.remove(client);
	std::cout << sender->tuio_type() << " connection closed"<< std::endl;
	if (sender->tcp_client_list.size()==0) sender->connected=false;
	//std::cout << sender->tcp_client_list.size() << " clients left"<< std::endl;	

	return 0;
};

#ifndef  WIN32
static void* ServerThreadFunc( void* obj )
#else
static DWORD WINAPI ServerThreadFunc( LPVOID obj )
#endif
{
	TcpSender *sender = static_cast<TcpSender*>(obj);
	struct sockaddr_in client_addr;
	socklen_t len = sizeof(client_addr);
	
	std::cout << sender->tuio_type() << " socket created on port " << sender->port_no << std::endl;
	while (sender->tcp_socket) {
#ifdef WIN32
		SOCKET tcp_client = -1;
#else
		int tcp_client = -1;
#endif
		
		tcp_client = accept(sender->tcp_socket, (struct sockaddr*)&client_addr, &len);
#ifdef WIN32
		 //win32 workaround on exit
		if (!client_addr.sin_addr.S_un.S_addr && !client_addr.sin_port) return 0;
		if ((client_addr.sin_addr.S_un.S_addr==3435973836) && (client_addr.sin_port==52428)) return 0;
#endif

		if (tcp_client>0) { 
			std::cout << sender->tuio_type() << " client connected from " << inet_ntoa(client_addr.sin_addr) << "@" << client_addr.sin_port << std::endl;
			sender->tcp_client_list.push_back(tcp_client);
			sender->connected=true;
			sender->newClient(tcp_client);
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

TcpSender::TcpSender()
	:connected (false)
{
	local = true;
	buffer_size = MAX_TCP_SIZE;
	
	tcp_socket = socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );
	if (tcp_socket < 0) {
		std::cerr << "could not create " << tuio_type() << " socket" << std::endl;
		return;
	}
	
	struct sockaddr_in tcp_server;
	memset( &tcp_server, 0, sizeof (tcp_server));
	//unsigned long addr = inet_addr("127.0.0.1");
	//memcpy( (char *)&tcp_server.sin_addr, &addr, sizeof(addr));
	
	tcp_server.sin_family = AF_INET;
	tcp_server.sin_port = htons(3333);
	tcp_server.sin_addr.s_addr = inet_addr("127.0.0.1");
	
	int ret = connect(tcp_socket,(struct sockaddr*)&tcp_server,sizeof(tcp_server));
	if (ret<0) {
		std::cerr << "could not open " << tuio_type() << " connection to 127.0.0.1:3333" << std::endl;
		return;
	} else {
		std::cout << tuio_type() << " connection opened to 127.0.0.1:3333" << std::endl;
		tcp_client_list.push_back(tcp_socket);
		connected = true;
		
#ifndef WIN32
		pthread_create(&server_thread , NULL, ClientThreadFunc,this);
#else
		HANDLE server_thread = CreateThread( 0, 0, ClientThreadFunc, this, 0, &ServerThreadId );
#endif

	}

}

TcpSender::TcpSender(const char *host, int port) 
	:connected (false)
{	
	if ((strcmp(host,"127.0.0.1")==0) || (strcmp(host,"localhost")==0)) {
		local = true;
	} else local = false;
	buffer_size = MAX_TCP_SIZE;
	
	tcp_socket = socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );
	if (tcp_socket < 0) {
		std::cerr << "could not create  " << tuio_type() << " socket" << std::endl;
		return;
	}

	struct sockaddr_in tcp_server;
	memset( &tcp_server, 0, sizeof (tcp_server));
	unsigned long addr = inet_addr(host);
	if (addr != INADDR_NONE) {
		memcpy( (char *)&tcp_server.sin_addr, &addr, sizeof(addr));
	} else {
		struct hostent *host_info = gethostbyname(host);
		if (host_info == NULL) {
			std::cerr << "unknown host name: " << host << std::endl;
			throw std::exception();
		}
		memcpy( (char *)&tcp_server.sin_addr, host_info->h_addr, host_info->h_length );
	}

	tcp_server.sin_family = AF_INET;
	tcp_server.sin_port = htons(port);

	int ret = connect(tcp_socket,(struct sockaddr*)&tcp_server,sizeof(tcp_server));
	if (ret<0) {
#ifdef WIN32
		closesocket(tcp_socket);
#else
		close(tcp_socket);
#endif	
		std::cerr << "could not open " << tuio_type() << " connection to " << host << ":"<< port << std::endl;
		throw std::exception();
	} else {
		std::cout << tuio_type() << " connection opened to " << host << ":"<< port << std::endl;
		tcp_client_list.push_back(tcp_socket);
		connected = true;
		
#ifndef WIN32
		pthread_create(&server_thread , NULL, ClientThreadFunc,this);
#else
		HANDLE server_thread = CreateThread( 0, 0, ClientThreadFunc, this, 0, &ServerThreadId );
#endif

	}
}

TcpSender::TcpSender(int port)
	:connected (false)
{
	local = false;
	buffer_size = MAX_TCP_SIZE;
	port_no = port;
	
	tcp_socket = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (tcp_socket < 0) {
		std::cerr << "could not create TUIO/TCP socket" << std::endl;
		throw std::exception();
	}

	int optval = 1;
	#ifdef  WIN32
	int ret = setsockopt(tcp_socket,SOL_SOCKET,SO_REUSEADDR, (const char *)&optval,  sizeof(int));
	#else
	int ret = setsockopt(tcp_socket,SOL_SOCKET,SO_REUSEADDR, (const void *)&optval,  sizeof(int));
	#endif
	if (ret < 0) {
		std::cerr << "could not reuse TUIO/TCP socket address" << std::endl;
		throw std::exception();
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
		throw std::exception();
	}
	
	ret =  listen(tcp_socket, 1);
	if (ret < 0) {
		std::cerr << "could not start listening to TUIO/TCP socket" << std::endl;
#ifdef WIN32
		closesocket(tcp_socket);
#else
		close(tcp_socket);
#endif
		throw std::exception();
	}
				
#ifndef WIN32
	pthread_create(&server_thread , NULL, ServerThreadFunc, this);
#else
	DWORD ServerThreadId;
	server_thread = CreateThread( 0, 0, ServerThreadFunc, this, 0, &ServerThreadId );
#endif
	
}

bool TcpSender::isConnected() {
	return connected;
}


TcpSender::~TcpSender() {
#ifdef WIN32

	for (std::list<SOCKET>::iterator client = tcp_client_list.begin(); client!=tcp_client_list.end(); client++) {
		closesocket((*client));
	}
	closesocket(tcp_socket);

	if( server_thread ) CloseHandle( server_thread );

#else
		for (std::list<int>::iterator client = tcp_client_list.begin(); client!=tcp_client_list.end(); client++) {
		close((*client));
	}
	close(tcp_socket);
	tcp_socket = 0;
	server_thread = 0;
#endif		
}


bool TcpSender::sendOscPacket (osc::OutboundPacketStream *bundle) {
	if (!connected) return false; 
	if ( bundle->Size() > buffer_size ) return false;
	if ( bundle->Size() == 0 ) return false;
	
#ifdef OSC_HOST_LITTLE_ENDIAN             
	data_size[0] =  bundle->Size()>>24;
	data_size[1] = (bundle->Size()>>16) & 255;
	data_size[2] = (bundle->Size()>>8) & 255;
	data_size[3] = (bundle->Size()) & 255;
#else
	*((int32_t*)data_size) = bundle->Size();
#endif

#ifdef WIN32
	std::list<SOCKET>::iterator client;
#else
	std::list<int>::iterator client;
#endif
	
	for (client = tcp_client_list.begin(); client!=tcp_client_list.end(); client++) {
		//send((*client), data_size, 4,0);
		//send((*client), bundle->Data(), bundle->Size(),0);
		memcpy(&data_buffer[0], &data_size, 4);
		memcpy(&data_buffer[4], bundle->Data(), bundle->Size());
		send((*client),data_buffer, 4+bundle->Size(),0);
	}

	return true;
}

void TcpSender::newClient( int tcp_client ) { }
