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

#ifndef INCLUDED_TCPRECEIVER_H
#define INCLUDED_TCPRECEIVER_H

#include "OscReceiver.h"
#define MAX_TCP_SIZE 65536

#ifdef WIN32
#include <winsock.h>
#include <io.h>
typedef int socklen_t;
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>
#endif

namespace TUIO {
	
	/**
	 * The TcpReceiver provides the OscReceiver functionality for the TCP transport method 
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TcpReceiver: public OscReceiver {
				
	public:
		
		/**
		 * This constructor creates a TcpReceiver instance listening to the provided TCP port 
		 *
		 * @param  port  the number of the TCP port to listen to, defaults to 3333
		 */
		TcpReceiver (int port=3333);

		/**
		 * This constructor creates a TcpReceiver connected to the provided host and TCP port 
		 *
		 * @param  host  the host name to connect
		 * @param  port  the number of the TCP port to listen to, defaults to 3333
		 */
		TcpReceiver (const char *host, int port);
		
		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~TcpReceiver();
		
		/**
		 * The TcpReceiver connects and starts receiving TUIO messages via TCP
		 *
		 * @param  lock  running in the background if set to false (default)
		 */
		void connect(bool lock=false);
		
		/**
		 * The TcpReceiver disconnects and stops receiving TUIO messages via TCP
		 */
		void disconnect();

#ifndef WIN32
		int tcp_socket;
		std::list<int> tcp_client_list;
#else
		SOCKET tcp_socket;
		std::list<SOCKET> tcp_client_list;
#endif
		
	private:

#ifndef WIN32
		pthread_t server_thread;
#else
		HANDLE server_thread;
		DWORD ServerThreadId;
#endif	
		
		bool locked;
	};
};
#endif /* INCLUDED_TcpReceiver_H */
