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

#ifndef INCLUDED_UDPRECEIVER_H
#define INCLUDED_UDPRECEIVER_H

#include "OscReceiver.h"
#include "oscpack/ip/UdpSocket.h"

namespace TUIO {
	
	/**
	 * The UdpReceiver provides the OscReceiver functionality for the UDP transport method 
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL UdpReceiver: public OscReceiver {
				
	public:

		/**
		 * The UDP socket is only public to be accessible from the thread function
		 */ 
		UdpListeningReceiveSocket *socket;
		
		/**
		 * This constructor creates a UdpReceiver instance listening to the provided UDP port 
		 *
		 * @param  port  the number of the UDP port to listen to, defaults to 3333
		 */
		UdpReceiver (int port=3333);

		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~UdpReceiver();
		
		/**
		 * The UdpReceiver connects and starts receiving TUIO messages via UDP
		 *
		 * @param  lock  running in the background if set to false (default)
		 */
		void connect(bool lock=false);
		
		/**
		 * The UdpReceiver disconnects and stops receiving TUIO messages via UDP
		 */
		void disconnect();
		
	private:

#ifndef WIN32
		pthread_t thread;
#else
		HANDLE thread;
#endif	
		
		bool locked;
	};
};
#endif /* INCLUDED_UDPRECEIVER_H */
