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

#ifndef INCLUDED_OSCRECEIVER_H
#define INCLUDED_OSCRECEIVER_H

#include "LibExport.h"
#include "TuioClient.h"

#include "oscpack/osc/OscReceivedElements.h"
#include "oscpack/osc/OscHostEndianness.h"
#include "oscpack/ip/PacketListener.h"
#include "oscpack/ip/IpEndpointName.h"

namespace TUIO {

	class TuioClient; // Forward declaration

	/**
	 * The OscReceiver is the base class for the various OSC transport methods such as UDP, TCP ...
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL OscReceiver: public PacketListener {
				
	public:

		/**
		 * The constructor is doing nothing in particular. 
		 */
		OscReceiver() : connected(false) {};

		/**
		 * The destructor is doing nothing in particular. 
		 */
		virtual ~OscReceiver() {};
		
		/**
		 * The OscReceiver connects and starts receiving TUIO messages via OSC
		 *
		 * @param  lock  running in the background if set to false (default)
		 */
		virtual void connect(bool lock=false) = 0;
		
		/**
		 * The OscReceiver disconnects and stops receiving TUIO messages via OSC
		 */
		virtual void disconnect() = 0;
		
		/**
		 * Returns true if this OscReceiver is currently connected.
		 * @return	true if this OscReceiver is currently connected
		 */
		bool isConnected();
	
		/**
		 * Attaches the provided TuioClient to this OscReceiver
		 *
		 * @param  client	a pointer to the TuioClient to attach
		 */
		void addTuioClient(TuioClient *client);
		
		/**
		 * The OSC callback method where the incoming OSC data is received
		 *
		 * @param  data		the received OSC data
		 * @param  size		the size of the received OSC data
		 * @param  remoteEndpoint	the origin of the received OSC data 
		 */
		void ProcessPacket( const char *data, int size, const IpEndpointName &remoteEndpoint );
		
	protected:
		void ProcessBundle( const osc::ReceivedBundle& b, const IpEndpointName& remoteEndpoint);
		
		void ProcessMessage( const osc::ReceivedMessage& message, const IpEndpointName& remoteEndpoint);
		
		std::list<TuioClient*> clientList;
		bool connected;
	};
};
#endif /* INCLUDED_OSCRECEIVER_H */
