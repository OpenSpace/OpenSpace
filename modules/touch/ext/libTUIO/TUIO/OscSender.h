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

#ifndef INCLUDED_OSCSENDER_H
#define INCLUDED_OSCSENDER_H

#include "LibExport.h"
#include "oscpack/osc/OscOutboundPacketStream.h"
#include "oscpack/osc/OscHostEndianness.h"
#include "oscpack/ip/NetworkingUtils.h"
#include <iostream>
#include <cstring>

namespace TUIO {
	
	/**
	 * The OscSender class is the base class for the various OSC transport methods such as UDP, TCP ...
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL OscSender {
				
	public:

		/**
		 * The constructor is doing nothing in particular. 
		 */
		OscSender (): local(true) {};

		/**
		 * The destructor is doing nothing in particular. 
		 */ 
		virtual ~OscSender() {}
		
		/**
		 * This method delivers the provided OSC data
		 *
		 * @param *bundle  the OSC stream to deliver
		 * @return true if the data was delivered successfully
		 */
		virtual bool sendOscPacket (osc::OutboundPacketStream *bundle) = 0;
		
		/**
		 * This method returns the connection state
		 *
		 * @return true if the connection is alive
		 */
		virtual bool isConnected () = 0;

		/**
		 * This method returns if this OscSender delivers locally
		 *
		 * @return true if this OscSender delivers locally
		 */
		bool isLocal () { return local; };
		
		/**
		 * This method returns the maximum bundle size in bytes
		 *
		 * @return the maximum bundle size in bytes
		 */
		int getBufferSize () { return buffer_size; };
	
		virtual const char* tuio_type() = 0;
		
	protected:
		unsigned int buffer_size;
		bool local;
	};
}


#endif /* INCLUDED_OSCSENDER_H */

