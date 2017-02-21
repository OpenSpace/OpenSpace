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

#ifndef INCLUDED_TUIOCLIENT_H
#define INCLUDED_TUIOCLIENT_H

#include "TuioDispatcher.h"
#include "OscReceiver.h"
#include "oscpack/osc/OscReceivedElements.h"

#include <iostream>
#include <list>
#include <map>
#include <algorithm>
#include <string>
#include <cstring>

namespace TUIO {
	
	class OscReceiver; // Forward declaration
	
	/**
	 * <p>The TuioClient class is the central TUIO protocol decoder component. It provides a simple callback infrastructure using the {@link TuioListener} interface.
	 * In order to receive and decode TUIO messages an instance of TuioClient needs to be created. The TuioClient instance then generates TUIO events
	 * which are broadcasted to all registered classes that implement the {@link TuioListener} interface.</p> 
	 * <p><code>
	 * TuioClient *client = new TuioClient();<br/>
	 * client->addTuioListener(myTuioListener);<br/>
	 * client->connect();<br/>
	 * </code></p>
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL TuioClient : public TuioDispatcher { 
		
	public:
		/**
		 * This constructor creates a TuioClient that uses an internal UdpReceiver listening to the default UDP port 3333
		 *
		 */
		TuioClient();
	
		/**
		 * This constructor creates a TuioClient that uses an internal UdpReceiver listening to the provided UDP port
		 *
		 * @param  port  the UDP port the internal UdpReceiver is listening to
		 */
		TuioClient(int port);
		
		/**
		 * This constructor creates a TuioClient that uses the provided OscReceiver for the incoming OSC data
		 *
		 * @param  oscreceiver  the OscReceiver implementation for the chosen transport method (UDP, TCP ...)
		 */
		TuioClient(OscReceiver *oscreceiver);

		/**
		 * The destructor is doing nothing in particular. 
		 */
		~TuioClient();

		/**
		 * The TuioClient connects and starts receiving TUIO messages from its associated OscReceiver
		 *
		 * @param  lock  running in the background if set to false (default)
		 */
		void connect(bool lock=false);
		
		/**
		 * The TuioClient disconnects and stops receiving TUIO messages from its associated OscReceiver
		 */
		void disconnect();
		
		/**
		 * Returns true if this TuioClient is currently connected.
		 * @return	true if this TuioClient is currently connected
		 */
		bool isConnected();

		/**
		 * Returns a List of all currently active TuioObjects
		 *
		 * @return  a List of TuioObjects
		 */
		std::list<TuioObject*> getTuioObjects() {
			return TuioDispatcher::getTuioObjects();
		}
		
		/**
		 * Returns a List of all currently active TuioObjects
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List of TuioObjects
		 */
		std::list<TuioObject*> getTuioObjects(int source_id);

		/**
		 * Returns a List with a copy of all currently active TuioObjects
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List with a copy of TuioObjects
		 */
		std::list<TuioObject> copyTuioObjects(int source_id);

		
		/**
		 * Returns a List with a copy of all currently active TuioObjects
		 *
		 * @return  a List with a copy of TuioObjects
		 */
		std::list<TuioObject> copyTuioObjects() {
			return TuioDispatcher::copyTuioObjects();
		}
		
		/**
		 * Returns the TuioObject corresponding to the provided Session ID
		 * or NULL if the Session ID does not refer to an active TuioObject
		 *
		 * @param  s_id  the session ID of the corresponding TuioObject
		 * @return  an active TuioObject corresponding to the provided Session ID or NULL
		 */
		TuioObject* getTuioObject(long s_id) {
			return getTuioObject(0,s_id);
		};
		
		/**
		 * Returns the TuioObject corresponding to the provided Session ID
		 * which is associated to the given Source ID
		 * or NULL if the Session ID does not refer to an active TuioObject
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @param  s_id  the session ID of the corresponding TuioObject
		 * @return  an active TuioObject corresponding to the provided Session ID or NULL
		 */
		TuioObject* getTuioObject(int src_id, long s_id);

		/**
		 * Returns a List of all currently active TuioCursors
		 *
		 * @return  a List of TuioCursors
		 */
		std::list<TuioCursor*> getTuioCursors() {
			return TuioDispatcher::getTuioCursors();
		}
		
		/**
		 * Returns a List of all currently active TuioCursors
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List of TuioCursors
		 */
		std::list<TuioCursor*> getTuioCursors(int source_id);

		/**
		 * Returns a List with a copy of all currently active TuioCursors
		 *
		 * @return  a List with a copy of TuioCursors
		 */
		std::list<TuioCursor> copyTuioCursors() {
			return TuioDispatcher::copyTuioCursors();
		}
		
		/**
		 * Returns a List with a copy of all currently active TuioCursors
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List with a copy of TuioCursors
		 */
		std::list<TuioCursor> copyTuioCursors(int source_id);
		
		/**
		 * Returns the TuioCursor corresponding to the provided Session ID
		 * or NULL if the Session ID does not refer to an active TuioCursor
		 *
		 * @param  s_id  the session ID of the corresponding TuioCursor
		 * @return  an active TuioCursor corresponding to the provided Session ID or NULL
		 */
		TuioCursor* getTuioCursor(long s_id) {
			return getTuioCursor(0,s_id);
		};
		
		/**
		 * Returns the TuioCursor corresponding to the provided Session ID
		 * which is associated to the given Source ID
		 * or NULL if the Session ID does not refer to an active TuioCursor
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @param  s_id  the session ID of the corresponding TuioCursor
		 * @return  an active TuioCursor corresponding to the provided Session ID or NULL
		 */
		TuioCursor* getTuioCursor(int src_id, long s_id);

		/**
		 * Returns a List of all currently active TuioBlobs
		 *
		 * @return  a List of TuioBlobs
		 */
		std::list<TuioBlob*> getTuioBlobs() {
			return TuioDispatcher::getTuioBlobs();
		}
		
		/**
		 * Returns a List of all currently active TuioBlobs
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List of TuioBlobs
		 */
		std::list<TuioBlob*> getTuioBlobs(int source_id);

		/**
		 * Returns a List with a copy of all currently active TuioBlobs
		 *
		 * @return  a List with a copy of TuioBlobs
		 */
		std::list<TuioBlob> copyTuioBlobs() {
			return TuioDispatcher::copyTuioBlobs();
		}
		
		/**
		 * Returns a List with a copy of all currently active TuioBlobs
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List with a copy of TuioBlobs
		 */
		std::list<TuioBlob> copyTuioBlobs(int source_id);
		
		/**
		 * Returns the TuioBlob corresponding to the provided Session ID
		 * or NULL if the Session ID does not refer to an active TuioBlob
		 *
		 * @param  s_id  the session ID of the corresponding TuioBlob
		 * @return  an active TuioBlob corresponding to the provided Session ID or NULL
		 */
		TuioBlob* getTuioBlob(long s_id) {
			return getTuioBlob(0,s_id);
		};
		
		/**
		 * Returns the TuioBlob corresponding to the provided Session ID
		 * which is associated to the given Source ID
		 * or NULL if the Session ID does not refer to an active TuioBlob
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @param  s_id  the session ID of the corresponding TuioBlob
		 * @return  an active TuioBlob corresponding to the provided Session ID or NULL
		 */
		TuioBlob* getTuioBlob(int src_id, long s_id);
		
		void processOSC( const osc::ReceivedMessage& message);
		
	private:
		void initialize();
		
		std::list<TuioObject*> frameObjects;
		std::list<long> aliveObjectList;
		std::list<TuioCursor*> frameCursors;
		std::list<long> aliveCursorList;
		std::list<TuioBlob*> frameBlobs;
		std::list<long> aliveBlobList;
		
		osc::int32 currentFrame;
		TuioTime currentTime;
			
		std::list<TuioCursor*> freeCursorList, freeCursorBuffer;
		std::map<int,int> maxCursorID;

		std::list<TuioBlob*> freeBlobList, freeBlobBuffer;
		std::map<int,int> maxBlobID;
		
		std::map<std::string,int> sourceList;
		int source_id;
		char *source_name;
		char *source_addr;
		
		OscReceiver *receiver;
		bool local_receiver;
	};
};
#endif /* INCLUDED_TUIOCLIENT_H */
