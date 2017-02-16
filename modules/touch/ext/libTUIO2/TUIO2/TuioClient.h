/*
 TUIO2 C++ Library
 Copyright (c) 2009-2014 Martin Kaltenbrunner <martin@tuio.org>
 
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

namespace TUIO2 {
	
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
	 * @version 2.0.a0
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
		TuioClient(unsigned short port);
		
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
         * Returns the TuioObject corresponding to the provided Session ID
         * which is associated to the given Source ID
         * or NULL if the Session ID does not refer to an active TuioObject
         *
         * @param  src_id  the source ID of the corresponding TUIO source
         * @param  s_id  the session ID of the corresponding TuioObject
         * @return  an active TuioObject corresponding to the provided Session ID or NULL
         */
        using TuioDispatcher::getTuioObject;
        TuioObject* getTuioObject(unsigned int src_id, unsigned int s_id);
        
        /**
         * Returns a list of all currently active TuioObject
         * which are associated to the given Source ID
         *
         * @param  src_id  the source ID of the corresponding TUIO source
         * @return  a list of TuioObject
         */
        using TuioDispatcher::getTuioObjectList;
        std::list<TuioObject*> getTuioObjectList(unsigned int src_id);
        
        /**
         * Returns the TuioToken corresponding to the provided Session ID
         * which is associated to the given Source ID
         * or NULL if the Session ID does not refer to an active TuioToken
         *
         * @param  src_id  the source ID of the corresponding TUIO source
         * @param  s_id  the session ID of the corresponding TuioToken
         * @return  an active TuioToken corresponding to the provided Session ID or NULL
         */
        using TuioDispatcher::getTuioToken;
        TuioToken* getTuioToken(unsigned int src_id, unsigned int s_id);

		/**
		 * Returns a list of all currently active TuioTokens
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a list of TuioTokens
		 */
        using TuioDispatcher::getTuioTokenList;
		std::list<TuioToken*> getTuioTokenList(unsigned int src_id);
	
        /**
         * Returns the TuioPointer corresponding to the provided Session ID
         * which is associated to the given Source ID
         * or NULL if the Session ID does not refer to an active TuioPointer
         *
         * @param  src_id  the source ID of the corresponding TUIO source
         * @param  s_id  the session ID of the corresponding TuioPointer
         * @return  an active TuioPointer corresponding to the provided Session ID or NULL
         */
        using TuioDispatcher::getTuioPointer;
        TuioPointer* getTuioPointer(unsigned int src_id, unsigned int s_id);
        
		/**
		 * Returns a List of all currently active TuioPointers
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List of TuioPointers
		 */
        using TuioDispatcher::getTuioPointerList;
		std::list<TuioPointer*> getTuioPointerList(unsigned int src_id);
		
        /**
         * Returns the TuioBounds corresponding to the provided Session ID
         * which is associated to the given Source ID
         * or NULL if the Session ID does not refer to an active TuioBounds
         *
         * @param  src_id  the source ID of the corresponding TUIO source
         * @param  s_id  the session ID of the corresponding TuioBounds
         * @return  an active TuioBounds corresponding to the provided Session ID or NULL
         */
        using TuioDispatcher::getTuioBounds;
        TuioBounds* getTuioBounds(unsigned int src_id, unsigned int s_id);
        
		/**
		 * Returns a List of all currently active TuioBounds
		 * which are associated to the given Source ID
		 *
		 * @param  src_id  the source ID of the corresponding TUIO source
		 * @return  a List of TuioBounds
		 */
        using TuioDispatcher::getTuioBoundsList;
		std::list<TuioBounds*> getTuioBoundsList(unsigned int src_id);
        
        /**
         * Returns the TuioSymbol corresponding to the provided Session ID
         * which is associated to the given Source ID
         * or NULL if the Session ID does not refer to an active TuioSymbol
         *
         * @param  src_id  the source ID of the corresponding TUIO source
         * @param  s_id  the session ID of the corresponding TuioSymbol
         * @return  an active TuioSymbol corresponding to the provided Session ID or NULL
         */
        using TuioDispatcher::getTuioSymbol;
        TuioSymbol* getTuioSymbol(unsigned int src_id, unsigned int s_id);
        
        /**
         * Returns a List of all currently active TuioSymbol
         * which are associated to the given Source ID
         *
         * @param  src_id  the source ID of the corresponding TUIO source
         * @return  a List of TuioSymbol
         */
         using TuioDispatcher::getTuioSymbolList;
         std::list<TuioSymbol*> getTuioSymbolList(unsigned int src_id);

        /**
         * Parses the incoming OSC message
         *
         * @param  message  the incoming OSC message
         */
         void processOSC( const osc::ReceivedMessage& message);
		
	private:
		void initialize();
		
        void addFrameObject(TuioObject *con);
        TuioObject* getFrameObject(unsigned int src_id,unsigned int s_id);
        std::list<unsigned int> aliveObjectList;
		std::list<TuioObject*> frameObjectList;
		
        TuioTime frameTime;
        bool lateFrame;
			
        unsigned int source_count;
        std::map<std::string,TuioSource*> sourceList;
        TuioSource *frameSource;
        
		OscReceiver *receiver;
		bool local_receiver;
        
#ifndef WIN32
        pthread_mutex_t frameMutex;
#else
        HANDLE frameMutex;
#endif
        
        void lockFrame();
        void unlockFrame();
	};
};
#endif /* INCLUDED_TUIOCLIENT_H */
