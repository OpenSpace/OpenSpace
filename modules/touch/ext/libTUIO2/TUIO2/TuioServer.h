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

#ifndef INCLUDED_TUIOSERVER_H
#define INCLUDED_TUIOSERVER_H

#include "TuioManager.h"
#include "UdpSender.h"
#include <iostream>
#include <vector>
#include <stdio.h>
#ifndef WIN32
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>
#endif

#define TOK_MESSAGE_SIZE 108
#define PTR_MESSAGE_SIZE 68
#define BND_MESSAGE_SIZE 116
#define SYM_MESSAGE_SIZE 116
#define ALV_MESSAGE_SIZE 20

namespace TUIO2 {
	/**
	 * <p>The TuioServer class is the central TUIO protocol encoder component.
	 * In order to encode and send TUIO messages an instance of TuioServer needs to be created. The TuioServer instance then generates TUIO messages
	 * which are deliverered by the provided OSCSender. The shown UDPSender send OSC to UDP port 3333 on localhost or to the configured host and port.</p> 
	 * <p>During runtime the each frame is marked with the initFrame and commitFrame methods, 
	 * while the currently present TuioTokens are managed by the server with ADD, UPDATE and REMOVE methods in analogy to the TuioClient's TuioListener interface.</p>
	 *<p>See the SimpleSimulator example project for further hints on how to use the TuioServer class and its various methods.
	 * <p><code>
	 * OscSender *sender = new UDPSender();</br>
	 * TuioServer *server = new TuioServer(sender);<br/>
	 * server->setSource(src); // passes a TuioSource* argument<br/>
	 * ...<br/>
	 * server->initTuioFrame(TuioTime::getSessionTime());<br/>
	 * TuioToken   *ttok = server->addTuioToken(xpos,ypos,angle);<br/>
	 * TuioPointer *tptr = server->addTuioPointer(xpos,ypos,width,pressure);<br/>
	 * TuioBounds  *tbnd = server->addTuioBounds(xpos,ypos,angle,width,height,area);<br/>
	 * server->commitTuioFrame();<br/>
	 * ...<br/>
	 * server->initTuioFrame(TuioTime::getSessionTime());<br/>
	 * server->updateTuioToken(ttok,xpos,ypos,angle);<br/>
	 * server->updateTuioPointer(tptr,xpos,ypos,width,pressure);<br/>
	 * server->updateTuioBounds(tbnd,xpos,ypos,angle,width,height,area);<br/>
	 * server->commitTuioFrame();<br/>
	 * ...<br/>
	 * server->initTuioFrame(TuioTime::getSessionTime());<br/>
	 * server->removeTuioToken(ttok);<br/>
	 * server->removeTuioPointer(tptr);<br/>
	 * server->removeTuioBounds(tbnd);<br/>
	 * server->commitTuioFrame();<br/>
	 * </code></p>
	 *
	 * @author Martin Kaltenbrunner
	 * @version 2.0.a0
	 */ 
	class LIBDECL TuioServer : public TuioManager { 
	
	public:

		/**
		 * This constructor creates a TuioServer that uses an internal UdpSender delivering the OSC data via UDP port 3333 on localhost
		 */
		TuioServer();

		/**
		 * This constructor creates a TuioServer that uses an internal UdpSender delivering the OSC data via the provided UDP port on the provided host
		 *
		 * @param  host  the host name for UDP deleivery
		 * @param  port  the UDP port number on the provided host
		 */
		TuioServer(const char *host, unsigned short port);
		
		/**
		 * This constructor creates a TuioServer that sends OSC data using the provided OscSender
		 *
		 * @param  sender  the OscSender used for OSC data delivery
		 */
		TuioServer(OscSender *sender);

		/**
		 * The destructor is doing nothing in particular. 
		 */
		~TuioServer();

		/**
		 * Generates and sends TUIO messages of all currently active TUIO Components
		 */
		void sendFullTuioBundle();
		
		/**
		 * Enables the full update of all currently active and inactive TUIO Components
		 *
		 */
		void enableFullUpdate()  {
			full_update = true;
		}
		
		/**
		 * Disables the full update of all currently active and inactive TUIO Components
		 */
		void disableFullUpdate() {
			full_update = false;
		}

		/**
		 * Returns true if the full update of all currently active TUIO Components is enabled.
		 * @return	true if the full update of all currently active TUIO Components is enabled
		 */
		bool fullUpdateEnabled() {
			return full_update;
		}

		/**
		 * Disables the periodic full update of all currently active TUIO Components
		 *
		 * @param	interval	update interval in seconds, defaults to one second
		 */
		void enablePeriodicMessages(int interval=1) {
			periodic_update =  true;
			update_interval = interval;
		}
		
		/**
		 * Disables the periodic full update of all currently active and inactive TUIO Components
		 */
		void disablePeriodicMessages() {
			periodic_update = false;
		}
		
		/**
		 * Returns true if the periodic update of all currently active TUIO Components is enabled.
		 * @return	true if the periodic update of all currently active TUIO Components is enabled
		 */
		bool periodicMessagesEnabled() {
			return periodic_update;
		}
		
		/**
		 * Returns the periodic update interval in seconds.
		 * @return	the periodic update interval in seconds
		 */
		int getUpdateInterval() {
			return update_interval;
		}
		
		/**
		 * Commits the current frame.
		 * Generates and sends TUIO messages of all currently active and updated TUIO Components.
		 */
		void commitTuioFrame();
		
		/**
		 * Creates the TuioSource that is transmitted within the /tuio2/frm source attributes.
		 *
		 * @param	name	the source name to assign
		 */		
		void setSourceName(const char* name);
        void setDimension(unsigned short w, unsigned short h);
		
		void addOscSender(OscSender *sender);
				
	private:
			
		void initialize();
		
		OscSender *primary_sender;
		bool local_sender;

		std::vector<OscSender*> senderList;
		void deliverOscPacket(osc::OutboundPacketStream  *packet);
		
		osc::OutboundPacketStream *oscPacket;
		char *oscBuffer; 
		osc::OutboundPacketStream *fullPacket;
		char *fullBuffer; 
		
        void checkBundleCapacity(int size);
		void startTuioBundle(unsigned int fseq);
		void addTokenMessage(TuioToken *ttok);
        void addPointerMessage(TuioPointer *tptr);
        void addBoundsMessage(TuioBounds *tbnd);
        void addSymbolMessage(TuioSymbol *tsym);
		void sendTuioBundle();
        void sendEmptyTuioBundle();
		
		int update_interval;
		bool full_update, periodic_update;
		TuioTime updateTime;
		TuioSource *source;
	};
}
#endif /* INCLUDED_TUIOSERVER_H */
