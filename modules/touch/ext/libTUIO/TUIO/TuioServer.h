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

#ifndef INCLUDED_TuioServer_H
#define INCLUDED_TuioServer_H

#include "TuioManager.h"
#include "UdpSender.h"
#include "TcpSender.h"
#include "WebSockSender.h"
#include "FlashSender.h"
#include <iostream>
#include <vector>
#include <stdio.h>
#ifndef WIN32
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>
#endif

namespace TUIO {
	/**
	 * <p>The TuioServer class is the central TUIO protocol encoder component.
	 * In order to encode and send TUIO messages an instance of TuioServer needs to be created. The TuioServer instance then generates TUIO messages
	 * which are deliverered by the provided OSCSender. The shown UDPSender send OSC to UDP port 3333 on localhost or to the configured host and port.</p> 
	 * <p>During runtime the each frame is marked with the initFrame and commitFrame methods, 
	 * while the currently present TuioObjects are managed by the server with ADD, UPDATE and REMOVE methods in analogy to the TuioClient's TuioListener interface.</p>
	 *<p>See the SimpleSimulator example project for further hints on how to use the TuioServer class and its various methods.
	 * <p><code>
	 * OscSender *sender = new UDPSender();</br>
	 * TuioServer *server = new TuioServer(sender);<br/>
	 * server->setSourceName("MyTuioSource"); // optional for TUIO 1.1<br/>	 
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * TuioObject *tobj = server->addTuioObject(xpos,ypos,angle);<br/>
	 * TuioCursor *tcur = server->addTuiCursor(xpos,ypos);<br/>
	 * TuioBlob *tblb = server->addTuioBlob(xpos,ypos,angle,width,height, area);<br/>
	 * server->commitFrame();<br/>
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * server->updateTuioObject(tobj,xpos,ypos,angle);<br/>
	 * server->updateTuioCursor(tcur,xpos,ypos);<br/>
	 * server->updateTuioBlob(tblb,xpos,ypos,angle,width,height,area);<br/>
	 * server->commitFrame();<br/>
	 * ...<br/>
	 * server->initFrame(TuioTime::getSessionTime());<br/>
	 * server->removeTuioObject(tobj);<br/>
	 * server->removeTuioCursor(tcur);<br/>
	 * server->removeTuioBlob(tblb);<br/>
	 * server->commitFrame();<br/>
	 * </code></p>
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
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
		TuioServer(const char *host, int port);
		
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
		 * Generates and sends TUIO messages of all currently active TuioObjects, TuioCursors and TuioBlobs
		 */
		void sendFullMessages();
		
		/**
		 * Enables the full update of all currently active and inactive TuioObjects, TuioCursors and TuioBlobs 
		 *
		 */
		void enableFullUpdate()  {
			full_update = true;
		}
		
		/**
		 * Disables the full update of all currently active and inactive TuioObjects, TuioCursors and TuioBlobs 
		 */
		void disableFullUpdate() {
			full_update = false;
		}

		/**
		 * Returns true if the full update of all currently active TuioObjects, TuioCursors and TuioBlobs is enabled.
		 * @return	true if the full update of all currently active TuioObjects, TuioCursors and TuioBlobs is enabled
		 */
		bool fullUpdateEnabled() {
			return full_update;
		}

		/**
		 * Disables the periodic full update of all currently active TuioObjects TuioObjects, TuioCursors and TuioBlobs
		 *
		 * @param	interval	update interval in seconds, defaults to one second
		 */
		void enablePeriodicMessages(int interval=1) {
			periodic_update =  true;
			update_interval = interval;
		}
		
		/**
		 * Disables the periodic full update of all currently active and inactive TuioObjects, TuioCursors and TuioBlobs
		 */
		void disablePeriodicMessages() {
			periodic_update = false;
		}
		
		/**
		 * Returns true if the periodic update of all currently active TuioObjects, TuioCursors and TuioBlobs is enabled.
		 * @return	true if the periodic update of all currently active TuioObjects, TuioCursors and TuioBlobs is enabled
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
		 * Generates and sends TUIO messages of all currently active and updated TuioObjects, TuioCursors and TuioBlobs.
		 */
		void commitFrame();

		/**
		 * Commits the current frame.
		 * Generates and sends TUIO messages of all currently active and updated TuioObjects, TuioCursors and TuioBlobs.
		 */
		
		/**
		 * Defines the name of this TUIO source, which is transmitted within the /tuio/[profile] source message.
		 *
		 * @param	name	the desired name of this TUIO source
		 */
		void setSourceName(const char *name);

		
		/**
		 * Defines the name and IP address of this TUIO source, which is transmitted within the /tuio/[profile] source message.
		 *
		 * @param	name	the desired name of this TUIO source
		 * @param	ip		the local IP address
		 */		
		void setSourceName(const char *name, const char *ip);
		
		
		
		void addOscSender(OscSender *sender);
		
		void enableObjectProfile(bool flag) { objectProfileEnabled = flag; };
		void enableCursorProfile(bool flag) { cursorProfileEnabled = flag; };
		void enableBlobProfile(bool flag) { blobProfileEnabled = flag; };
				
	private:
			
		void initialize(OscSender *oscsend);

		std::vector<OscSender*> senderList;
		void deliverOscPacket(osc::OutboundPacketStream  *packet);
		
		osc::OutboundPacketStream  *oscPacket;
		char *oscBuffer; 
		osc::OutboundPacketStream  *fullPacket;
		char *fullBuffer; 
		
		void startObjectBundle();
		void addObjectMessage(TuioObject *tobj);
		void sendObjectBundle(long fseq);
		void sendEmptyObjectBundle();

		void startCursorBundle();
		void addCursorMessage(TuioCursor *tcur);
		void sendCursorBundle(long fseq);
		void sendEmptyCursorBundle();

		void startBlobBundle();
		void addBlobMessage(TuioBlob *tblb);
		void sendBlobBundle(long fseq);
		void sendEmptyBlobBundle();
		
		int update_interval;
		bool full_update, periodic_update;
		TuioTime objectUpdateTime, cursorUpdateTime, blobUpdateTime ;
		bool objectProfileEnabled, cursorProfileEnabled, blobProfileEnabled;		
		char *source_name;
	};
}
#endif /* INCLUDED_TuioServer_H */
