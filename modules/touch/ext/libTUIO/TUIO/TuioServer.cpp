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

#include "TuioServer.h"
#include "UdpSender.h"

using namespace TUIO;
using namespace osc;

TuioServer::TuioServer() 
	:full_update			(false)
	,periodic_update		(false)	
	,objectProfileEnabled	(true)
	,cursorProfileEnabled	(true)
	,blobProfileEnabled		(true)
	,source_name			(NULL)
{
	OscSender *oscsend = new UdpSender();
	initialize(oscsend);
}

TuioServer::TuioServer(const char *host, int port) 
:full_update			(false)
,periodic_update		(false)	
,objectProfileEnabled	(true)
,cursorProfileEnabled	(true)
,blobProfileEnabled		(true)
,source_name			(NULL)
{
	OscSender *oscsend = new UdpSender(host,port);
	initialize(oscsend);
}

TuioServer::TuioServer(OscSender *oscsend)
	:full_update			(false)
	,periodic_update		(false)	
	,objectProfileEnabled	(true)
	,cursorProfileEnabled	(true)
	,blobProfileEnabled		(true)
	,source_name			(NULL)
{
	initialize(oscsend);
}

void TuioServer::initialize(OscSender *oscsend) {
	
	senderList.push_back(oscsend);
	int size = oscsend->getBufferSize();
	oscBuffer = new char[size];
	oscPacket = new osc::OutboundPacketStream(oscBuffer,size);
	fullBuffer = new char[size];
	fullPacket = new osc::OutboundPacketStream(oscBuffer,size);
	
	objectUpdateTime = TuioTime(currentFrameTime);
	cursorUpdateTime = TuioTime(currentFrameTime);
	blobUpdateTime = TuioTime(currentFrameTime);
	
	if (cursorProfileEnabled) sendEmptyCursorBundle();
	if (objectProfileEnabled) sendEmptyObjectBundle();
	if (blobProfileEnabled) sendEmptyBlobBundle();
	
	invert_x = false;
	invert_y = false;
	invert_a = false;	
}

TuioServer::~TuioServer() {

	initFrame(TuioTime::getSessionTime());
	stopUntouchedMovingCursors();
	stopUntouchedMovingObjects();
	stopUntouchedMovingBlobs();
	
	initFrame(TuioTime::getSessionTime());
	removeUntouchedStoppedCursors();
	removeUntouchedStoppedObjects();
	removeUntouchedStoppedBlobs();
	
	if (cursorProfileEnabled) sendEmptyCursorBundle();
	if (objectProfileEnabled) sendEmptyObjectBundle();
	if (blobProfileEnabled) sendEmptyBlobBundle();
	
	delete []oscBuffer;
	delete oscPacket;
	delete []fullBuffer;
	delete fullPacket;
	
	if (source_name) delete[] source_name;
	for (unsigned int i=0;i<senderList.size();i++)
		delete senderList[i];
}


void TuioServer::addOscSender(OscSender *sender) {

	// add source address if previously local
	/*if ((source_name) && (primary_sender->isLocal()) && (senderList.size()==1)) {
		setSourceName(source_name);
	}*/ 
	
	// resize packets to smallest transport method
	unsigned int size = sender->getBufferSize();
	if (size<oscPacket->Capacity()) {
		osc::OutboundPacketStream *temp = oscPacket;
		oscPacket = new osc::OutboundPacketStream(oscBuffer,size);
		delete temp;
		temp = fullPacket;
		fullPacket = new osc::OutboundPacketStream(oscBuffer,size);
		delete temp;
		
	}
	
	senderList.push_back(sender);
}

void TuioServer::deliverOscPacket(osc::OutboundPacketStream  *packet) {

	for (unsigned int i=0;i<senderList.size();i++)
		senderList[i]->sendOscPacket(packet);
}

void TuioServer::setSourceName(const char *name, const char *ip) {
	if (!source_name) source_name = new char[256];
	sprintf(source_name,"%s@%s",name,ip);
}


void TuioServer::setSourceName(const char *src) {
	
	if (!source_name) source_name = new char[256];

	/*if (senderList[0]->isLocal()) {
		sprintf(source_name,"%s",src);
	} else {*/
		char hostname[64];
		char *source_addr = NULL;
		struct hostent *hp = NULL;
		struct in_addr *addr = NULL;
		
		gethostname(hostname, 64);
		hp = gethostbyname(hostname);
		
		if (hp==NULL) {
			sprintf(hostname, "%s.local", hostname);
			hp = gethostbyname(hostname);
		}
		
		if (hp!=NULL) {
			for (int i = 0; hp->h_addr_list[i] != 0; ++i) {
				addr = (struct in_addr *)(hp->h_addr_list[i]);
				//std::cout << inet_ntoa(*addr) << std::endl;
				source_addr = inet_ntoa(*addr);
			}
		} else {
			//generate a random internet address
			srand ( (unsigned int)time(NULL) );
			int32 r = rand();
			addr = (struct in_addr*)&r;
			source_addr = inet_ntoa(*addr);
		}
		sprintf(source_name,"%s@%s",src,source_addr);
	//}
	
	std::cout << "tuio/src " << source_name << std::endl;
}

void TuioServer::commitFrame() {
	TuioManager::commitFrame();
		
	if(updateObject) {
		startObjectBundle();
		for (std::list<TuioObject*>::iterator  tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++) {
			
			// start a new packet if we exceed the packet capacity
			if ((oscPacket->Capacity()-oscPacket->Size())<OBJ_MESSAGE_SIZE) {
				sendObjectBundle(currentFrame);
				startObjectBundle();
			}
			TuioObject *tobj = (*tuioObject);
			if  ((full_update) || (tobj->getTuioTime()==currentFrameTime)) addObjectMessage(tobj);
		}
		objectUpdateTime = TuioTime(currentFrameTime);
		sendObjectBundle(currentFrame);
	} else if (objectProfileEnabled && periodic_update) {
		
		TuioTime timeCheck = currentFrameTime - objectUpdateTime;
		if(timeCheck.getSeconds()>=update_interval) {
			objectUpdateTime = TuioTime(currentFrameTime);
			startObjectBundle();
			if  (full_update) {
				for (std::list<TuioObject*>::iterator  tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++) {
					// start a new packet if we exceed the packet capacity
					if ((oscPacket->Capacity()-oscPacket->Size())<OBJ_MESSAGE_SIZE) {
						sendObjectBundle(currentFrame);
						startObjectBundle();
					}
					addObjectMessage(*tuioObject);
				}
			}
			sendObjectBundle(currentFrame);
		}
	}
	updateObject = false;

	if(updateCursor) {
		startCursorBundle();
		for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++) {
			
			// start a new packet if we exceed the packet capacity
			if ((oscPacket->Capacity()-oscPacket->Size())<CUR_MESSAGE_SIZE) {
				sendCursorBundle(currentFrame);
				startCursorBundle();
			}
			TuioCursor *tcur = (*tuioCursor);
			if ((full_update) || (tcur->getTuioTime()==currentFrameTime)) addCursorMessage(tcur);				
		}
		cursorUpdateTime = TuioTime(currentFrameTime);
		sendCursorBundle(currentFrame);
	} else if (cursorProfileEnabled && periodic_update) {
		TuioTime timeCheck = currentFrameTime - cursorUpdateTime;
		if(timeCheck.getSeconds()>=update_interval) {
			cursorUpdateTime = TuioTime(currentFrameTime);
			startCursorBundle();
			if (full_update) {
				for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++) {
					// start a new packet if we exceed the packet capacity
					if ((oscPacket->Capacity()-oscPacket->Size())<CUR_MESSAGE_SIZE) {
						sendCursorBundle(currentFrame);
						startCursorBundle();
					}
					addCursorMessage(*tuioCursor);
				}
			}
			sendCursorBundle(currentFrame);
		}
	}
	updateCursor = false;
	
	if(updateBlob) {
		startBlobBundle();
		for (std::list<TuioBlob*>::iterator tuioBlob =blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++) {
			// start a new packet if we exceed the packet capacity
			if ((oscPacket->Capacity()-oscPacket->Size())<BLB_MESSAGE_SIZE) {
				sendBlobBundle(currentFrame);
				startBlobBundle();
			}
			TuioBlob *tblb = (*tuioBlob);
			if ((full_update) || (tblb->getTuioTime()==currentFrameTime)) addBlobMessage(tblb);		
		}
		blobUpdateTime = TuioTime(currentFrameTime);
		sendBlobBundle(currentFrame);
	} else if (blobProfileEnabled && periodic_update) {
		TuioTime timeCheck = currentFrameTime - blobUpdateTime;
		if(timeCheck.getSeconds()>=update_interval) {
			blobUpdateTime = TuioTime(currentFrameTime);
			startBlobBundle();
			if (full_update) {
				for (std::list<TuioBlob*>::iterator tuioBlob =blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++) {
				
					// start a new packet if we exceed the packet capacity
					if ((oscPacket->Capacity()-oscPacket->Size())<BLB_MESSAGE_SIZE) {
						sendBlobBundle(currentFrame);
						startBlobBundle();
					}
					addBlobMessage(*tuioBlob);
				}
			}
			sendBlobBundle(currentFrame);
		}
	}
	updateBlob = false;
}

void TuioServer::sendEmptyCursorBundle() {
	oscPacket->Clear();	
	(*oscPacket) << osc::BeginBundleImmediate;
	if (source_name) (*oscPacket) << osc::BeginMessage( "/tuio/2Dcur") << "source" << source_name << osc::EndMessage;
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dcur") << "alive" << osc::EndMessage;	
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dcur") << "fseq" << -1 << osc::EndMessage;
	(*oscPacket) << osc::EndBundle;
	deliverOscPacket( oscPacket );
}

void TuioServer::startCursorBundle() {	
	oscPacket->Clear();	
	(*oscPacket) << osc::BeginBundleImmediate;
	if (source_name) (*oscPacket) << osc::BeginMessage( "/tuio/2Dcur") << "source" << source_name << osc::EndMessage;
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dcur") << "alive";
	for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++) {
		if ((*tuioCursor)->getTuioState()!=TUIO_ADDED) (*oscPacket) << (int32)((*tuioCursor)->getSessionID());
	}
	(*oscPacket) << osc::EndMessage;	
}

void TuioServer::addCursorMessage(TuioCursor *tcur) {
	
	if (tcur->getTuioState()==TUIO_ADDED) return;

	float xpos = tcur->getX();
	float xvel = tcur->getXSpeed();
	if (invert_x) {
		xpos = 1 - xpos;
		xvel = -1 * xvel;
	}
	float ypos = tcur->getY();
	float yvel = tcur->getYSpeed();
	if (invert_y) {
		ypos = 1 - ypos;
		yvel = -1 * yvel;
	}

	(*oscPacket) << osc::BeginMessage( "/tuio/2Dcur") << "set";
	(*oscPacket) << (int32)(tcur->getSessionID()) << xpos << ypos;
	(*oscPacket) << xvel << yvel << tcur->getMotionAccel();	
	(*oscPacket) << osc::EndMessage;
}

void TuioServer::sendCursorBundle(long fseq) {
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dcur") << "fseq" << (int32)fseq << osc::EndMessage;
	(*oscPacket) << osc::EndBundle;
	deliverOscPacket( oscPacket );
}

void TuioServer::sendEmptyObjectBundle() {
	oscPacket->Clear();	
	(*oscPacket) << osc::BeginBundleImmediate;
	if (source_name) (*oscPacket) << osc::BeginMessage( "/tuio/2Dobj") << "source" << source_name << osc::EndMessage;
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dobj") << "alive" << osc::EndMessage;	
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dobj") << "fseq" << -1 << osc::EndMessage;
	(*oscPacket) << osc::EndBundle;
	deliverOscPacket( oscPacket );
}

void TuioServer::startObjectBundle() {
	oscPacket->Clear();	
	(*oscPacket) << osc::BeginBundleImmediate;
	if (source_name) (*oscPacket) << osc::BeginMessage( "/tuio/2Dobj") << "source" << source_name << osc::EndMessage;
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dobj") << "alive";
	for (std::list<TuioObject*>::iterator tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++) {
		(*oscPacket) << (int32)((*tuioObject)->getSessionID());	
	}
	(*oscPacket) << osc::EndMessage;
}

void TuioServer::addObjectMessage(TuioObject *tobj) {
	
	float xpos = tobj->getX();
	float xvel = tobj->getXSpeed();
	if (invert_x) {
		xpos = 1 - xpos;
		xvel = -1 * xvel;
	}
	float ypos = tobj->getY();
	float yvel = tobj->getYSpeed();
	if (invert_y) {
		ypos = 1 - ypos;
		yvel = -1 * yvel;
	}
	float angle = tobj->getAngle();
	float rvel = tobj->getRotationSpeed();
	if (invert_a) {
		angle = 2.0f*(float)M_PI - angle;
		rvel = -1 * rvel;
	}
	
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dobj") << "set";
	(*oscPacket) << (int32)(tobj->getSessionID()) << tobj->getSymbolID() << xpos << ypos << angle;
	(*oscPacket) << xvel << yvel << rvel << tobj->getMotionAccel() << tobj->getRotationAccel();	
	(*oscPacket) << osc::EndMessage;
}

void TuioServer::sendObjectBundle(long fseq) {
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dobj") << "fseq" << (int32)fseq << osc::EndMessage;
	(*oscPacket) << osc::EndBundle;
	deliverOscPacket( oscPacket );
}


void TuioServer::sendEmptyBlobBundle() {
	oscPacket->Clear();	
	(*oscPacket) << osc::BeginBundleImmediate;
	if (source_name) (*oscPacket) << osc::BeginMessage( "/tuio/2Dblb") << "source" << source_name << osc::EndMessage;
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dblb") << "alive" << osc::EndMessage;	
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dblb") << "fseq" << -1 << osc::EndMessage;
	(*oscPacket) << osc::EndBundle;
	deliverOscPacket( oscPacket );
}

void TuioServer::startBlobBundle() {	
	oscPacket->Clear();	
	(*oscPacket) << osc::BeginBundleImmediate;
	if (source_name) (*oscPacket) << osc::BeginMessage( "/tuio/2Dblb") << "source" << source_name << osc::EndMessage;
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dblb") << "alive";
	for (std::list<TuioBlob*>::iterator tuioBlob = blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++) {
		if ((*tuioBlob)->getTuioState()!=TUIO_ADDED) (*oscPacket) << (int32)((*tuioBlob)->getSessionID());
	}
	(*oscPacket) << osc::EndMessage;	
}

void TuioServer::addBlobMessage(TuioBlob *tblb) {
	
	if (tblb->getTuioState()==TUIO_ADDED) return;
	
	float xpos = tblb->getX();
	float xvel = tblb->getXSpeed();
	if (invert_x) {
		xpos = 1 - xpos;
		xvel = -1 * xvel;
	}
	float ypos = tblb->getY();
	float yvel = tblb->getYSpeed();
	if (invert_y) {
		ypos = 1 - ypos;
		yvel = -1 * yvel;
	}
	float angle = tblb->getAngle();
	float rvel = tblb->getRotationSpeed();
	if (invert_a) {
		angle = 2.0f*(float)M_PI - angle;
		rvel = -1 * rvel;
	}
	
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dblb") << "set";
	(*oscPacket) << (int32)(tblb->getSessionID()) << xpos << ypos << angle << tblb->getWidth() << tblb->getHeight() << tblb->getArea();
	(*oscPacket) << xvel << yvel  << rvel << tblb->getMotionAccel()  << tblb->getRotationAccel();	
	(*oscPacket) << osc::EndMessage;
}

void TuioServer::sendBlobBundle(long fseq) {
	(*oscPacket) << osc::BeginMessage( "/tuio/2Dblb") << "fseq" << (int32)fseq << osc::EndMessage;
	(*oscPacket) << osc::EndBundle;

	deliverOscPacket( oscPacket );
}

void TuioServer::sendFullMessages() {
	
	// prepare the cursor packet
	fullPacket->Clear();
	(*fullPacket) << osc::BeginBundleImmediate;
	if (source_name) (*fullPacket) << osc::BeginMessage( "/tuio/2Dcur") << "source" << source_name << osc::EndMessage;
	// add the cursor alive message
	(*fullPacket) << osc::BeginMessage( "/tuio/2Dcur") << "alive";
	for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++)
		(*fullPacket) << (int32)((*tuioCursor)->getSessionID());	
	(*fullPacket) << osc::EndMessage;	
	
	// add all current cursor set messages
	for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++) {
		
		// start a new packet if we exceed the packet capacity
		if ((fullPacket->Capacity()-fullPacket->Size())<CUR_MESSAGE_SIZE) {
			
			// add the immediate fseq message and send the cursor packet
			(*fullPacket) << osc::BeginMessage( "/tuio/2Dcur") << "fseq" << -1 << osc::EndMessage;
			(*fullPacket) << osc::EndBundle;
			deliverOscPacket( fullPacket );
			
			// prepare the new cursor packet
			fullPacket->Clear();	
			(*fullPacket) << osc::BeginBundleImmediate;
			if (source_name) (*fullPacket) << osc::BeginMessage( "/tuio/2Dcur") << "source" << source_name << osc::EndMessage;
			// add the cursor alive message
			(*fullPacket) << osc::BeginMessage( "/tuio/2Dcur") << "alive";
			for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++)
				(*fullPacket) << (int32)((*tuioCursor)->getSessionID());	
			(*fullPacket) << osc::EndMessage;				
		}
		
		float xpos = (*tuioCursor)->getX();
		float xvel = (*tuioCursor)->getXSpeed();
		if (invert_x) {
			xpos = 1 - xpos;
			xvel = -1 * xvel;
		}
		float ypos = (*tuioCursor)->getY();
		float yvel = (*tuioCursor)->getYSpeed();
		if (invert_y) {
			ypos = 1 - ypos;
			yvel = -1 * yvel;
		}
		
		// add the actual cursor set message
		(*fullPacket) << osc::BeginMessage( "/tuio/2Dcur") << "set";
		(*fullPacket) << (int32)((*tuioCursor)->getSessionID()) << xpos << ypos;
		(*fullPacket) << xvel << yvel <<(*tuioCursor)->getMotionAccel();	
		(*fullPacket) << osc::EndMessage;	
	}
	
	// add the immediate fseq message and send the cursor packet
	(*fullPacket) << osc::BeginMessage( "/tuio/2Dcur") << "fseq" << -1 << osc::EndMessage;
	(*fullPacket) << osc::EndBundle;
	deliverOscPacket( fullPacket );
	
	// prepare the object packet
	fullPacket->Clear();
	(*fullPacket) << osc::BeginBundleImmediate;
	if (source_name) (*fullPacket) << osc::BeginMessage( "/tuio/2Dobj") << "source" << source_name << osc::EndMessage;
	// add the object alive message
	(*fullPacket) << osc::BeginMessage( "/tuio/2Dobj") << "alive";
	for (std::list<TuioObject*>::iterator tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++)
		(*fullPacket) << (int32)((*tuioObject)->getSessionID());	
	(*fullPacket) << osc::EndMessage;	
	
	for (std::list<TuioObject*>::iterator tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++) {
		
		// start a new packet if we exceed the packet capacity
		if ((fullPacket->Capacity()-fullPacket->Size())<OBJ_MESSAGE_SIZE) {
			// add the immediate fseq message and send the object packet
			(*fullPacket) << osc::BeginMessage( "/tuio/2Dobj") << "fseq" << -1 << osc::EndMessage;
			(*fullPacket) << osc::EndBundle;
			deliverOscPacket( fullPacket );
			
			// prepare the new object packet
			fullPacket->Clear();	
			(*fullPacket) << osc::BeginBundleImmediate;
			if (source_name) (*fullPacket) << osc::BeginMessage( "/tuio/2Dobj") << "source" << source_name << osc::EndMessage;
			// add the object alive message
			(*fullPacket) << osc::BeginMessage( "/tuio/2Dobj") << "alive";
			for (std::list<TuioObject*>::iterator tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++)
				(*fullPacket) << (int32)((*tuioObject)->getSessionID());	
			(*fullPacket) << osc::EndMessage;	
		}
		
		float xpos = (*tuioObject)->getX();
		float xvel = (*tuioObject)->getXSpeed();
		if (invert_x) {
			xpos = 1 - xpos;
			xvel = -1 * xvel;
		}
		float ypos = (*tuioObject)->getY();
		float yvel = (*tuioObject)->getYSpeed();
		if (invert_y) {
			ypos = 1 - ypos;
			yvel = -1 * yvel;
		}
		float angle = (*tuioObject)->getAngle();
		float rvel = (*tuioObject)->getRotationSpeed();
		if (invert_a) {
			angle =  2.0f*(float)M_PI - angle;
			rvel = -1 * rvel;
		}
		
		// add the actual object set message
		(*fullPacket) << osc::BeginMessage( "/tuio/2Dobj") << "set";
		(*fullPacket) << (int32)((*tuioObject)->getSessionID()) << (*tuioObject)->getSymbolID() << xpos << ypos << angle;
		(*fullPacket) << xvel << yvel << rvel << (*tuioObject)->getMotionAccel() << (*tuioObject)->getRotationAccel();	
		(*fullPacket) << osc::EndMessage;
		
	}
	// add the immediate fseq message and send the object packet
	(*fullPacket) << osc::BeginMessage( "/tuio/2Dobj") << "fseq" << -1 << osc::EndMessage;
	(*fullPacket) << osc::EndBundle;
	deliverOscPacket( fullPacket );
	
	// prepare the blob packet
	fullPacket->Clear();
	(*fullPacket) << osc::BeginBundleImmediate;
	if (source_name) (*fullPacket) << osc::BeginMessage( "/tuio/2Dblb") << "source" << source_name << osc::EndMessage;
	// add the object alive message
	(*fullPacket) << osc::BeginMessage( "/tuio/2Dblb") << "alive";
	for (std::list<TuioBlob*>::iterator tuioBlob = blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++)
		(*fullPacket) << (int32)((*tuioBlob)->getSessionID());	
	(*fullPacket) << osc::EndMessage;	
	
	for (std::list<TuioBlob*>::iterator tuioBlob = blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++) {
		
		// start a new packet if we exceed the packet capacity
		if ((fullPacket->Capacity()-fullPacket->Size())<BLB_MESSAGE_SIZE) {
			// add the immediate fseq message and send the object packet
			(*fullPacket) << osc::BeginMessage( "/tuio/2Dblb") << "fseq" << -1 << osc::EndMessage;
			(*fullPacket) << osc::EndBundle;
			deliverOscPacket( fullPacket );
			
			// prepare the new blob packet
			fullPacket->Clear();	
			(*fullPacket) << osc::BeginBundleImmediate;
			if (source_name) (*fullPacket) << osc::BeginMessage( "/tuio/2Dblb") << "source" << source_name << osc::EndMessage;
			// add the blob alive message
			(*fullPacket) << osc::BeginMessage( "/tuio/2Dblb") << "alive";
			for (std::list<TuioBlob*>::iterator tuioBlob = blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++)
				(*fullPacket) << (int32)((*tuioBlob)->getSessionID());	
			(*fullPacket) << osc::EndMessage;	
		}
		
		float xpos = (*tuioBlob)->getX();
		float xvel = (*tuioBlob)->getXSpeed();
		if (invert_x) {
			xpos = 1 - xpos;
			xvel = -1 * xvel;
		}
		float ypos = (*tuioBlob)->getY();
		float yvel = (*tuioBlob)->getYSpeed();
		if (invert_y) {
			ypos = 1 - ypos;
			yvel = -1 * yvel;
		}
		float angle = (*tuioBlob)->getAngle();
		float rvel = (*tuioBlob)->getRotationSpeed();
		if (invert_a) {
			angle = 2.0f*(float)M_PI - angle;
			rvel = -1 * rvel;
		}		
		
		// add the actual blob set message
		(*fullPacket) << osc::BeginMessage( "/tuio/2Dblb") << "set";
		(*fullPacket) << (int32)((*tuioBlob)->getSessionID()) << xpos << ypos  << angle << (*tuioBlob)->getWidth() << (*tuioBlob)->getHeight() << (*tuioBlob)->getArea();
		(*fullPacket) << xvel << yvel << rvel << (*tuioBlob)->getMotionAccel() << (*tuioBlob)->getRotationAccel();	
		(*fullPacket) << osc::EndMessage;
		
	}
	// add the immediate fseq message and send the blob packet
	(*fullPacket) << osc::BeginMessage( "/tuio/2Dblb") << "fseq" << -1 << osc::EndMessage;
	(*fullPacket) << osc::EndBundle;
	deliverOscPacket( fullPacket );
}



