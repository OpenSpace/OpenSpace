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

#include "TuioClient.h"
#include "UdpReceiver.h"

using namespace TUIO2;
using namespace osc;


TuioClient::TuioClient()
: lateFrame     (false)
, source_count  (0)
, receiver		(NULL)
, local_receiver(true)
{
	receiver = new UdpReceiver();
	initialize();
}

TuioClient::TuioClient(unsigned short port)
: lateFrame     (false)
, source_count  (0)
, receiver		(NULL)
, local_receiver(true)
{
	receiver = new UdpReceiver(port);
	initialize();
}

TuioClient::TuioClient(OscReceiver *osc)
: lateFrame     (false)
, source_count  (0)
, receiver		(osc)
, local_receiver(false)
{
	initialize();
}

void TuioClient::initialize()	{	
	receiver->addTuioClient(this);
}

TuioClient::~TuioClient() {
	if (local_receiver) delete receiver;
}

void TuioClient::processOSC( const ReceivedMessage& msg ) {
    try {
        ReceivedMessageArgumentStream args = msg.ArgumentStream();
        //ReceivedMessage::const_iterator arg = msg.ArgumentsBegin();
        
        if( strcmp( msg.AddressPattern(), "/tuio2/frm" ) == 0 ) {
            //lockFrame();
            int32 fseq_raw,dim_raw;
            TimeTag timetag;
            const char* src_string;
            args >> fseq_raw >> timetag >> dim_raw >> src_string;
            
            // check if we know that source
            std::map<std::string,TuioSource*>::iterator iter = sourceList.find(src_string);

            if (iter==sourceList.end()) { // add a new source
                frameSource = new TuioSource(source_count, src_string, (unsigned int)dim_raw);
                sourceList[src_string] = frameSource;
                source_count++;
            } else { // use the found source
                 frameSource = sourceList[src_string];
            }
            
            unsigned int currentFrameID = (unsigned int)fseq_raw;
            frameTime = TuioTime(timetag);
            frameTime.setFrameID(currentFrameID);
            
            // frame sequence
            lateFrame = false;
            unsigned int lastFrameID = frameSource->getFrameTime().getFrameID();
            unsigned int timeDiff = frameTime.getTotalMilliseconds() - frameSource->getFrameTime().getTotalMilliseconds();
            frameSource->setFrameTime(frameTime);
            
            // drop late frames (but accept the reserved ID 0 and consider a possible reset after 1sec
            if ((currentFrameID<lastFrameID) && (currentFrameID!=0) && (timeDiff<1000)) lateFrame = true;
            
        } else if( strcmp( msg.AddressPattern(), "/tuio2/tok" ) == 0 ) {

            if (lateFrame) return;
            int32 s_id_raw, tu_id_raw, c_id_raw;
            unsigned short t_id, u_id;
            unsigned int s_id,c_id;
            float xpos, ypos, angle, xspeed, yspeed, rspeed, maccel, raccel;
            args >> s_id_raw >> tu_id_raw >> c_id_raw >> xpos >> ypos >> angle;
            if (!args.Eos()) args >> xspeed >> yspeed >> rspeed >> maccel >> raccel;
            else xspeed = yspeed = rspeed = maccel = raccel = 0.0f;
            
            s_id = (unsigned int)s_id_raw;
            c_id = (unsigned int)c_id_raw;
            t_id = tu_id_raw >> 16;
            u_id = tu_id_raw & 0x0000FFFF;
            
            TuioObject *tobj = getFrameObject(frameSource->getSourceID(),s_id);
            //if (tobj == NULL) std::cout << "new cont " << s_id << " " << frameSource.getSourceID() << std::endl;
            if (tobj == NULL) tobj = new TuioObject(frameTime,frameSource,s_id);
            addFrameObject(tobj);
            TuioToken *ttok = tobj->getTuioToken();
            if (ttok == NULL) {
                ttok = new TuioToken(frameTime,tobj,t_id,u_id,c_id,xpos,ypos,angle);
                tobj->setTuioToken(ttok);
            } else if ( (ttok->getX()!=xpos) || (ttok->getY()!=ypos) || (ttok->getAngle()!=angle) || (ttok->getXSpeed()!=xspeed) || (ttok->getYSpeed()!=yspeed) || (ttok->getRotationSpeed()!=rspeed) || (ttok->getMotionAccel()!=maccel) || (ttok->getRotationAccel()!=raccel) ) {

                ttok->update(frameTime,xpos,ypos,angle,xspeed,yspeed,rspeed,maccel,raccel);
            }
            
        } else if( strcmp( msg.AddressPattern(), "/tuio2/ptr" ) == 0 ) {

            if (lateFrame) return;
            int32 s_id_raw, tu_id_raw, c_id_raw;
            unsigned short t_id, u_id;
            unsigned int s_id,c_id;
            float xpos, ypos, angle, shear,radius, pressure, xspeed, yspeed, pspeed, maccel, paccel;
            args >> s_id_raw >> tu_id_raw >> c_id_raw >> xpos >> ypos >> angle >> shear >> radius >> pressure;
            if (!args.Eos()) args >> xspeed >> yspeed >> pspeed >> maccel >> paccel;
            else xspeed = yspeed = pspeed = maccel = paccel = 0.0f;
            
            s_id = (unsigned int)s_id_raw;
            c_id = (unsigned int)c_id_raw;
            t_id = tu_id_raw >> 16;
            u_id = tu_id_raw & 0x0000FFFF;
            
            TuioObject *tobj = getFrameObject(frameSource->getSourceID(),s_id);
            //if (tobj == NULL) std::cout << "new cont " << s_id << " " << frameSource.getSourceID() << std::endl;
            if (tobj == NULL) tobj = new TuioObject(frameTime,frameSource,s_id);
            addFrameObject(tobj);
            TuioPointer *tptr = tobj->getTuioPointer();
            if (tptr == NULL) {
                tptr = new TuioPointer(frameTime,tobj,t_id,u_id,c_id,xpos,ypos,angle,shear,radius,pressure);
                tobj->setTuioPointer(tptr);
               
            } else if ( (tptr->getX()!=xpos) || (tptr->getY()!=ypos) || (tptr->getAngle()!=angle) || (tptr->getShear()!=shear) || (tptr->getRadius()!=radius) || (tptr->getPressure()!=pressure) || (tptr->getXSpeed()!=xspeed) || (tptr->getYSpeed()!=yspeed) || (tptr->getPressureSpeed()!=pspeed) || (tptr->getMotionAccel()!=maccel) || (tptr->getPressureAccel()!=paccel) ) {
                
                tptr->update(frameTime,xpos,ypos,angle,shear,radius,pressure,xspeed,yspeed,pspeed,maccel,paccel);
            }
        } else if( strcmp( msg.AddressPattern(), "/tuio2/bnd" ) == 0 ) {

            if (lateFrame) return;
            int32 s_id_raw;
            unsigned int s_id;
            float xpos, ypos, angle, width, height, area;
            float xspeed, yspeed, rspeed, maccel, raccel;
            args >> s_id_raw >> xpos >> ypos >> angle >> width >> height >> area;
            if (!args.Eos()) args >> xspeed >> yspeed >> rspeed >> maccel >> raccel;
            else xspeed = yspeed = rspeed = maccel = raccel = 0.0f;
            
            s_id = (unsigned int)s_id_raw;
            TuioObject *tobj = getFrameObject(frameSource->getSourceID(),s_id);
            if (tobj == NULL) tobj = new TuioObject(frameTime,frameSource,s_id);
            addFrameObject(tobj);
            TuioBounds *tbnd = tobj->getTuioBounds();
            if (tbnd == NULL) {
                tbnd = new TuioBounds(frameTime,tobj,xpos,ypos,angle,width,height,area);
                tobj->setTuioBounds(tbnd);
            } else if ( (tbnd->getX()!=xpos) || (tbnd->getY()!=ypos) || (tbnd->getAngle()!=angle) || (tbnd->getWidth()!=width) || (tbnd->getHeight()!=height) || (tbnd->getArea()!=area) || (tbnd->getXSpeed()!=xspeed) || (tbnd->getYSpeed()!=yspeed) || (tbnd->getRotationSpeed()!=rspeed) || (tbnd->getMotionAccel()!=maccel) || (tbnd->getRotationAccel()!=raccel)) {
                
                tbnd->update(frameTime,xpos,ypos,angle,width,height,area,xspeed,yspeed,rspeed,maccel,raccel);
            }
        } else if( strcmp( msg.AddressPattern(), "/tuio2/sym" ) == 0 ) {

            if (lateFrame) return;
            int32 s_id_raw, tu_id_raw, c_id_raw;
            unsigned int s_id, c_id;
            unsigned short t_id, u_id;
            const char* type;
            const char* data;
            args >> s_id_raw >> tu_id_raw >> c_id_raw >> type >> data;
            
            s_id = (unsigned int)s_id_raw;
            c_id = (unsigned int)c_id_raw;
            t_id = tu_id_raw >> 16;
            u_id = tu_id_raw & 0x0000FFFF;
            
            TuioObject *tobj = getFrameObject(frameSource->getSourceID(),s_id);
            if (tobj == NULL) tobj = new TuioObject(frameTime,frameSource,s_id);
            addFrameObject(tobj);
            TuioSymbol *tsym = tobj->getTuioSymbol();
            if (tsym == NULL) {
                tsym = new TuioSymbol(frameTime,tobj,t_id,u_id,c_id,type,data);
                tobj->setTuioSymbol(tsym);
            } else {
                tsym->update(frameTime);
            }
            
        } else if( strcmp( msg.AddressPattern(), "/tuio2/chg" ) == 0 ) {
            if (lateFrame) return;
            int32 s_id_raw;
            args >> s_id_raw;
            unsigned int  s_id = (unsigned int)s_id_raw;
            
            std::list<TuioPoint> pointList;
            while(!args.Eos()) {
                float xpos, ypos;
                args >> xpos >> ypos;
                pointList.push_back(TuioPoint(frameTime, xpos, ypos));
            }
            
            TuioObject *tobj = getFrameObject(frameSource->getSourceID(),s_id);
            if (tobj == NULL) tobj = new TuioObject(frameTime,frameSource,s_id);
            addFrameObject(tobj);
            
            /*TuioGeometry *tgeo = tobj->getTuioGeometry();
            if (tgeo == NULL) {
                tgeo = new TuioGeometry(frameTime, tobj);
                tobj->setTuioGeometry(tgeo);
            } tgeo->setConvexHull(pointList);*/
            
        } else if( strcmp( msg.AddressPattern(), "/tuio2/ocg" ) == 0 ) {
            if (lateFrame) return;
            int32 s_id_raw;
            args >> s_id_raw;
            unsigned int s_id = (unsigned int)s_id_raw;
 
            std::list<TuioPoint> pointList;
            while(!args.Eos()) {
                float xpos, ypos;
                args >> xpos >> ypos;
                pointList.push_back(TuioPoint(frameTime, xpos, ypos));
            }
            
            TuioObject *tobj = getFrameObject(frameSource->getSourceID(),s_id);
            if (tobj == NULL) tobj = new TuioObject(frameTime,frameSource,s_id);
            addFrameObject(tobj);
            
            /*TuioGeometry *tgeo = tobj->getTuioGeometry();
            if (tgeo == NULL) {
                tgeo = new TuioGeometry(frameTime, tobj);
                tobj->setTuioGeometry(tgeo);
            } tgeo->setOuterContour(pointList);*/
 
        } else if( strcmp( msg.AddressPattern(), "/tuio2/icg" ) == 0 ) {
            if (lateFrame) return;
            int32 s_id_raw;
            args >> s_id_raw;
            unsigned int  s_id = (unsigned int)s_id_raw;
            
            std::list<TuioPoint> pointList;
            while(!args.Eos()) {
                float xpos,ypos;
                args >> xpos >> ypos;
                pointList.push_back(TuioPoint(frameTime, xpos, ypos));
            }
            
            TuioObject *tobj = getFrameObject(frameSource->getSourceID(),s_id);
            if (tobj == NULL) tobj = new TuioObject(frameTime,frameSource,s_id);
            addFrameObject(tobj);
            
            /*TuioGeometry *tgeo = tobj->getTuioGeometry();
             if (tgeo == NULL) {
             tgeo = new TuioGeometry(frameTime, tobj);
             tobj->setTuioGeometry(tgeo);
             } tgeo->setOuterContour(pointList);*/

        } else if( strcmp( msg.AddressPattern(), "/tuio2/alv" ) == 0 ) {
 
            if (lateFrame) return;
            int32 s_id;
            aliveObjectList.clear();
            while(!args.Eos()) {
                args >> s_id;
                aliveObjectList.push_back((unsigned int)s_id);
            }
            
            lockObjectList();
            //find the removed tobjs first
            for (std::list<TuioObject*>::iterator tobj=tobjList.begin(); tobj!=tobjList.end(); tobj++) {
                if ((*tobj)->getTuioSource()->getSourceID()!=frameSource->getSourceID()) continue;
                std::list<unsigned int>::iterator iter = find(aliveObjectList.begin(), aliveObjectList.end(), (*tobj)->getSessionID());
                if (iter == aliveObjectList.end()) {
                    (*tobj)->remove(frameTime);
                    addFrameObject(*tobj);
                }
            }
            unlockObjectList();
            
            for (std::list<TuioObject*>::iterator iter=frameObjectList.begin(); iter!=frameObjectList.end(); iter++) {
                TuioObject *tobj = (*iter);
                
                switch (tobj->getTuioState()) {
                    case TUIO_REMOVED:
                        
                        for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener!=listenerList.end(); listener++)
                            (*listener)->tuioRemove(tobj);
                        
                        lockObjectList();
                        for (std::list<TuioObject*>::iterator delcon=tobjList.begin(); delcon!=tobjList.end(); delcon++) {
                            if (((*delcon)->getSessionID()==tobj->getSessionID()) && ((*delcon)->getTuioSource()->getSourceID()==frameSource->getSourceID())) {
                                delete *delcon;
                                tobjList.erase(delcon);
                                break;
                            }
                        }
                        unlockObjectList();
                        break;
                    case TUIO_ADDED:
                        
                        lockObjectList();
                        tobjList.push_back(tobj);
                        unlockObjectList();
                        for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
                            (*listener)->tuioAdd(tobj);
                        
                        break;
                    default:
                        
                        for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
                            (*listener)->tuioUpdate(tobj);
                }
            }
            
            for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
                (*listener)->tuioRefresh(frameTime);
            
            frameObjectList.clear();
            //unlockFrame();
        }         
    } catch( Exception& e ){
        std::cerr << "error parsing TUIO2 message: "<< msg.AddressPattern() <<  " - " << e.what() << std::endl;
        //unlockFrame();
    }
}

bool TuioClient::isConnected() {	
	return receiver->isConnected();
}

void TuioClient::connect(bool lock) {
				
	receiver->connect(lock);
	unlockObjectList();
}

void TuioClient::disconnect() {
	
	receiver->disconnect();
	aliveObjectList.clear();

	for (std::list<TuioObject*>::iterator iter=tobjList.begin(); iter != tobjList.end(); iter++)
		delete (*iter);
	tobjList.clear();
}

TuioObject* TuioClient::getTuioObject(unsigned int src_id, unsigned int s_id) {
    lockObjectList();
    for (std::list<TuioObject*>::iterator iter=tobjList.begin(); iter != tobjList.end(); iter++) {
        if (((*iter)->getSessionID()==s_id) && ((*iter)->getTuioSource()->getSourceID()==src_id)) {
            unlockObjectList();
            return (*iter);
        }
    }
    unlockObjectList();
    return NULL;
}

TuioToken* TuioClient::getTuioToken(unsigned int src_id, unsigned int s_id) {
	lockObjectList();
	for (std::list<TuioObject*>::iterator iter=tobjList.begin(); iter != tobjList.end(); iter++) {
		if (((*iter)->getSessionID()==s_id) && ((*iter)->getTuioSource()->getSourceID()==src_id)) {
			unlockObjectList();
            return (*iter)->getTuioToken();
		}
	}	
	unlockObjectList();
	return NULL;
}

TuioPointer* TuioClient::getTuioPointer(unsigned int src_id, unsigned int s_id) {
    lockObjectList();
    for (std::list<TuioObject*>::iterator iter=tobjList.begin(); iter != tobjList.end(); iter++) {
        if (((*iter)->getSessionID()==s_id) && ((*iter)->getTuioSource()->getSourceID()==src_id)) {
            unlockObjectList();
            return (*iter)->getTuioPointer();
        }
    }
    unlockObjectList();
    return NULL;
}

TuioBounds* TuioClient::getTuioBounds(unsigned int src_id, unsigned int s_id) {
    lockObjectList();
    for (std::list<TuioObject*>::iterator iter=tobjList.begin(); iter != tobjList.end(); iter++) {
        if (((*iter)->getSessionID()==s_id) && ((*iter)->getTuioSource()->getSourceID()==src_id)) {
            unlockObjectList();
            return (*iter)->getTuioBounds();
        }
    }
    unlockObjectList();
    return NULL;
}

TuioSymbol* TuioClient::getTuioSymbol(unsigned int src_id, unsigned int s_id) {
    lockObjectList();
    for (std::list<TuioObject*>::iterator iter=tobjList.begin(); iter != tobjList.end(); iter++) {
        if (((*iter)->getSessionID()==s_id) && ((*iter)->getTuioSource()->getSourceID()==src_id)) {
            unlockObjectList();
            return (*iter)->getTuioSymbol();
        }
    }
    unlockObjectList();
    return NULL;
}

std::list<TuioObject*> TuioClient::getTuioObjectList(unsigned int src_id) {
    lockObjectList();
    std::list<TuioObject*> listBuffer;
    for (std::list<TuioObject*>::iterator tobj=tobjList.begin(); tobj != tobjList.end(); tobj++) {
        if ((*tobj)->getTuioSource()->getSourceID()==src_id) listBuffer.push_back(*tobj);
    }
    unlockObjectList();
    return listBuffer;
}

std::list<TuioToken*> TuioClient::getTuioTokenList(unsigned int src_id) {
    lockObjectList();
    std::list<TuioToken*> listBuffer;
    for (std::list<TuioObject*>::iterator tobj=tobjList.begin(); tobj != tobjList.end(); tobj++) {
        if ((*tobj)->getTuioSource()->getSourceID()==src_id) {
            TuioToken *ttok = (*tobj)->getTuioToken();
            if (ttok!=NULL) listBuffer.push_back(ttok);
        }
    }
    unlockObjectList();
    return listBuffer;
}

std::list<TuioPointer*> TuioClient::getTuioPointerList(unsigned int src_id) {
    lockObjectList();
    std::list<TuioPointer*> listBuffer;
    for (std::list<TuioObject*>::iterator tobj=tobjList.begin(); tobj != tobjList.end(); tobj++) {
        if ((*tobj)->getTuioSource()->getSourceID()==src_id) {
            TuioPointer *tptr = (*tobj)->getTuioPointer();
            if (tptr!=NULL) listBuffer.push_back(tptr);
        }
    }
    unlockObjectList();
    return listBuffer;
}

std::list<TuioBounds*> TuioClient::getTuioBoundsList(unsigned int src_id) {
    lockObjectList();
    std::list<TuioBounds*> listBuffer;
    for (std::list<TuioObject*>::iterator tobj=tobjList.begin(); tobj != tobjList.end(); tobj++) {
        if ((*tobj)->getTuioSource()->getSourceID()==src_id) {
            TuioBounds *tbnd = (*tobj)->getTuioBounds();
            if (tbnd!=NULL) listBuffer.push_back(tbnd);
        }
    }
    unlockObjectList();
    return listBuffer;
}

std::list<TuioSymbol*> TuioClient::getTuioSymbolList(unsigned int src_id) {
    lockObjectList();
    std::list<TuioSymbol*> listBuffer;
    for (std::list<TuioObject*>::iterator tobj=tobjList.begin(); tobj != tobjList.end(); tobj++) {
        if ((*tobj)->getTuioSource()->getSourceID()==src_id) {
            TuioSymbol *tsym = (*tobj)->getTuioSymbol();
            if (tsym!=NULL) listBuffer.push_back(tsym);
        }
    }
    unlockObjectList();
    return listBuffer;
}

void TuioClient::addFrameObject(TuioObject *tobj) {
    for (std::list<TuioObject*>::iterator iter=frameObjectList.begin(); iter != frameObjectList.end(); iter++) {
        if ((*iter)->getSessionID()==tobj->getSessionID()) return;
    }
    
    frameObjectList.push_back(tobj);
}

TuioObject* TuioClient::getFrameObject(unsigned int src_id, unsigned int s_id) {
    for (std::list<TuioObject*>::iterator tobj=frameObjectList.begin(); tobj != frameObjectList.end(); tobj++) {
        if ((*tobj)->getSessionID()==s_id) return *tobj;
    }
    
    for (std::list<TuioObject*>::iterator tobj=tobjList.begin(); tobj != tobjList.end(); tobj++) {
        if (((*tobj)->getSessionID()==s_id) && ((*tobj)->getTuioSource()->getSourceID()==src_id)) return *tobj;
    }
    
    return NULL;
}

void TuioClient::lockFrame() {
#ifndef WIN32
    pthread_mutex_lock(&frameMutex);
#else
    WaitForSingleObject(frameMutex, INFINITE);
#endif
}

void TuioClient::unlockFrame() {
#ifndef WIN32
    pthread_mutex_unlock(&frameMutex);
#else
    ReleaseMutex(frameMutex);
#endif
}
