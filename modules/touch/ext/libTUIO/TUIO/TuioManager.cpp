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

#include "TuioManager.h"
using namespace TUIO;


TuioManager::TuioManager() 
	: currentFrameTime(TuioTime::getSystemTime())
	, currentFrame(-1)
	, maxCursorID(-1)
	, maxBlobID(-1)
	, sessionID(-1)
	, updateObject(false)
	, updateCursor(false)
	, updateBlob(false)
	, verbose(false)
	, invert_x(false)
	, invert_y(false)
	, invert_a(false)
{

}

TuioManager::~TuioManager() {
}


TuioObject* TuioManager::addTuioObject(int f_id, float x, float y, float a) {
	sessionID++;
	TuioObject *tobj = new TuioObject(currentFrameTime, sessionID, f_id, x, y, a);
	objectList.push_back(tobj);
	updateObject = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->addTuioObject(tobj);

	if (verbose)
		std::cout << "add obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() << std::endl;

	return tobj;
}

void TuioManager::addExternalTuioObject(TuioObject *tobj) {
	if (tobj==NULL) return;
	tobj->setSessionID(sessionID++);
	objectList.push_back(tobj);
	updateObject = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->addTuioObject(tobj);

	if (verbose)
		std::cout << "add obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() << std::endl;
}

void TuioManager::updateTuioObject(TuioObject *tobj, float x, float y, float a) {
	if (tobj==NULL) return;
	if (tobj->getTuioTime()==currentFrameTime) return;
	tobj->update(currentFrameTime,x,y,a);
	updateObject = true;

	if (tobj->isMoving()) {
		for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
			(*listener)->updateTuioObject(tobj);
		
		if (verbose)	
			std::cout << "set obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() 
			<< " " << tobj->getXSpeed() << " " << tobj->getYSpeed() << " " << tobj->getRotationSpeed() << " " << tobj->getMotionAccel() << " " << tobj->getRotationAccel() << std::endl;
	}	
}

void TuioManager::updateExternalTuioObject(TuioObject *tobj) {
	if (tobj==NULL) return;
	updateObject = true;

	if (tobj->isMoving()) {
		for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
			(*listener)->updateTuioObject(tobj);
		
		if (verbose)	
			std::cout << "set obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() 
			<< " " << tobj->getXSpeed() << " " << tobj->getYSpeed() << " " << tobj->getRotationSpeed() << " " << tobj->getMotionAccel() << " " << tobj->getRotationAccel() << std::endl;
	}
}

void TuioManager::removeTuioObject(TuioObject *tobj) {
	if (tobj==NULL) return;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->removeTuioObject(tobj);
	
	if (verbose)
		std::cout << "del obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << ")" << std::endl;
    
    objectList.remove(tobj);
    delete tobj;
    updateObject = true;
}

void TuioManager::removeExternalTuioObject(TuioObject *tobj) {
	if (tobj==NULL) return;
	objectList.remove(tobj);
	updateObject = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->removeTuioObject(tobj);

	if (verbose)
		std::cout << "del obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << ")" << std::endl;
}

TuioCursor* TuioManager::addTuioCursor(float x, float y) {
	sessionID++;
	
	int cursorID = (int)cursorList.size();
	if ((int)(cursorList.size())<=maxCursorID) {
		std::list<TuioCursor*>::iterator closestCursor = freeCursorList.begin();
		
		for(std::list<TuioCursor*>::iterator iter = freeCursorList.begin();iter!= freeCursorList.end(); iter++) {
			if((*iter)->getDistance(x,y)<(*closestCursor)->getDistance(x,y)) closestCursor = iter;
		}
		
		TuioCursor *freeCursor = (*closestCursor);
		cursorID = (*closestCursor)->getCursorID();
		freeCursorList.erase(closestCursor);
		delete freeCursor;
	} else maxCursorID = cursorID;	
	
	TuioCursor *tcur = new TuioCursor(currentFrameTime, sessionID, cursorID, x, y);
	cursorList.push_back(tcur);
	updateCursor = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->addTuioCursor(tcur);
	
	if (verbose) 
		std::cout << "add cur " << tcur->getCursorID() << " (" <<  tcur->getSessionID() << ") " << tcur->getX() << " " << tcur->getY() << std::endl;

	return tcur;
}

void TuioManager::addExternalTuioCursor(TuioCursor *tcur) {
	if (tcur==NULL) return;
	tcur->setSessionID(sessionID++);
	cursorList.push_back(tcur);
	updateCursor = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->addTuioCursor(tcur);

	if (verbose) 
		std::cout << "add cur " << tcur->getCursorID() << " (" <<  tcur->getSessionID() << ") " << tcur->getX() << " " << tcur->getY() << std::endl;
}

void TuioManager::updateTuioCursor(TuioCursor *tcur,float x, float y) {
	if (tcur==NULL) return;
	//if (tcur->getTuioTime()==currentFrameTime) return;
	tcur->update(currentFrameTime,x,y);
	updateCursor = true;

	if (tcur->isMoving()) {	
		for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
			(*listener)->updateTuioCursor(tcur);

		if (verbose)	 	
			std::cout << "set cur " << tcur->getCursorID() << " (" <<  tcur->getSessionID() << ") " << tcur->getX() << " " << tcur->getY() 
			<< " " << tcur->getXSpeed() << " " << tcur->getYSpeed() << " " << tcur->getMotionAccel() << " " << std::endl;
	}
}

void TuioManager::updateExternalTuioCursor(TuioCursor *tcur) {
	if (tcur==NULL) return;
	updateCursor = true;
	
	if (tcur->isMoving()) {	
		for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
			(*listener)->updateTuioCursor(tcur);
				
		if (verbose)		
			std::cout << "set cur " << tcur->getCursorID() << " (" <<  tcur->getSessionID() << ") " << tcur->getX() << " " << tcur->getY() 
			<< " " << tcur->getXSpeed() << " " << tcur->getYSpeed() << " " << tcur->getMotionAccel() << " " << std::endl;
	}
}

void TuioManager::removeTuioCursor(TuioCursor *tcur) {
	if (tcur==NULL) return;

	cursorList.remove(tcur);
	tcur->remove(currentFrameTime);
	updateCursor = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->removeTuioCursor(tcur);

	if (verbose)
		std::cout << "del cur " << tcur->getCursorID() << " (" <<  tcur->getSessionID() << ")" << std::endl;

	if (tcur->getCursorID()==maxCursorID) {
		maxCursorID = -1;
		delete tcur;
		
		if (cursorList.size()>0) {
			std::list<TuioCursor*>::iterator clist;
			for (clist=cursorList.begin(); clist != cursorList.end(); clist++) {
				int cursorID = (*clist)->getCursorID();
				if (cursorID>maxCursorID) maxCursorID=cursorID;
			}
			
			freeCursorBuffer.clear();
			for (std::list<TuioCursor*>::iterator flist=freeCursorList.begin(); flist != freeCursorList.end(); flist++) {
				TuioCursor *freeCursor = (*flist);
				if (freeCursor->getCursorID()>maxCursorID) delete freeCursor;
				else freeCursorBuffer.push_back(freeCursor);
			}
			
			freeCursorList = freeCursorBuffer;

		} else {
			for (std::list<TuioCursor*>::iterator flist=freeCursorList.begin(); flist != freeCursorList.end(); flist++) {
				TuioCursor *freeCursor = (*flist);
				delete freeCursor;
			}
			freeCursorList.clear();
		}
	} else if (tcur->getCursorID()<maxCursorID) {
		freeCursorList.push_back(tcur);	
	}
}

void TuioManager::removeExternalTuioCursor(TuioCursor *tcur) {
	if (tcur==NULL) return;
	cursorList.remove(tcur);
	updateCursor = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->removeTuioCursor(tcur);

	if (verbose)
		std::cout << "del cur " << tcur->getCursorID() << " (" <<  tcur->getSessionID() << ")" << std::endl;
}

TuioBlob* TuioManager::addTuioBlob(float x, float y, float a, float w, float h, float f) {
	sessionID++;
	
	int blobID = (int)blobList.size();
	if ((int)(blobList.size())<=maxBlobID) {
		std::list<TuioBlob*>::iterator closestBlob = freeBlobList.begin();
		
		for(std::list<TuioBlob*>::iterator iter = freeBlobList.begin();iter!= freeBlobList.end(); iter++) {
			if((*iter)->getDistance(x,y)<(*closestBlob)->getDistance(x,y)) closestBlob = iter;
		}
		
		TuioBlob *freeBlob = (*closestBlob);
		blobID = (*closestBlob)->getBlobID();
		freeBlobList.erase(closestBlob);
		delete freeBlob;
	} else maxBlobID = blobID;	
	
	TuioBlob *tblb = new TuioBlob(currentFrameTime, sessionID, blobID, x, y, a, w, h, f);
	blobList.push_back(tblb);
	updateBlob = true;
	
	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->addTuioBlob(tblb);
	
	if (verbose) 
		std::cout << "add blb " << tblb->getBlobID() << " (" <<  tblb->getSessionID() << ") " << tblb->getX() << " " << tblb->getY()  << " " << tblb->getAngle() << " " << tblb->getWidth() << " " << tblb->getHeight() << " " << tblb->getArea() << std::endl;
	
	return tblb;
}

void TuioManager::addExternalTuioBlob(TuioBlob *tblb) {
	if (tblb==NULL) return;
	
	int blobID = (int)blobList.size();
	if (blobID <= maxBlobID) {
		std::list<TuioBlob*>::iterator closestBlob = freeBlobList.begin();
		
		for(std::list<TuioBlob*>::iterator iter = freeBlobList.begin();iter!= freeBlobList.end(); iter++) {
			if((*iter)->getDistance(tblb->getX(),tblb->getY())<(*closestBlob)->getDistance(tblb->getX(),tblb->getY())) closestBlob = iter;
		}
		
		TuioBlob *freeBlob = (*closestBlob);
		blobID = (*closestBlob)->getBlobID();
		freeBlobList.erase(closestBlob);
		delete freeBlob;
	} else maxBlobID = blobID;
	
	tblb->setSessionID(sessionID++);
	tblb->setBlobID(blobID);
	
	blobList.push_back(tblb);
	updateBlob = true;
	
	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->addTuioBlob(tblb);
	
	if (verbose) 
		std::cout << "add blb " << tblb->getBlobID() << " (" <<  tblb->getSessionID() << ") " << tblb->getX() << " " << tblb->getY()  << " " << tblb->getAngle() << " " << tblb->getWidth()  << " " << tblb->getHeight() << " " << tblb->getArea() << std::endl;
}

void TuioManager::updateTuioBlob(TuioBlob *tblb,float x, float y, float a, float w, float h, float f) {
	if (tblb==NULL) return;
	if (tblb->getTuioTime()==currentFrameTime) return;
	tblb->update(currentFrameTime,x,y,a,w,h,f);
	updateBlob = true;
	
	if (tblb->isMoving()) {	
		for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
			(*listener)->updateTuioBlob(tblb);
		
		if (verbose)	 	
			std::cout << "set blb " << tblb->getBlobID() << " (" <<  tblb->getSessionID() << ") " << tblb->getX() << " " << tblb->getY()  << " " << tblb->getAngle() << " " << tblb->getWidth()  << " " << tblb->getHeight() << " " << tblb->getArea()
			<< " " << tblb->getXSpeed() << " " << tblb->getYSpeed()  << " " << tblb->getRotationSpeed() << " " << tblb->getMotionAccel()<< " " << tblb->getRotationAccel() << " " << std::endl;
	}
}

void TuioManager::updateExternalTuioBlob(TuioBlob *tblb) {
	if (tblb==NULL) return;
	updateBlob = true;
	
	if (tblb->isMoving()) {	
		for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
			(*listener)->updateTuioBlob(tblb);
		
		if (verbose)		
			std::cout << "set blb " << tblb->getBlobID() << " (" <<  tblb->getSessionID() << ") " << tblb->getX() << " " << tblb->getY() << " " << tblb->getAngle() << " " << tblb->getWidth()  << " " << tblb->getHeight() << " " << tblb->getArea()
			<< " " << tblb->getXSpeed() << " " << tblb->getYSpeed() << " " << tblb->getRotationSpeed() << " " << tblb->getMotionAccel()<< " " << tblb->getRotationAccel() << " " << std::endl;
	}
}

void TuioManager::removeTuioBlob(TuioBlob *tblb) {
	if (tblb==NULL) return;
	
	blobList.remove(tblb);
	tblb->remove(currentFrameTime);
	updateBlob = true;

	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->removeTuioBlob(tblb);
	
	if (verbose)
		std::cout << "del blb " << tblb->getBlobID() << " (" <<  tblb->getSessionID() << ")" << std::endl;
	
	if (tblb->getBlobID()==maxBlobID) {
		maxBlobID = -1;
		delete tblb;
		
		if (blobList.size()>0) {
			std::list<TuioBlob*>::iterator clist;
			for (clist=blobList.begin(); clist != blobList.end(); clist++) {
				int blobID = (*clist)->getBlobID();
				if (blobID>maxBlobID) maxBlobID=blobID;
			}
			
			freeBlobBuffer.clear();
			for (std::list<TuioBlob*>::iterator flist=freeBlobList.begin(); flist != freeBlobList.end(); flist++) {
				TuioBlob *freeBlob = (*flist);
				if (freeBlob->getBlobID()>maxBlobID) delete freeBlob;
				else freeBlobBuffer.push_back(freeBlob);
			}
			
			freeBlobList = freeBlobBuffer;
			
		} else {
			for (std::list<TuioBlob*>::iterator flist=freeBlobList.begin(); flist != freeBlobList.end(); flist++) {
				TuioBlob *freeBlob = (*flist);
				delete freeBlob;
			}
			freeBlobList.clear();
		}
	} else if (tblb->getBlobID()<maxBlobID) {
		freeBlobList.push_back(tblb);	
	}
	
}

void TuioManager::removeExternalTuioBlob(TuioBlob *tblb) {
	if (tblb==NULL) return;
	blobList.remove(tblb);
	updateBlob = true;
	
	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->removeTuioBlob(tblb);
	
	if (verbose)
		std::cout << "del blb " << tblb->getBlobID() << " (" <<  tblb->getSessionID() << ")" << std::endl;
}

long TuioManager::getSessionID() {
	sessionID++;
	return sessionID;
}

long TuioManager::getFrameID() {
	return currentFrame;
}

TuioTime TuioManager::getFrameTime() {
	return currentFrameTime;
}

void TuioManager::initFrame(TuioTime ttime) {
	currentFrameTime = TuioTime(ttime);
	currentFrame++;
}

void TuioManager::commitFrame() {
	for (std::list<TuioListener*>::iterator listener=listenerList.begin(); listener != listenerList.end(); listener++)
		(*listener)->refresh(currentFrameTime);
}

TuioObject* TuioManager::getClosestTuioObject(float xp, float yp) {
	
	TuioObject *closestObject = NULL;
	float closestDistance = 1.0f;
	
	for (std::list<TuioObject*>::iterator iter=objectList.begin(); iter != objectList.end(); iter++) {
		float distance = (*iter)->getDistance(xp,yp);
		if(distance<closestDistance) {
			closestObject = (*iter);
			closestDistance = distance;
		}
	}
	
	return closestObject;
}

TuioCursor* TuioManager::getClosestTuioCursor(float xp, float yp) {

	TuioCursor *closestCursor = NULL;
	float closestDistance = 1.0f;

	for (std::list<TuioCursor*>::iterator iter=cursorList.begin(); iter != cursorList.end(); iter++) {
		float distance = (*iter)->getDistance(xp,yp);
		if(distance<closestDistance) {
			closestCursor = (*iter);
			closestDistance = distance;
		}
	}
	
	return closestCursor;
}

TuioBlob* TuioManager::getClosestTuioBlob(float xp, float yp) {
	
	TuioBlob *closestBlob = NULL;
	float closestDistance = 1.0f;
	
	for (std::list<TuioBlob*>::iterator iter=blobList.begin(); iter != blobList.end(); iter++) {
		float distance = (*iter)->getDistance(xp,yp);
		if(distance<closestDistance) {
			closestBlob = (*iter);
			closestDistance = distance;
		}
	}
	
	return closestBlob;
}

std::list<TuioObject*> TuioManager::getUntouchedObjects() {
	
	std::list<TuioObject*> untouched;
	for (std::list<TuioObject*>::iterator tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++) {
		TuioObject *tobj = (*tuioObject);
		if (tobj->getTuioTime()!=currentFrameTime) untouched.push_back(tobj);
	}	
	return untouched;
}

void TuioManager::stopUntouchedMovingObjects() {
	
	std::list<TuioObject*> untouched;
	for (std::list<TuioObject*>::iterator tuioObject = objectList.begin(); tuioObject!=objectList.end(); tuioObject++) {
		
		TuioObject *tobj = (*tuioObject);
		if ((tobj->getTuioTime()!=currentFrameTime) && (tobj->isMoving())) {
			tobj->stop(currentFrameTime);
			updateObject = true;
			if (verbose)		
				std::cout << "set obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() 
				<< " " << tobj->getXSpeed() << " " << tobj->getYSpeed() << " " << tobj->getRotationSpeed() << " " << tobj->getMotionAccel() << " " << tobj->getRotationAccel() << std::endl;
		}
	}
}

void TuioManager::removeUntouchedStoppedObjects() {
	
	std::list<TuioObject*>::iterator tuioObject = objectList.begin();
	while (tuioObject!=objectList.end()) {
		TuioObject *tobj = (*tuioObject);
		if ((tobj->getTuioTime()!=currentFrameTime) && (!tobj->isMoving())) {
			removeTuioObject(tobj);
			tuioObject = objectList.begin();
		} else tuioObject++;
	}
}

void TuioManager::resetTuioObjects() {
	
	std::list<TuioObject*>::iterator tuioObject = objectList.begin();
	while (tuioObject!=objectList.end()) {
		removeTuioObject((*tuioObject));
		tuioObject = objectList.begin();
	}
}

std::list<TuioCursor*> TuioManager::getUntouchedCursors() {
	
	std::list<TuioCursor*> untouched;
	for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++) {
		TuioCursor *tcur = (*tuioCursor);
		if (tcur->getTuioTime()!=currentFrameTime) untouched.push_back(tcur);
	}	
	return untouched;
}

void TuioManager::stopUntouchedMovingCursors() {
	
	std::list<TuioCursor*> untouched;
	for (std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin(); tuioCursor!=cursorList.end(); tuioCursor++) {
		TuioCursor *tcur = (*tuioCursor);
		if ((tcur->getTuioTime()!=currentFrameTime) && (tcur->isMoving())) {
			tcur->stop(currentFrameTime);
			updateCursor = true;
			if (verbose) 	
				std::cout << "set cur " << tcur->getCursorID() << " (" <<  tcur->getSessionID() << ") " << tcur->getX() << " " << tcur->getY() 
				<< " " << tcur->getXSpeed() << " " << tcur->getYSpeed()<< " " << tcur->getMotionAccel() << " " << std::endl;							
		}
	}	
}

void TuioManager::removeUntouchedStoppedCursors() {
	
	if (cursorList.size()==0) return;
	std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin();
	while (tuioCursor!=cursorList.end()) {
		TuioCursor *tcur = (*tuioCursor);
		if ((tcur->getTuioTime()!=currentFrameTime) && (!tcur->isMoving())) {
			removeTuioCursor(tcur);
			tuioCursor = cursorList.begin();
		} else tuioCursor++;
	}	
}

void TuioManager::resetTuioCursors() {
	
	std::list<TuioCursor*>::iterator tuioCursor = cursorList.begin();
	while (tuioCursor!=cursorList.end()) {
		removeTuioCursor((*tuioCursor));
		tuioCursor = cursorList.begin();
	}
}

std::list<TuioBlob*> TuioManager::getUntouchedBlobs() {
	
	std::list<TuioBlob*> untouched;
	for (std::list<TuioBlob*>::iterator tuioBlob = blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++) {
		TuioBlob *tblb = (*tuioBlob);
		if (tblb->getTuioTime()!=currentFrameTime) untouched.push_back(tblb);
	}	
	return untouched;
}

void TuioManager::stopUntouchedMovingBlobs() {
	
	std::list<TuioBlob*> untouched;
	for (std::list<TuioBlob*>::iterator tuioBlob = blobList.begin(); tuioBlob!=blobList.end(); tuioBlob++) {
		TuioBlob *tblb = (*tuioBlob);
		if ((tblb->getTuioTime()!=currentFrameTime) && (tblb->isMoving())) {
			tblb->stop(currentFrameTime);
			updateBlob = true;
			if (verbose) 	
				std::cout << "set blb " << tblb->getBlobID() << " (" <<  tblb->getSessionID() << ") " << tblb->getX() << " " << tblb->getY()  << " " << tblb->getAngle() << " " << tblb->getWidth()  << " " << tblb->getHeight() << " " << tblb->getArea()
				<< " " << tblb->getXSpeed() << " " << tblb->getYSpeed()  << " " << tblb->getRotationSpeed() << " " << tblb->getMotionAccel()<< " " << tblb->getRotationAccel() << " " << std::endl;
		}
	}	
}

void TuioManager::removeUntouchedStoppedBlobs() {
	
	std::list<TuioBlob*>::iterator tuioBlob = blobList.begin();
	while (tuioBlob!=blobList.end()) {
		TuioBlob *tblb = (*tuioBlob);
		if ((tblb->getTuioTime()!=currentFrameTime) && (!tblb->isMoving())) {
			removeTuioBlob(tblb);
			tuioBlob = blobList.begin();
		} else tuioBlob++;
	}	
}

void TuioManager::resetTuioBlobs() {
	
	std::list<TuioBlob*>::iterator tuioBlob = blobList.begin();
	while (tuioBlob!=blobList.end()) {
		removeTuioBlob((*tuioBlob));
		tuioBlob = blobList.begin();
	}
}
