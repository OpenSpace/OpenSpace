/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/touch/include/TuioEar.h>
#include <vector>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <ghoul/logging/logmanager.h>
#include <boost/thread/mutex.hpp>

namespace {
	const std::string _loggerCat = "TuioEar";
}

void TuioEar::tuioAdd(TuioObject *tobj) {
	if (tobj->containsNewTuioPointer()) {
		boost::lock_guard<boost::mutex> lock(_mx);
		_list.push_back(tobj);

		LINFO("add ptr " << tobj->getTuioPointer()->getSessionID() << ", size: " << _list.size() << "\n");
	}
	if (tobj->containsNewTuioToken()) LINFO("add tok " << tobj->getTuioToken()->getSessionID() << "\n");
	if(tobj->containsNewTuioBounds()) LINFO("add bnd " << tobj->getTuioBounds()->getSessionID() << "\n");
	if(tobj->containsNewTuioSymbol()) LINFO("add sym " << tobj->getTuioSymbol()->getSessionID() << "\n");
	//std::cout << "add obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << "/"<<  tobj->getTuioSourceID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() << std::endl;
}

void TuioEar::tuioUpdate(TuioObject *tobj) {
	if (tobj->containsTuioPointer()) {
		boost::lock_guard<boost::mutex> lock(_mx);
		_list.push_back(tobj);

		LINFO("set ptr " << tobj->getTuioPointer()->getSessionID() << ", size: " << _list.size() << "\n");
	}
	if (tobj->containsTuioToken()) LINFO("set tok " << tobj->getTuioToken()->getSessionID() << "\n");
	if (tobj->containsTuioBounds()) LINFO("set bnd " << tobj->getTuioBounds()->getSessionID() << "\n");
	if (tobj->containsTuioSymbol()) LINFO("set sym " << tobj->getTuioSymbol()->getSessionID() << "\n");
	//std::cout << "set obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << "/"<<  tobj->getTuioSourceID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() << " " << tobj->getMotionSpeed() << " " << tobj->getRotationSpeed() << " " << tobj->getMotionAccel() << " " << tobj->getRotationAccel() << std::endl;	
}

void TuioEar::tuioRemove(TuioObject *tobj) {
	if (tobj->containsTuioPointer()) {
		LINFO("del ptr " << tobj->getTuioPointer()->getSessionID() << ", size: " << _list.size() << "\n");
	} 
	if (tobj->containsTuioToken()) LINFO("del tok " << tobj->getTuioToken()->getSessionID() << "\n");
	if (tobj->containsTuioBounds()) LINFO("del bnd " << tobj->getTuioBounds()->getSessionID() << "\n");
	if (tobj->containsTuioSymbol()) LINFO("del sym " << tobj->getTuioSymbol()->getSessionID() << "\n");
	//std::cout << "del obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << "/"<<  tobj->getTuioSourceID() << ")" << std::endl;
}

void TuioEar::tuioRefresh(TuioTime frameTime) {
	LINFO("refresh " << frameTime.getFrameID() << " "<< frameTime.getTotalMilliseconds() << "\n");
}

std::vector<TuioObject*> TuioEar::getInput() {
	boost::lock_guard<boost::mutex> lock(_mx);
	return _list;
}

void TuioEar::clearInput() {
	boost::lock_guard<boost::mutex> lock(_mx);
	_list.clear();
}

TuioEar::TuioEar() {
	_oscReceiver = new UdpReceiver(3333);
	//oscReceiver = new TcpReceiver("127.0.0.1",3333);
	_tuioClient = new TuioClient(_oscReceiver);
	_tuioClient->addTuioListener(this);
	_tuioClient->connect();
}
