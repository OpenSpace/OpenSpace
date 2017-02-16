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
#include <algorithm>    // std::find


#include <ghoul/logging/logmanager.h>

namespace {
	const std::string _loggerCat = "TuioEar";
}

void TuioEar::tuioAdd(TuioObject *tobj) {
	
	if (tobj->containsNewTuioToken()) LINFO("add tok " << tobj->getTuioToken()->getSessionID() << "\n");
	if (tobj->containsNewTuioPointer()) {
		list.push_back(tobj->getTuioPointer());
		LINFO("add ptr " << tobj->getTuioPointer()->getSessionID() << ", Fingers: " << list.size() << "\n");

	}
	if(tobj->containsNewTuioBounds()) LINFO("add bnd " << tobj->getTuioBounds()->getSessionID() << "\n");
	if(tobj->containsNewTuioSymbol()) LINFO("add sym " << tobj->getTuioSymbol()->getSessionID() << "\n");
	//std::cout << "add obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << "/"<<  tobj->getTuioSourceID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() << std::endl;
	//std::cout << std::flush;
}

void TuioEar::tuioUpdate(TuioObject *tobj) {
	if (tobj->containsTuioToken()) LINFO("set tok " << tobj->getTuioToken()->getSessionID() << "\n");
	if (tobj->containsTuioPointer()) LINFO("set ptr " << tobj->getTuioPointer()->getSessionID() << "\n");
	if (tobj->containsTuioBounds()) LINFO("set bnd " << tobj->getTuioBounds()->getSessionID() << "\n");
	if (tobj->containsTuioSymbol()) LINFO("set sym " << tobj->getTuioSymbol()->getSessionID() << "\n");
	//std::cout << "set obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << "/"<<  tobj->getTuioSourceID() << ") "<< tobj->getX() << " " << tobj->getY() << " " << tobj->getAngle() << " " << tobj->getMotionSpeed() << " " << tobj->getRotationSpeed() << " " << tobj->getMotionAccel() << " " << tobj->getRotationAccel() << std::endl;	
	//std::cout << std::flush;
}

void TuioEar::tuioRemove(TuioObject *tobj) {
	if (tobj->containsTuioToken()) LINFO("del tok " << tobj->getTuioToken()->getSessionID() << "\n");

	if (tobj->containsTuioPointer()) {
		it = list.begin();
		for (; it != list.end(); ++it) {
			if (it->getSessionID() == tobj->getTuioPointer()->getSessionID()) {
				list.erase(it);
				LINFO("del ptr " << tobj->getTuioPointer()->getSessionID() << ", Fingers: " << list.size() << "\n");
				break;
			}
		}
	} 

	if (tobj->containsTuioBounds()) LINFO("del bnd " << tobj->getTuioBounds()->getSessionID() << "\n");
	if (tobj->containsTuioSymbol()) LINFO("del sym " << tobj->getTuioSymbol()->getSessionID() << "\n");
	//std::cout << "del obj " << tobj->getSymbolID() << " (" << tobj->getSessionID() << "/"<<  tobj->getTuioSourceID() << ")" << std::endl;
	//std::cout << std::flush;
}

void TuioEar::tuioRefresh(TuioTime frameTime) {
	LINFO("refresh " << frameTime.getFrameID() << " "<< frameTime.getTotalMilliseconds() << "\n");
}

TuioEar::TuioEar() {
	oscReceiver = new UdpReceiver(3333);
	//oscReceiver = new TcpReceiver("127.0.0.1",3333);
	tuioClient = new TuioClient(oscReceiver);
	tuioClient->addTuioListener(this);
	tuioClient->connect();
}
