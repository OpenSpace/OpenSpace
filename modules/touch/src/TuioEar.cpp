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

using namespace TUIO;

namespace {
	const std::string _loggerCat = "TuioEar";
}

void TuioEar::addTuioObject(TuioObject *tobj) { }

void TuioEar::updateTuioObject(TuioObject *tobj) { }

void TuioEar::removeTuioObject(TuioObject *tobj) { }

void TuioEar::addTuioCursor(TuioCursor *tcur) {
	_mx.lock();
	// find same id in _list if it exists in _removeList (new input with same ID as a previously stored)
	int i = tcur->getSessionID();
	std::vector<int>::iterator foundID = std::find_if(
		_removeList.begin(),
		_removeList.end(),
		[&i](int id) { return id == i; });

	// if found, remove id from _removeList and update, otherwise add new id to list
	if (foundID != _removeList.end()) {
		std::find_if(
			_list.begin(),
			_list.end(),
			[&i](const TuioCursor& cursor) {
			return cursor.getSessionID() == i;
		})->update(tcur);
		_removeList.erase(foundID);
	}
	else
		_list.push_back(TuioCursor(*tcur));

	_mx.unlock();
}

void TuioEar::updateTuioCursor(TuioCursor *tcur) {
	_mx.lock();
	_tap = false;
	int i = tcur->getSessionID();
	std::find_if(
		_list.begin(),
		_list.end(),
		[&i](const TuioCursor& cursor) {
			return cursor.getSessionID() == i;
	})->update(tcur);
	_mx.unlock();
}

// save id to be removed and remove it in clearInput
void TuioEar::removeTuioCursor(TuioCursor *tcur) {
	_mx.lock();
	if (tcur->getPath().size() < 3 && tcur->getMotionSpeed() < 0.05) {
		_tapCo = TuioCursor(*tcur);
		_tap = true;
	}
		
	_removeList.push_back(tcur->getSessionID());
	_mx.unlock();
}

void TuioEar::addTuioBlob(TuioBlob *tblb) { }

void TuioEar::updateTuioBlob(TuioBlob *tblb) { }

void TuioEar::removeTuioBlob(TuioBlob *tblb) { }

void TuioEar::refresh(TuioTime frameTime) { } // about every 15ms on TuioPad app

std::vector<TuioCursor> TuioEar::getInput() {
	std::lock_guard<std::mutex> lock(_mx);
	return _list;
}

bool TuioEar::tap() {
	std::lock_guard<std::mutex> lock(_mx);
	if (_tap) {
		_tap = false;
		return !_tap;
	}
	else
		return _tap;
}

TuioCursor TuioEar::getTap() {
	return _tapCo;
}

void TuioEar::clearInput() {
	_mx.lock();
	_list.erase(
		std::remove_if(
			_list.begin(),
			_list.end(),
			[this](const TuioCursor& cursor) {
		return std::find_if(
			_removeList.begin(),
			_removeList.end(),
			[&cursor](int id) {
			return cursor.getSessionID() == id;
		}
		) != _removeList.end();
	}),
	_list.end()
	);
	_removeList.clear();
	_mx.unlock();
}

TuioEar::TuioEar() {
	_oscReceiver = new UdpReceiver(3333);
	_tuioClient = new TuioClient(_oscReceiver);
	_tuioClient->addTuioListener(this);
	_tuioClient->connect();
}
