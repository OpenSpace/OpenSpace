/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/touch/include/tuioear.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <ghoul/logging/logmanager.h>

using namespace TUIO;

void TuioEar::addTuioObject(TuioObject*) { }

void TuioEar::updateTuioObject(TuioObject*) { }

void TuioEar::removeTuioObject(TuioObject*) { }

void TuioEar::addTuioCursor(TuioCursor* tcur) {
    _mx.lock();
    _tap = false;
    // find same id in _list if it exists in _removeList (new input with same ID as a
    // previously stored)
    long i = tcur->getSessionID();
    std::vector<long>::iterator foundID = std::find_if(
        _removeList.begin(),
        _removeList.end(),
        [&i](long id) { return id == i; });

    // if found, remove id from _removeList and update, otherwise add new id to list
    if (foundID != _removeList.end()) {
        std::find_if(
            _list.begin(),
            _list.end(),
            [&i](const TuioCursor& cursor) {
                return cursor.getSessionID() == i;
            }
        )->update(tcur);
        _removeList.erase(foundID);
    }
    else {
        _list.emplace_back(*tcur);
    }
    _mx.unlock();
}

void TuioEar::updateTuioCursor(TuioCursor* tcur) {
    _mx.lock();
    _tap = false;
    long i = tcur->getSessionID();
    std::find_if(
        _list.begin(),
        _list.end(),
        [&i](const TuioCursor& cursor) {
            return cursor.getSessionID() == i;
        }
    )->update(tcur);
    _mx.unlock();
}

// save id to be removed and remove it in clearInput
void TuioEar::removeTuioCursor(TuioCursor* tcur) {
    _mx.lock();
    _removeList.push_back(tcur->getSessionID());

    // Check if the cursor ID could be considered a tap
    glm::dvec2 currPos = glm::dvec2(tcur->getX(), tcur->getY());
    double dist = 0;
    for (const TuioPoint& p : tcur->getPath()) {
        dist += glm::length(glm::dvec2(p.getX(), p.getY()) - currPos);
    }
    dist /= tcur->getPath().size();

    double heldTime =
        tcur->getPath().back().getTuioTime().getTotalMilliseconds() -
        tcur->getPath().front().getTuioTime().getTotalMilliseconds();

    if (heldTime < 180 && dist < 0.0004 && _list.size() == 1 && _removeList.size() == 1) {
        _tapCo = TuioCursor(*tcur);
        _tap = true;
    }
    _mx.unlock();
}

void TuioEar::addTuioBlob(TuioBlob*) { }

void TuioEar::updateTuioBlob(TuioBlob*) { }

void TuioEar::removeTuioBlob(TuioBlob*) { }

void TuioEar::refresh(TuioTime) { } // about every 15ms

std::vector<TuioCursor> TuioEar::getInput() {
    return _list;
}

bool TuioEar::tap() {
    if (_tap) {
        _tap = false;
        return !_tap;
    }
    else {
        return _tap;
    }
}

TuioCursor TuioEar::getTap() {
    std::lock_guard<std::mutex> lock(_mx);
    return _tapCo;
}

// Removes all cursor ID from list that exists in _removeList
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
                    [&cursor](long id) {
                        return cursor.getSessionID() == id;
                    }
                ) != _removeList.end();
            }
        ),
        _list.end()
    );
    _removeList.clear();
    _mx.unlock();
}

// Standard UDP IP connection to port 3333
TuioEar::TuioEar() {
    _oscReceiver = new UdpReceiver(3333);
    _tuioClient = new TuioClient(_oscReceiver);
    _tuioClient->addTuioListener(this);
    _tuioClient->connect();
}
