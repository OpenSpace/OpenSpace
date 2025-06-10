/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <ghoul/logging/logmanager.h>
#include <mutex>

using namespace TUIO;

namespace {
    openspace::TouchInput touchInput(TuioCursor* tcur) {
        return openspace::TouchInput(
            static_cast<size_t>(tcur->getTuioSourceID()),
            static_cast<size_t>(tcur->getCursorID()),
            tcur->getX(),
            tcur->getY(),
            static_cast<double>(tcur->getTuioTime().getTotalMilliseconds()) / 1000.0
        );
    }
}

namespace openspace {

void TuioEar::addTuioObject(TuioObject*) {}

void TuioEar::updateTuioObject(TuioObject*) {}

void TuioEar::removeTuioObject(TuioObject*) {}

void TuioEar::addTuioCursor(TuioCursor* tcur) {
    std::lock_guard lock(_mx);
    _inputList.push_back(touchInput(tcur));
}

void TuioEar::updateTuioCursor(TuioCursor* tcur) {
    std::lock_guard lock(_mx);
    _inputList.push_back(touchInput(tcur));
}

// save id to be removed and remove it in clearInput
void TuioEar::removeTuioCursor(TuioCursor* tcur) {
    std::lock_guard lock(_mx);
    _removalList.push_back(touchInput(tcur));
}

void TuioEar::addTuioBlob(TuioBlob*) {}

void TuioEar::updateTuioBlob(TuioBlob*) {}

void TuioEar::removeTuioBlob(TuioBlob*) {}

void TuioEar::refresh(TuioTime) {} // about every 15ms

std::vector<TouchInput> TuioEar::takeInputs() {
    std::vector<TouchInput> outputList;
    {
        std::lock_guard lock(_mx);
        outputList.swap(_inputList);
    }
    return outputList;
}

std::vector<TouchInput> TuioEar::takeRemovals() {
    std::vector<TouchInput> outputList;
    {
        std::lock_guard lock(_mx);
        outputList.swap(_removalList);
    }
    return outputList;
}

// Standard UDP IP connection to port 3333
TuioEar::TuioEar()
    : _tuioClient(3333)
{
    _tuioClient.addTuioListener(this);
    _tuioClient.connect();
}

TuioEar::~TuioEar() {
    _tuioClient.disconnect();
}

} // namespace openspace
