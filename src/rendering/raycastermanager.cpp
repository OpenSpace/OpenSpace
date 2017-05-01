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

#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/raycasterlistener.h>
#include <algorithm>
#include <string>

namespace {
    const std::string _loggerCat = "RaycasterManager";
}

namespace openspace {

RaycasterManager::RaycasterManager() {}

RaycasterManager::~RaycasterManager() {}

void RaycasterManager::attachRaycaster(VolumeRaycaster& raycaster) {
    if (!isAttached(raycaster)) {
        _raycasters.push_back(&raycaster);
    }
    for (auto &listener : _listeners) {
        listener->raycastersChanged(raycaster, true);
    }
}

bool RaycasterManager::isAttached(VolumeRaycaster& raycaster) {
    auto it = std::find(_raycasters.begin(), _raycasters.end(), &raycaster);
    return it != _raycasters.end();
}


void RaycasterManager::detachRaycaster(VolumeRaycaster& raycaster) {
    auto it = std::find(_raycasters.begin(), _raycasters.end(), &raycaster);
    if (it != _raycasters.end()) {
        _raycasters.erase(it);
        for (auto &listener : _listeners) {
            listener->raycastersChanged(raycaster, false);
        }
    }
}


void RaycasterManager::addListener(RaycasterListener& listener) {
    auto it = std::find(_listeners.begin(), _listeners.end(), &listener);
    if (it == _listeners.end()) {
        _listeners.push_back(&listener);
    }
}

void RaycasterManager::removeListener(RaycasterListener& listener) {
    auto it = std::find(_listeners.begin(), _listeners.end(), &listener);
    if (it != _listeners.end()) {
        _listeners.erase(it);
    }
}

const std::vector<VolumeRaycaster*>& RaycasterManager::raycasters() {
    return _raycasters;
}

}
