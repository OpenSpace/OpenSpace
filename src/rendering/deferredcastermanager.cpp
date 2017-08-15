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

#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/deferredcasterlistener.h>
#include <algorithm>
#include <string>

namespace {
    const char* _loggerCat = "DeferredcasterManager";
}

namespace openspace {

DeferredcasterManager::DeferredcasterManager() = default;

DeferredcasterManager::~DeferredcasterManager() = default;

void DeferredcasterManager::attachDeferredcaster(Deferredcaster& deferredcaster) {
    if (!isAttached(deferredcaster)) {
        _deferredcasters.push_back(&deferredcaster);
    }
    for (auto &listener : _listeners) {
        listener->deferredcastersChanged(deferredcaster, ghoul::Boolean::Yes);
    }
}

void DeferredcasterManager::detachDeferredcaster(Deferredcaster& deferredcaster) {
    auto it = std::find(_deferredcasters.begin(), _deferredcasters.end(), &deferredcaster);
    if (it != _deferredcasters.end()) {
        _deferredcasters.erase(it);
        for (auto &listener : _listeners) {
            listener->deferredcastersChanged(deferredcaster, ghoul::Boolean::No);
        }
    }
}

bool DeferredcasterManager::isAttached(Deferredcaster& deferredcaster) {
    auto it = std::find(_deferredcasters.begin(), _deferredcasters.end(), &deferredcaster);
    return it != _deferredcasters.end();
}

void DeferredcasterManager::addListener(DeferredcasterListener& listener) {
    auto it = std::find(_listeners.begin(), _listeners.end(), &listener);
    if (it == _listeners.end()) {
        _listeners.push_back(&listener);
    }
}

void DeferredcasterManager::removeListener(DeferredcasterListener& listener) {
    auto it = std::find(_listeners.begin(), _listeners.end(), &listener);
    if (it != _listeners.end()) {
        _listeners.erase(it);
    }
}

const std::vector<Deferredcaster*>& DeferredcasterManager::deferredcasters() {
    return _deferredcasters;
}

}
