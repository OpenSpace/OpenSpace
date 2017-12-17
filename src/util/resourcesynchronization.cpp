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

#include <openspace/util/resourcesynchronization.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <modules/sync/syncmodule.h>

#include <openspace/util/factorymanager.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/logging/logmanager.h>

#include <memory>

namespace {
    constexpr char* KeyType = "Type";
    constexpr char* _loggerCat = "ResourceSynchronization";
}

namespace openspace {

documentation::Documentation ResourceSynchronization::Documentation() {
    using namespace openspace::documentation;

    return {
        "ResourceSynchronization",
        "resourceSynchronization",
        {
            {
                KeyType,
                new StringAnnotationVerifier(
                    "A valid ResourceSyncrhonization created by a factory"
                ),
                Optional::No,
                "This key specifies the type of ResourceSyncrhonization that gets created. "
                "It has to be one of the valid ResourceSyncrhonizations that are available "
                "for creation (see the FactoryDocumentation for a list of possible "
                "ResourceSyncrhonizations), which depends on the configration of the "
                " application"
            }
        }
    };
}

ResourceSynchronization::ResourceSynchronization() {}

ResourceSynchronization::~ResourceSynchronization() {}

std::unique_ptr<ResourceSynchronization> ResourceSynchronization::createFromDictionary(
    const ghoul::Dictionary & dictionary)
{
    documentation::testSpecificationAndThrow(
        Documentation(), dictionary, "ResourceSynchronization");

    std::string synchronizationType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<ResourceSynchronization>();
    ghoul_assert(factory, "ResourceSynchronization factory did not exist");
    std::unique_ptr<ResourceSynchronization> result =
        factory->create(synchronizationType, dictionary);

    if (result == nullptr) {
        LERROR("Failed to create a ResourceSynchronization object of type '" <<
               synchronizationType << "'");
        return nullptr;
    }

    return result;
}

void ResourceSynchronization::wait() {
}

ResourceSynchronization::State ResourceSynchronization::state() {
    return _state;
}

bool ResourceSynchronization::isResolved() {
    return _state == State::Resolved;
}

bool ResourceSynchronization::isRejected() {
    return _state == State::Rejected;
}

bool ResourceSynchronization::isSyncing() {
    return _state == State::Syncing;
}

ResourceSynchronization::CallbackHandle
    ResourceSynchronization::addStateChangeCallback(StateChangeCallback cb)
{
    std::lock_guard<std::mutex> guard(_callbackMutex);
    CallbackHandle callbackId = _nextCallbackId++;
    _stateChangeCallbacks[callbackId] = cb;
    return callbackId;
}

void ResourceSynchronization::removeStateChangeCallback(CallbackHandle id) {
    std::lock_guard<std::mutex> guard(_callbackMutex);
    _stateChangeCallbacks.erase(id);
}

void ResourceSynchronization::resolve() {
    setState(State::Resolved);
}

void ResourceSynchronization::reject() {
    setState(State::Rejected);
}

void ResourceSynchronization::reset() {
    setState(State::Unsynced);
}

void ResourceSynchronization::begin() {
    setState(State::Syncing);
}

void ResourceSynchronization::setState(State state) {
    _state = state;
    _callbackMutex.lock();
    std::vector<StateChangeCallback> callbacks;
    callbacks.reserve(_stateChangeCallbacks.size());
    for (const auto& it : _stateChangeCallbacks) {
        callbacks.push_back(it.second);
    }
    _callbackMutex.unlock();
    for (auto& cb : callbacks) {
        cb(state);
    }
}

float ResourceSynchronization::progress() {
    if (!nTotalBytesIsKnown()) {
        return 0.f;
    }
    if (nTotalBytes() == 0) {
        return 1.f;
    }
    return static_cast<float>(nSynchronizedBytes()) / static_cast<float>(nTotalBytes());
}

}
