/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/engine/syncengine.h>
#include <openspace/util/syncdata.h>
#include <openspace/util/syncbuffer.h>
#include <ghoul/logging/logmanager.h>

#include <string>


namespace {
    const std::string _loggerCat = "SyncEngine";
}


namespace openspace {


    // should be called on sgct master
    void SyncEngine::encode(SyncBuffer* syncBuffer) {
        for (const auto& syncable : _syncables) {
            syncable->encode(syncBuffer);
        }
    }

    //should be called on sgct slaves
    void SyncEngine::decode(SyncBuffer* syncBuffer) {
        for (const auto& syncable : _syncables) {
            syncable->decode(syncBuffer);
        }
    }

    void SyncEngine::applySyncedUpdates() {
        for (const auto& syncable : _syncables) {
            syncable->applySyncedUpdate();
        }
    }

    void SyncEngine::addSyncable(Syncable*  syncable) {
        _syncables.push_back(syncable);
    }

    void SyncEngine::addSyncables(const std::vector<Syncable*>& syncables) {
        for (const auto& syncable : syncables) {
            addSyncable(syncable);
        }
    }

    void SyncEngine::removeSyncable(Syncable*  syncable) {
        _syncables.erase(std::remove(_syncables.begin(), _syncables.end(), syncable));
    }

}
