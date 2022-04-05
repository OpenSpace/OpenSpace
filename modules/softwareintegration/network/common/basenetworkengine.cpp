/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/softwareintegration/network/common/basenetworkengine.h>

#include <openspace/engine/globals.h>
// #include <openspace/engine/globalscallbacks.h>
// #include <openspace/navigation/navigationhandler.h>
// #include <openspace/scene/scene.h>
// #include <openspace/scripting/scriptengine.h>
#include <openspace/engine/syncengine.h>
// #include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
//REMOVE
// #include <openspace/util/timemanager.h>

// #include <openspace/engine/openspaceengine.h>

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegration_BaseNetworkEngine";
} // namespace

namespace openspace {

BaseNetworkEngine::~BaseNetworkEngine() {}

void BaseNetworkEngine::start() {
    global::syncEngine->addSyncables(getSyncables());
    _eventLoopThread = std::thread([this]() { eventLoop(); });
}

void BaseNetworkEngine::stop() {
    global::syncEngine->removeSyncables(getSyncables());
    _shouldStop = true;

    if (_eventLoopThread.joinable()) {
		_eventLoopThread.join();
	}
}

std::vector<Syncable*> BaseNetworkEngine::getSyncables() {
    return { &_incomingMessages };
}


// void BaseNetworkEngine::preSync(bool isMaster) {
//     // ZoneScoped

//     if (!isMaster) {
//         return;
//     }

//     std::lock_guard guard(_clientMessageMutex);
//     while (!_incomingMessages.empty()) {
//         PeerMessage item = std::move(_incomingMessages.front());
//         _incomingMessages.pop();

//         _messagesToSync.push_back(item);
//         // const bool remoteScripting = item.remoteScripting;

//         // Not really a received script but the master also needs to run the script...
//         // _masterScriptQueue.push(item);

//         // if (global::parallelPeer->isHost() && remoteScripting) {
//         //     global::parallelPeer->sendScript(item.script);
//         // }
//         // if (global::sessionRecording->isRecording()) {
//         //     global::sessionRecording->saveScriptKeyframeToTimeline(item.script);
//         // }
//     }
// }

// void BaseNetworkEngine::encode(SyncBuffer* syncBuffer) {
    // ZoneScoped

    // size_t nMessages = _incomingMessages.size();
    // syncBuffer->encode(nScripts);
    // for (const std::string& s : _scriptsToSync) {
    //     syncBuffer->encode(s);
    // }
    // _scriptsToSync.clear();
// }

// void BaseNetworkEngine::decode(SyncBuffer* syncBuffer) {
    // ZoneScoped

    // std::lock_guard guard(_slaveScriptsMutex);
    // size_t nScripts;
    // syncBuffer->decode(nScripts);

    // for (size_t i = 0; i < nScripts; ++i) {
    //     std::string script;
    //     syncBuffer->decode(script);
    //     _slaveScriptQueue.push(std::move(script));
    // }
// }

// void BaseNetworkEngine::postSync(bool isMaster) {
//     // ZoneScoped

//     // if (isMaster) {
//     //     while (!_masterScriptQueue.empty()) {
//     //         std::string script = std::move(_masterScriptQueue.front().script);
//     //         ScriptCallback callback = std::move(_masterScriptQueue.front().callback);
//     //         _masterScriptQueue.pop();
//     //         try {
//     //             runScript(script, callback);
//     //         }
//     //         catch (const ghoul::RuntimeError& e) {
//     //             LERRORC(e.component, e.message);
//     //             continue;
//     //         }
//     //     }
//     // }
//     // else {
//     //     std::lock_guard guard(_slaveScriptsMutex);
//     //     while (!_slaveScriptQueue.empty()) {
//     //         try {
//     //             runScript(_slaveScriptQueue.front());
//     //             _slaveScriptQueue.pop();
//     //         }
//     //         catch (const ghoul::RuntimeError& e) {
//     //             LERRORC(e.component, e.message);
//     //         }
//     //     }
//     // }
// }


// size_t SoftwareIntegrationNodeHandler::nConnections() const {
//     return _nConnections;
// }

} // namespace openspace
