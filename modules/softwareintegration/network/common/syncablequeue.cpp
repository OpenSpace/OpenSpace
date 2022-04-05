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

#include <modules/softwareintegration/network/common/syncablequeue.h>

#include <ghoul/logging/logmanager.h>

// TODO: REMOVE
#include <openspace/util/syncable.h>
#include <openspace/util/concurrentqueue.h>
#include <openspace/util/syncbuffer.h>

 

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegration_SyncableQueue";
} // namespace

namespace openspace {

// template <typename T>
// SyncableQueue::SyncableQueue<T>() {
//     _messages.clear();
// };

// void SyncableQueue::preSync(bool isMaster) {
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

template <typename T>
void SyncableQueue<T>::encode(SyncBuffer* syncBuffer) {
    //size_t nMessages = _messages.size();
    //syncBuffer->encode(nMessages);
    //for (auto m : _messages) {
    //    syncBuffer->encode(m);
    //}
    //// _messages.clear();
}

template <typename T>
void SyncableQueue<T>::decode(SyncBuffer* syncBuffer) {
    //std::lock_guard guard(_clientMessageMutex);
    //size_t nMessages;
    //syncBuffer->decode(nMessages);

    //for (size_t i = 0; i < nMessages; ++i) {
    //    T message;
    //    syncBuffer->decode(message);
    //    _messages.push(std::move(message));
    //}
}

template <typename T>
void SyncableQueue<T>::push(T &&item) {
    _messages.push(item);
}

template <typename T>
void SyncableQueue<T>::push(const T& item) {
    _messages.push(item);
}

template <typename T>
T SyncableQueue<T>::pop() {
    return _messages.pop();
}

template <typename T>
void SyncableQueue<T>::pop(T& item) {
    _messages.pop(item);
}

template <typename T>
size_t SyncableQueue<T>::size() const {
    return _messages.size();
}

template <typename T>
bool SyncableQueue<T>::empty() const {
    return _messages.empty();
}

// template <typename T>
// void SyncableQueue<T>::postSync(bool isMaster) {
//     if (isMaster) {
//         while (!_messages.empty()) {
//             T message = _messages.front();
//             _messages.pop();
            
//             std::string msg = { std::begin(_message.data().content), std::end(_message.data().content) };
//             LWARNING(fmt::format("Message (MASTER) with {}", msg));

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
//         }
//     }
//     else {
//         std::lock_guard<std::mutex> guard(_clientMessageMutex);
//         while (!_messages.empty()) {
//             T message = _messages.front();
//             _messages.pop();

//             std::string msg = { std::begin(_message.data().content), std::end(_message.data().content) };
//             LWARNING(fmt::format("Message (CLIENT) with {}", msg));
//     //         try {
//     //             runScript(_slaveScriptQueue.front());
//     //             _slaveScriptQueue.pop();
//     //         }
//     //         catch (const ghoul::RuntimeError& e) {
//     //             LERRORC(e.component, e.message);
//     //         }
//         }
//     }
// }


    // size_t SoftwareIntegrationNodeHandler::nConnections() const {
    //     return _nConnections;
    // }

} // namespace openspace
