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

#include <modules/softwareintegration/network/common/syncablemessagequeue.h>
#include <openspace/util/syncbuffer.h>

#include <ghoul/logging/logmanager.h>


namespace {
	constexpr const char* _loggerCat = "SoftwareIntegration_SyncableMessageQueue";
} // namespace

namespace openspace {

// SyncableMessageQueue::SyncableMessageQueue() {
//     _queue.clear();
// };

// void SyncableMessageQueue::preSync(bool isMaster) {
//     if (!isMaster) {
//         return;
//     }

//     std::lock_guard guard(_clientItemMutex);
//     while (!_incomingMessages.empty()) {
//         PeerMessage item = std::move(_incomingMessages.front());
//         _incomingMessages.pop();

//         _queueToSync.push_back(item);
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

void SyncableMessageQueue::encode(SyncBuffer* syncBuffer) {
    std::lock_guard guard(_mutex);
    
    size_t nItems = size();
    syncBuffer->encode(nItems);

    for (auto m : _messagesToSync) {
        syncBuffer->encode(static_cast<uint32_t>(m.message.type));
        LWARNING(fmt::format("message.type: {}", static_cast<uint32_t>(m.message.type)));
        size_t nChars = m.message.content.size();
        syncBuffer->encode(nChars);
        LWARNING(fmt::format("nChars: {}", nChars));
        std::string s( m.message.content.begin(), m.message.content.end() );
        syncBuffer->encode(s);
        // syncBuffer->encode(m.message.content);
        // LWARNING(fmt::format("m.message.content: {}", m.message.content));
    }
    _messagesToSync.clear();
}

void SyncableMessageQueue::decode(SyncBuffer* syncBuffer) {
    std::lock_guard guard(_mutex);
    
    size_t nItems;
    syncBuffer->decode(nItems);

    for (size_t i = 0; i < nItems; ++i) {
        uint32_t _type;
        syncBuffer->decode(_type);
        SoftwareConnection::MessageType type { _type };

        size_t nChars;
        std::string content;
        // std::vector<char> content;
        syncBuffer->decode(nChars);
        content.resize(nChars);
        syncBuffer->decode(content);
        
        std::vector<char> _content{ content.begin(), content.end() };

        PeerMessage item{ 0, SoftwareConnection::Message { type, _content } };
        _queue.push_back(std::move(item));
    }
}

void SyncableMessageQueue::postSync(bool isMaster) {
    if (isMaster) {
        if (size() > 0) {
            LWARNING(fmt::format("SyncableMessageQueue.size() (MASTER): {}", size()));
        }
    //     while (!_queue.empty()) {
    //         T item = _queue.front();
    //         _queue.pop();
            
    //         std::string msg = { std::begin(item.data().content), std::end(item.data().content) };
    //         LWARNING(fmt::format("Item (MASTER): {}", msg));

    // //         std::string script = std::move(_masterScriptQueue.front().script);
    // //         ScriptCallback callback = std::move(_masterScriptQueue.front().callback);
    // //         _masterScriptQueue.pop();
    // //         try {
    // //             runScript(script, callback);
    // //         }
    // //         catch (const ghoul::RuntimeError& e) {
    // //             LERRORC(e.component, e.message);
    // //             continue;
    // //         }
    //     }
    }
    else {
        if (size() > 0) {
            LWARNING(fmt::format("SyncableMessageQueue.size() (CLIENT): {}", size()));
        }
    //     std::lock_guard<std::mutex> guard(_clientMessageMutex);
    //     while (!_queue.empty()) {
    //         T item = _queue.front();
    //         _queue.pop();

    //         std::string msg = { std::begin(item.data().content), std::end(item.data().content) };
    //         LWARNING(fmt::format("Message (CLIENT): {}", msg));
    // //         try {
    // //             runScript(_slaveScriptQueue.front());
    // //             _slaveScriptQueue.pop();
    // //         }
    // //         catch (const ghoul::RuntimeError& e) {
    // //             LERRORC(e.component, e.message);
    // //         }
    //     }
    }
}


void SyncableMessageQueue::push(PeerMessage &&item) {
    _messagesToSync.push_back(item);
    _queue.push_back(item);
}

void SyncableMessageQueue::push(const PeerMessage& item) {
    _messagesToSync.push_back(item);
    _queue.push_back(item);
}

PeerMessage SyncableMessageQueue::pop() {
    PeerMessage item = front();
    _queue.pop_front();
    return item;
}

// void SyncableMessageQueue::pop(PeerMessage& item) {
//     _queue.pop_front(item);
// }

size_t SyncableMessageQueue::size() const {
    return _queue.size();
}

bool SyncableMessageQueue::empty() const {
    return _queue.empty();
}

PeerMessage& SyncableMessageQueue::front() {
    return _queue.front();
}

const PeerMessage& SyncableMessageQueue::front() const {
    return _queue.front();
}

PeerMessage& SyncableMessageQueue::back() {
    return _queue.back();
}

const PeerMessage& SyncableMessageQueue::back() const {
    return _queue.back();
}

} // namespace openspace
