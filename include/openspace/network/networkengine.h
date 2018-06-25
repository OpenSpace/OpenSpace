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

#ifndef __OPENSPACE_CORE___NETWORKENGINE___H__
#define __OPENSPACE_CORE___NETWORKENGINE___H__

#include <cstdint>
#include <map>
#include <string>
#include <vector>

namespace openspace {

class NetworkEngine {
public:
    using MessageIdentifier = uint16_t;

    NetworkEngine();

    // Receiving messages
    bool handleMessage(const std::string& message);

    // Sending messages
    void publishStatusMessage();
    void publishIdentifierMappingMessage();
    void publishMessage(MessageIdentifier identifier, std::vector<char> message);
    void sendMessages();

    // Initial Connection Messages
    void setInitialConnectionMessage(MessageIdentifier identifier,
        std::vector<char> message);
    void sendInitialInformation();

    // Background
    MessageIdentifier identifier(std::string name);

private:
    std::map<std::string, MessageIdentifier> _identifiers;
    MessageIdentifier _lastAssignedIdentifier = MessageIdentifier(-1);

    struct Message {
        MessageIdentifier identifer;
        std::vector<char> body;
    };
    std::vector<Message> _messagesToSend;

    std::vector<Message> _initialConnectionMessages;

    bool _shouldPublishStatusMessage = true;

    MessageIdentifier _statusMessageIdentifier;
    MessageIdentifier _identifierMappingIdentifier;
    MessageIdentifier _initialMessageFinishedIdentifier;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___NETWORKENGINE___H__
