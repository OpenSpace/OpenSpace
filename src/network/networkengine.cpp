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

#include <openspace/network/networkengine.h>
#include <openspace/util/time.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <array>
#include <chrono>
#include <thread>

#include <ghoul/opengl/ghoul_gl.h>

namespace {
    const std::string _loggerCat = "NetworkEngine";

    const std::string StatusMessageIdentifierName = "StatusMessage";
    const std::string MappingIdentifierIdentifierName = "IdentifierMapping";
    const std::string InitialMessageFinishedIdentifierName = "InitialMessageFinished";

    const char MessageTypeLuaScript = '0';
    const char MessageTypeExternalControlConnected = '1';
}

namespace openspace {

NetworkEngine::NetworkEngine() 
    : _lastAssignedIdentifier(MessageIdentifier(-1)) // -1 is okay as we assign one identifier in this ctor
    , _shouldPublishStatusMessage(true)
{
    static_assert(
        sizeof(MessageIdentifier) == 2,
        "MessageIdentifier has to be 2 bytes or dependent applications will break"
    );
    _statusMessageIdentifier = identifier(StatusMessageIdentifierName);
    _identifierMappingIdentifier = identifier(MappingIdentifierIdentifierName);
    _initialMessageFinishedIdentifier = identifier(InitialMessageFinishedIdentifierName);
}

bool NetworkEngine::handleMessage(const std::string& message) {
    // The first byte determines the type of message
    const char type = message[0];
    switch (type) {
    case MessageTypeLuaScript:  // LuaScript
    {
        std::string script = message.substr(1);
        //LINFO("Received Lua Script: '" << script << "'");
        OsEng.scriptEngine().queueScript(script);
        return true;
    }
    case MessageTypeExternalControlConnected:
    {
        publishIdentifierMappingMessage();
        std::this_thread::sleep_for(std::chrono::milliseconds(250));
        sendInitialInformation();
        return true;
    }
    default:
        LERROR("Unknown type '" << type << "'");
        return false;
    }

}

void NetworkEngine::publishStatusMessage() {
    if (!_shouldPublishStatusMessage || !OsEng.windowWrapper().isExternalControlConnected())
        return;
    // Protocol:
    // 8 bytes: time as a ET double
    // 24 bytes: time as a UTC string
    // 8 bytes: delta time as double
    // Total: 40

    uint16_t messageSize = 0;
    
    double time = Time::ref().j2000Seconds();
    std::string timeString = Time::ref().UTC();
    double delta = Time::ref().deltaTime();

    messageSize += sizeof(time);
    messageSize += static_cast<uint16_t>(timeString.length());
    messageSize += sizeof(delta);

    ghoul_assert(messageSize == 40, "Message size is not correct");

    unsigned int currentLocation = 0;
    std::vector<char> buffer(messageSize);
    
    std::memmove(buffer.data() + currentLocation, &time, sizeof(time));
    currentLocation += sizeof(time);
    std::memmove(buffer.data() + currentLocation, timeString.c_str(), timeString.length());
    currentLocation += static_cast<unsigned int>(timeString.length());
    std::memmove(buffer.data() + currentLocation, &delta, sizeof(delta));

    publishMessage(_statusMessageIdentifier, std::move(buffer));
}

void NetworkEngine::publishIdentifierMappingMessage() {
    size_t bufferSize = sizeof(uint16_t);
    for (const std::pair<std::string, MessageIdentifier>& i : _identifiers) {
        bufferSize += sizeof(MessageIdentifier);
        bufferSize += i.first.size() + 1; // +1 for \0 terminating character
    }

    std::vector<char> buffer(bufferSize);
    size_t currentWritingPosition = 0;
    uint16_t size = _identifiers.size();
    std::memcpy(buffer.data(), &size, sizeof(uint16_t));
    currentWritingPosition += sizeof(uint16_t);
    for (const std::pair<std::string, MessageIdentifier>& i : _identifiers) {
        std::memcpy(buffer.data() + currentWritingPosition, &(i.second), sizeof(MessageIdentifier));
        currentWritingPosition += sizeof(MessageIdentifier);
        uint8_t stringSize = i.first.size();
        std::memcpy(buffer.data() + currentWritingPosition, &stringSize, sizeof(uint8_t));
        currentWritingPosition += sizeof(uint8_t);
        std::memcpy(buffer.data() + currentWritingPosition, i.first.data(), stringSize);
        currentWritingPosition += i.first.size();
    }

    publishMessage(_identifierMappingIdentifier, std::move(buffer));
}


NetworkEngine::MessageIdentifier NetworkEngine::identifier(std::string name) {
    auto i = _identifiers.find(name);
    if (i != _identifiers.end())
        return i->second;
    else {
        _lastAssignedIdentifier++;

        MessageIdentifier result = _lastAssignedIdentifier;

        _identifiers[std::move(name)] = result;
        return result;
    }
}

void NetworkEngine::publishMessage(MessageIdentifier identifier, std::vector<char> message) {
    _messagesToSend.push_back({ std::move(identifier), std::move(message) });
}

void NetworkEngine::sendMessages() {
    if (!OsEng.windowWrapper().isExternalControlConnected())
        return;

    for (Message& m : _messagesToSend) {
        // Protocol:
        // 2 bytes: type of message as uint16_t
        // Rest of payload depending on the message type

        union {
            MessageIdentifier value;
            std::array<char, 2> data;
        } identifier;
        identifier.value = m.identifer;

        // Prepending the message identifier to the front
        m.body.insert(m.body.begin(), identifier.data.begin(), identifier.data.end());
        OsEng.windowWrapper().sendMessageToExternalControl(m.body);
    }

    _messagesToSend.clear();
}

void NetworkEngine::sendInitialInformation() {
    static const int SleepTime = 250;
    _shouldPublishStatusMessage = false;
    for (const Message& m : _initialConnectionMessages) {
        union {
            MessageIdentifier value;
            std::array<char, 2> data;
        } identifier;
        identifier.value = m.identifer;

        std::vector<char> payload = m.body;
        payload.insert(payload.begin(), identifier.data.begin(), identifier.data.end());
        OsEng.windowWrapper().sendMessageToExternalControl(payload);
        LINFO("Sent initial message: (s=" << m.body.size() << ") [i=" << identifier.value << "]");

        std::this_thread::sleep_for(std::chrono::milliseconds(SleepTime));
    }

    std::this_thread::sleep_for(std::chrono::milliseconds(4 * SleepTime));

    // Send finished message
    union {
        MessageIdentifier value;
        std::array<char, 2> data;
    } identifier;
    identifier.value = _initialMessageFinishedIdentifier;

    std::vector<char> d;
    d.insert(d.begin(), identifier.data.begin(), identifier.data.end());

    OsEng.windowWrapper().sendMessageToExternalControl(d);
    _shouldPublishStatusMessage = true;
}

void NetworkEngine::setInitialConnectionMessage(MessageIdentifier identifier, std::vector<char> message) {
    // Add check if a MessageIdentifier already exists ---abock
    _initialConnectionMessages.push_back({std::move(identifier), std::move(message)});
}

} // namespace openspace
