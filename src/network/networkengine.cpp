/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include "sgct.h"

namespace {
    const std::string _loggerCat = "NetworkEngine";

    const uint8_t MessageTypeStatus = 0;
}

namespace openspace {

bool NetworkEngine::handleMessage(const std::string& message) {
    // The first byte determines the type of message
    const char type = message[0];
    switch (type) {
    case '0':  // LuaScript
    {
        std::string script = message.substr(1);
        //LINFO("Received Lua Script: '" << script << "'");
        OsEng.scriptEngine()->queueScript(script);
        return true;
    }
    default:
        LERROR("Unknown type '" << type << "'");
        return false;
    }

}

void NetworkEngine::sendStatusMessage() {
    if (!sgct::Engine::instance()->isExternalControlConnected())
        return;
    // Protocols:
    // 1 byte: type of message
    // 8 bytes: time as a ET double
    // 24 bytes: time as a UTC string
    // 8 bytes: delta time as double
    // Total: 41

    uint16_t messageSize = 0;
    
    double time = Time::ref().currentTime();
    std::string timeString = Time::ref().currentTimeUTC();
    double delta = Time::ref().deltaTime();

    messageSize += sizeof(uint8_t);
    messageSize += sizeof(time);
    messageSize += timeString.length();
    messageSize += sizeof(delta);

    //LINFO(delta);

    ghoul_assert(messageSize == 41, "Message size is not correct");

    unsigned int currentLocation = 0;
    std::vector<char> buffer(messageSize);
    
    std::memcpy(buffer.data(), &MessageTypeStatus, sizeof(MessageTypeStatus));
    currentLocation += sizeof(MessageTypeStatus);
    std::memmove(buffer.data() + currentLocation, &time, sizeof(time));
    currentLocation += sizeof(time);
    std::memmove(buffer.data() + currentLocation, timeString.c_str(), timeString.length());
    currentLocation += timeString.length();
    std::memmove(buffer.data() + currentLocation, &delta, sizeof(delta));

    sgct::Engine::instance()->sendMessageToExternalControl(buffer.data(), messageSize);
}

} // namespace openspace
