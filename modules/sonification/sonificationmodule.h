/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
#define __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__

#include "modules/sonification/ext/osc/ip/UdpSocket.h"
#include "modules/sonification/ext/osc/osc/OscOutboundPacketStream.h"
#include "modules/sonification/include/sonificationhelper.h"
#include <string>
#include <thread>
#include <atomic>
#include <ghoul/glm.h>

#include <openspace/util/openspacemodule.h>

namespace openspace {

class SonificationModule : public OpenSpaceModule {
public:
    SonificationModule();
    ~SonificationModule();

    //Send message to supercollider, address the message with label
    //NOTE: label must be in the format: "/label"
    void sendMesssage(const std::string label, const float message);

    //Send message to supercollider, address the message with label
    //NOTE: label must be in the format: "/label"
    void sendMesssage(const std::string label, const std::string message);

    //Send message to supercollider, address the message with label
    //NOTE: label must be in the format: "/label"
    void sendMesssage(const std::string label, const glm::dvec3 message);

protected:
    void internalInitialize(const ghoul::Dictionary& dictionary) override;

private:
    char* _buffer;
    osc::OutboundPacketStream _stream;
    SonificationHelper _helper;
    std::thread _thread;
    std::atomic<bool> _isRunning;

    void threadFunk(std::atomic<bool>& isRunning);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
