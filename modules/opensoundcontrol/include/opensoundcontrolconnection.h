/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_OPENSOUNDCONTROL___OPENSOUNDCONTROLCONNECTION___H__
#define __OPENSPACE_MODULE_OPENSOUNDCONTROL___OPENSOUNDCONTROLCONNECTION___H__

#include <modules/opensoundcontrol/ext/oscpack/ip/UdpSocket.h>
#include <modules/opensoundcontrol/ext/oscpack/osc/OscOutboundPacketStream.h>
#include <string>
#include <variant>
#include <vector>

namespace openspace {

using OpenSoundControlDataType = std::variant<osc::Blob, double, int, std::string>;

class OpenSoundControlConnection {
public:
    OpenSoundControlConnection(const std::string& ip, int port);
    ~OpenSoundControlConnection();

    void send(const std::string& label,
        const std::vector<OpenSoundControlDataType>& data);
private:
    UdpTransmitSocket _socket;
    char* _buffer = nullptr;
    osc::OutboundPacketStream _stream;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_OPENSOUNDCONTROL___OPENSOUNDCONTROLCONNECTION___H__
