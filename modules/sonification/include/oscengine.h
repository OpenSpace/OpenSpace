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

#ifndef __OPENSPACE_MODULE_SONIFICATION___OSCENGINE___H__
#define __OPENSPACE_MODULE_SONIFICATION___OSCENGINE___H__

#include <modules/sonification/ext/osc/ip/UdpSocket.h>
#include <modules/sonification/ext/osc/osc/OscOutboundPacketStream.h>

namespace openspace {

class OscEngine {
public:
    enum class OscDataType {
        Blob = 0,
        Double,
        Int,
        String
    };

    struct OscDataEntry {
        osc::Blob blobValue;
        int intValue;
        double doubleValue;
        std::string stringValue;

        OscDataType type;
    };

    OscEngine(const std::string& ip, int port);
    ~OscEngine();

    void send(const std::string& label, const std::vector<OscDataEntry>& data);

private:
    UdpTransmitSocket _socket;
    osc::OutboundPacketStream _stream;
    char* _buffer;
};

} // openspace namespace

#endif // __OPENSPACE_MODULE_SONIFICATION___OSCENGINE___H__
