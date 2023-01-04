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

#include <modules/sonification/include/oscengine.h>

namespace {
    constexpr int BufferSize = 1024;

} // namespace

namespace openspace {

OscEngine::OscEngine(const std::string& ip, int port)
    : _socket(IpEndpointName(ip.c_str(), port))
{
    // Create buffer and stream that will be used to send messages to SuperCollider
    _buffer = new char[BufferSize];
    _stream = osc::OutboundPacketStream(_buffer, BufferSize);
}

OscEngine::~OscEngine() {
    delete[] _buffer;
}

void OscEngine::send(const std::string& label, const std::vector<OscDataEntry>& data)
{
    _stream.Clear();
    _stream << osc::BeginMessage(label.c_str());

    for (size_t i = 0; i < data.size(); ++i) {
        switch (data[i].type) {
            case OscEngine::OscDataType::Blob:
                _stream << data[i].blobValue;
                break;
            case OscEngine::OscDataType::Double:
                _stream << data[i].doubleValue;
                break;
            case OscEngine::OscDataType::Int:
                _stream << data[i].intValue;
                break;
            case OscEngine::OscDataType::String:
                _stream << data[i].stringValue.c_str();
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }

    _stream  << osc::EndMessage;
    _socket.Send(_stream.Data(), _stream.Size());
}

} // openspace namespace
