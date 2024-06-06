/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                              *
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

#include <modules/osc/include/oscconnection.h>

#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "OscConnection";
    constexpr int BufferSize = 1024;

    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...)->overloaded<Ts...>;
} // namespace

namespace openspace {

OscConnection::OscConnection(const std::string& ip, int port)
    : _socket(IpEndpointName(ip.c_str(), port))
{
    // Create buffer and stream that will be used to send messages
    _buffer = new char[BufferSize];
    _stream = osc::OutboundPacketStream(_buffer, BufferSize);
}

OscConnection::~OscConnection() {
    delete[] _buffer;
}

void OscConnection::send(const std::string& label, const std::vector<OscDataType>& data)
{
    if (label.empty()) {
        LERROR("Cannot send osc message without label");
        return;
    }

    _stream.Clear();
    _stream << osc::BeginMessage(label.c_str());

    LDEBUG(std::format("Sending: {}", label));

    for (const OscDataType& item : data) {
        std::visit(overloaded {
            [this](const osc::Blob& value) {
                _stream << value;
            },
            [this](double value) {
                _stream << value;
            },
            [this](int value) {
                _stream << value;
            },
            [this](const std::string& value) {
                _stream << value.c_str();
            },
            [this](auto v) {
                _stream << osc::EndMessage;
                throw ghoul::MissingCaseException();
            },
        }, item);
    }

    _stream << osc::EndMessage;
    _socket.Send(_stream.Data(), _stream.Size());
}

} // namespace openspace
