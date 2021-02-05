/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWARECONNECTION___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWARECONNECTION___H__

#include <openspace/network/messagestructures.h>
#include <ghoul/io/socket/tcpsocket.h>

namespace openspace {

class SoftwareConnection {
public:
    enum class Status : uint32_t {
        Disconnected = 0,
        Connecting
    };

    enum class MessageType : uint32_t {
        Connection = 0,
        ReadPointData,
        AddSceneGraphNode,
        RemoveSceneGraphNode,
        Color,
        Opacity,
        Size,
        Visibility,
        Disconnection
    };

    struct Message {
        Message() = default;
        Message(MessageType type, std::vector<char> content);

        MessageType type;
        std::vector<char> content;
    };

    class SoftwareConnectionLostError : public ghoul::RuntimeError {
    public:
        explicit SoftwareConnectionLostError();
    };

    SoftwareConnection() = default;
    SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket);

    bool isConnectedOrConnecting() const;
    bool sendMessage(std::string message);
    void disconnect();

    ghoul::io::TcpSocket* socket();

    SoftwareConnection::Message receiveMessage();

    static const float ProtocolVersion;

private:
    bool _isListening = true;
    std::unique_ptr<ghoul::io::TcpSocket> _socket;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWAREINTEGRATIONMODULE___H__
