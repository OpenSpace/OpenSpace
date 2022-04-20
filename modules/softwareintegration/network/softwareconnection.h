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
    enum class MessageType : uint32_t {
        Connection = 0,
        ReadPointData,
        RemoveSceneGraphNode,
        Color,
        Opacity,
        Size,
        Visibility,
        Disconnection
    };

    static std::map<std::string, MessageType> mapSIMPTypeToMessageType;

    // struct SIMPMessageType {
    //     static inline const std::string Connection = "CONN";
    //     static inline const std::string ReadPointData = "PDAT";
    //     static inline const std::string RemoveSceneGraphNode = "RSGN";
    //     static inline const std::string Color = "UPCO";
    //     static inline const std::string Opacity = "UPOP";
    //     static inline const std::string Size = "UPSI";
    //     static inline const std::string Visibility = "TOVI";
    //     static inline const std::string Disconnection = "DISC";
    // };

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

    SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket);

    bool isConnected() const;
    bool isConnectedOrConnecting() const;
    bool sendMessage(std::string message);
    void disconnect();

    ghoul::io::TcpSocket* socket();

    SoftwareConnection::Message receiveMessageFromSoftware();

    // static const float ProtocolVersion;
    static const std::string ProtocolVersion;

private:
    bool _isListening = true;
    std::unique_ptr<ghoul::io::TcpSocket> _socket;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWAREINTEGRATIONMODULE___H__
