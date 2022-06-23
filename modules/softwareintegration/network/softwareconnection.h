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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWARECONNECTION___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWARECONNECTION___H__

#include <modules/softwareintegration/simp/simp.h>
#include <openspace/network/messagestructures.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <openspace/properties/property.h>

#include <unordered_map>

namespace openspace {

class Renderable;
class SoftwareConnection;

namespace softwareintegration::network {

struct NetworkState;
struct IncomingMessage;

namespace connection {
    
    void eventLoop(
        std::weak_ptr<SoftwareConnection> connectionWeakPtr,
        std::weak_ptr<softwareintegration::network::NetworkState> networkStateWeakPtr
    );

    IncomingMessage receiveMessageFromSoftware(
        std::shared_ptr<SoftwareConnection> connectionPtr
    );

} // namespace connection

} // namespace softwareintegration::network

using namespace softwareintegration::network;

class SoftwareConnection {
public:
    struct SceneGraphNodeInfo;

    explicit SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket);
    SoftwareConnection(SoftwareConnection&& p);
    ~SoftwareConnection();

    void disconnect();
    bool isConnected() const;
    bool isConnectedOrConnecting() const;
    bool sendMessage(
        const softwareintegration::simp::MessageType& message_type,
        const std::vector<std::byte>& subjectBuffer
    );

    bool hasPropertySubscription(
        const std::string& identifier,
        const std::string& propertyName
    );

    void addPropertySubscription(
        const std::string& propertyName,
        const std::string& identifier,
        std::function<void()> newHandler
    );

    void addSceneGraphNode(const std::string& identifier);
    void removeSceneGraphNode(const std::string& identifier);

    size_t id();
    void setThread(std::thread& t);

    friend void connection::eventLoop(
        std::weak_ptr<SoftwareConnection> connectionWeakPtr,
        std::weak_ptr<NetworkState> networkStateWeakPtr
    );

    void removePropertySubscriptions(const std::string& identifier);
    void removePropertySubscription(const std::string& identifier, const std::string& propertyName);

    /**
     * @brief Called from onChange handlers to see if we should send back to
     * external software. Will also set its state to "true" when called
     * 
     */
    bool shouldSendData(const std::string& identifier, const std::string& propertyName);
    /**
     * @brief Called from message handlers to not send back data in onChange handlers
     * when properties are updated
     * 
     */
    void setShouldNotSendData(const std::string& identifier, const std::string& propertyName);

    void addToMessageQueue(
        const std::string& identifier,
        softwareintegration::simp::DataKey dataKey,
        const std::tuple<std::vector<std::byte>, int32_t>& entry
    );

    void notifyMessageQueueHandler();

    void handleOutgoingMessages();

    friend IncomingMessage connection::receiveMessageFromSoftware(
        std::shared_ptr<SoftwareConnection> connectionPtr
    );

    bool handshakeHasBeenMade();
    void setHandshakeHasBeenMade();

    ghoul::io::TcpSocket* socket();

    std::mutex&  outgoingMessagesMutex();

private:
    void removeMessageQueue(const std::string& identifier);

    std::unique_ptr<ghoul::io::TcpSocket> _socket;

    std::unordered_map<std::string, SceneGraphNodeInfo> _sceneGraphNodes;
    std::mutex _outgoingMessagesMutex;
    std::atomic_bool _shouldStopOutgoingMessagesThread;
    std::unique_ptr<std::thread> _outgoingMessagesThread;
    std::mutex _outgoingMessagesNotifierMutex;
    std::condition_variable _outgoingMessagesNotifier;

    size_t _id;
    std::thread _thread;
    std::atomic_bool _shouldStopThread;

    static std::atomic_size_t _nextConnectionId;

    bool _handshakeHasBeenMade = false;
};

struct SoftwareConnection::SceneGraphNodeInfo {
    using OnChangeHandle = properties::Property::OnChangeHandle;
    struct PropertySubscription {
        OnChangeHandle onChangehandle;
        bool shouldSendMessage{ false };
    };
    using PropertySubscriptions = std::unordered_map<std::string, PropertySubscription>;
    //                                                    /\
    //                                                    |
    //                                               propertyName

    using OutgoingMessages = std::unordered_map<
        softwareintegration::simp::DataKey,
        std::tuple<std::vector<std::byte>, int32_t>
    >;

    OutgoingMessages outgoingMessages{};
    PropertySubscriptions propertySubscriptions{};
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWAREINTEGRATIONMODULE___H__
