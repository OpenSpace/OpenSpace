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
#include <modules/softwareintegration/utils.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <openspace/properties/property.h>

#include <unordered_set>
#include <unordered_map>

namespace openspace {

class Renderable;

class SoftwareConnection {
public:
    using OnChangeHandle = properties::Property::OnChangeHandle;
    struct PropertySubscription {
        OnChangeHandle onChangeHandle;
        bool shouldSendMessage{true};
    };
    using PropertyName = std::string;
    using PropertySubscriptions = std::unordered_map<PropertyName, OnChangeHandle>;
    using Identifier = std::string;
    using SubscribedProperties = std::unordered_map<Identifier, PropertySubscriptions>;

    struct Message {
        softwareintegration::simp::MessageType type;
        std::vector<char> content{};
        std::string rawMessageType{""};
    };

    class SoftwareConnectionLostError;

    explicit SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket);
    SoftwareConnection(SoftwareConnection&& p);
    ~SoftwareConnection();

    void disconnect();
    bool isConnected() const;
    bool isConnectedOrConnecting() const;
    bool sendMessage(const std::string& message);

    void addPropertySubscription(
        const std::string& propertyName,
        const std::string& identifier,
        std::function<void()> newHandler
    );
    // std::shared_ptr<PropertySubscription> getPropertySubscription(
    //     const std::string& propertyName,
    //     const std::string& identifier
    // );

    SoftwareConnection::Message receiveMessageFromSoftware();

    void addSceneGraphNode(const std::string& identifier);
    void removeSceneGraphNode(const std::string& identifier);

    size_t id();
    size_t nConnections();
    void setThread(std::thread& t);
    bool shouldStopThread();

    class NetworkEngineFriends {
    private:
        static void stopThread(std::shared_ptr<SoftwareConnection> connectionPtr);
        friend class NetworkEngine;
    };

    class PointDataMessageHandlerFriends {
    private:
        static void removePropertySubscription(
            std::shared_ptr<SoftwareConnection> connectionPtr,
            const std::string& propertyName,
            const std::string& identifier
        );
        friend class PointDataMessageHandler;
    };

private:
    void removePropertySubscriptions(const std::string& identifier);
    void removeExistingPropertySubscription(
        const std::string& identifier,
        properties::Property *property,
        OnChangeHandle onChangeHandle
    );

    SubscribedProperties _subscribedProperties;

    std::unordered_set<std::string> _sceneGraphNodes;

    bool _isConnected = true;
    std::unique_ptr<ghoul::io::TcpSocket> _socket;

    size_t _id;
    std::thread _thread;
    std::atomic_bool _shouldStopThread;

    static std::atomic_size_t _nextConnectionId;
};

class SoftwareConnection::SoftwareConnectionLostError : public ghoul::RuntimeError {
public:
    explicit SoftwareConnectionLostError(const std::string& msg);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWAREINTEGRATIONMODULE___H__
