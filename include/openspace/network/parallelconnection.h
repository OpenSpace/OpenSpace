/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_CORE___PARALLELCONNECTION___H__
#define __OPENSPACE_CORE___PARALLELCONNECTION___H__

#include <openspace/network/messagestructures.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/numericalproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <glm/gtx/quaternion.hpp>

#include <ghoul/designpattern/event.h>

#include <string>
#include <vector>
#include <deque>
#include <atomic>
#include <thread>
#include <mutex>
#include <map>
#include <condition_variable>

#if defined(WIN32) || defined(__MING32__) || defined(__MING64__)
typedef size_t _SOCKET;
#else
typedef int _SOCKET;
#include <netdb.h>
#endif

struct addrinfo;

namespace openspace {

class ParallelConnection : public properties::PropertyOwner {
    public:
    enum class Status : uint32_t {
        Disconnected = 0,
        ClientWithoutHost,
        ClientWithHost,
        Host
    };

    enum class MessageType : uint32_t {
        Authentication = 0,
        Data,
        ConnectionStatus,
        HostshipRequest,
        HostshipResignation,
        NConnections
    };

    struct Message {
        Message() {};
        Message(MessageType t, const std::vector<char>& c)
            : type(t)
            , content(c)
        {};

        MessageType type;
        std::vector<char> content;
    };

    struct DataMessage {
        DataMessage() {};
        DataMessage(datamessagestructures::Type t, const std::vector<char>& c)
            : type(t)
            , content(c)
        {};
        datamessagestructures::Type type;
        std::vector<char> content;
    };

    ParallelConnection();
    ~ParallelConnection();
    void clientConnect();
    void setPort(std::string port);
    void setAddress(std::string address);
    void setName(std::string name);
    bool isHost();
    const std::string& hostName();
    void requestHostship();
    void resignHostship();
    void setPassword(std::string password);
    void setHostPassword(std::string hostPassword);
    void signalDisconnect();
    void preSynchronization();
    void sendScript(std::string script);
    void resetTimeOffset();
    double latencyStandardDeviation() const;
    double timeTolerance() const;

    /**
        * Returns the Lua library that contains all Lua functions available to affect the
        * remote OS parallel connection. The functions contained are
        * -
        * \return The Lua library that contains all Lua functions available to affect the
        * interaction
        */
    static scripting::LuaLibrary luaLibrary();
    Status status();
    int nConnections();
    std::shared_ptr<ghoul::Event<>> connectionEvent();

            
            
private:
    //@TODO change this into the ghoul hasher for client AND server
    uint32_t hash(const std::string &val);
    void queueOutMessage(const Message& message);
    void queueOutDataMessage(const DataMessage& dataMessage);
    void queueInMessage(const Message& message);

    void disconnect();
    void closeSocket();
    bool initNetworkAPI();
    void establishConnection(addrinfo *info);
    void sendAuthentication();
    void listenCommunication();
    int receiveData(_SOCKET & socket, std::vector<char> &buffer, int length, int flags);

    void handleMessage(const Message&);
    void dataMessageReceived(const std::vector<char>& messageContent);
    void connectionStatusMessageReceived(const std::vector<char>& messageContent);
    void nConnectionsMessageReceived(const std::vector<char>& messageContent);

    void sendCameraKeyframe();
    void sendTimeKeyframe();

    void sendFunc();
    void threadManagement();

    void setStatus(Status status);
    void setHostName(const std::string& hostName);
    void setNConnections(size_t nConnections);

    double calculateBufferedKeyframeTime(double originalTime);

    properties::StringProperty _password;
    properties::StringProperty _hostPassword;
    properties::StringProperty _port;
    properties::StringProperty _address;
    properties::StringProperty _name;
    properties::FloatProperty _bufferTime;
    properties::FloatProperty _timeKeyframeInterval;
    properties::FloatProperty _cameraKeyframeInterval;
    properties::FloatProperty _timeTolerance;

    double _lastTimeKeyframeTimestamp;
    double _lastCameraKeyframeTimestamp;
            
    _SOCKET _clientSocket;

    std::atomic<bool> _isConnected;
    std::atomic<bool> _isRunning;
    std::atomic<bool> _tryConnect;
    std::atomic<bool> _disconnect;
    std::atomic<bool> _initializationTimejumpRequired;

    std::atomic<size_t> _nConnections;
    std::atomic<Status> _status;
    std::string _hostName;

    std::condition_variable _disconnectCondition;
    std::mutex _disconnectMutex;
            
    std::condition_variable _sendCondition;
    std::deque<Message> _sendBuffer;
    std::mutex _sendBufferMutex;

    std::deque<Message> _receiveBuffer;
    std::mutex _receiveBufferMutex;
            
    std::atomic<bool> _timeJumped;
    std::mutex _latencyMutex;
    std::deque<double> _latencyDiffs;
    double _initialTimeDiff;

    std::unique_ptr<std::thread> _connectionThread;
    std::unique_ptr<std::thread> _sendThread;
    std::unique_ptr<std::thread> _listenThread;
    std::unique_ptr<std::thread> _handlerThread;
    std::shared_ptr<ghoul::Event<>> _connectionEvent;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PARALLELCONNECTION___H__
