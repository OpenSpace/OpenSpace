/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___PARALLELPEER___H__
#define __OPENSPACE_CORE___PARALLELPEER___H__

#include <openspace/network/parallelconnection.h>
#include <openspace/network/messagestructures.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/numericalproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <glm/gtx/quaternion.hpp>

#include <ghoul/designpattern/event.h>
#include <ghoul/io/socket/tcpsocket.h>

#include <string>
#include <vector>
#include <deque>
#include <atomic>
#include <thread>
#include <mutex>
#include <map>
#include <condition_variable>

namespace openspace {

class ParallelPeer : public properties::PropertyOwner {
public:
    ParallelPeer();
    ~ParallelPeer();
    void connect();
    void setPort(std::string port);
    void setAddress(std::string address);
    void setName(std::string name);
    bool isHost();
    const std::string& hostName();
    void requestHostship();
    void resignHostship();
    void setPassword(std::string password);
    void setHostPassword(std::string hostPassword);
    void disconnect();
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
    ParallelConnection::Status status();
    int nConnections();
    std::shared_ptr<ghoul::Event<>> connectionEvent();



private:
    //@TODO change this into the ghoul hasher for client AND server
    uint32_t hash(const std::string &val);
    void sendDataMessage(const ParallelConnection::DataMessage& dataMessage);
    void sendMessage(const ParallelConnection::Message& message);
    void queueInMessage(const ParallelConnection::Message& message);

    void sendAuthentication();
    void handleCommunication();

    void handleMessage(const ParallelConnection::Message&);
    void dataMessageReceived(const std::vector<char>& messageContent);
    void connectionStatusMessageReceived(const std::vector<char>& messageContent);
    void nConnectionsMessageReceived(const std::vector<char>& messageContent);

    void sendCameraKeyframe();
    void sendTimeKeyframe();

    void setStatus(ParallelConnection::Status status);
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

    std::unique_ptr<ghoul::io::TcpSocket> _socket;
    std::atomic<bool> _shouldDisconnect;

    std::atomic<size_t> _nConnections;
    std::atomic<ParallelConnection::Status> _status;
    std::string _hostName;

    std::deque<ParallelConnection::Message> _receiveBuffer;
    std::mutex _receiveBufferMutex;

    std::atomic<bool> _timeJumped;
    std::mutex _latencyMutex;
    std::deque<double> _latencyDiffs;
    double _initialTimeDiff;

    std::unique_ptr<std::thread> _receiveThread;
    std::shared_ptr<ghoul::Event<>> _connectionEvent;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PARALLELPEER___H__
