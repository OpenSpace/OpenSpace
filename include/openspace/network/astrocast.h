/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___ASTROCAST___H__
#define __OPENSPACE_CORE___ASTROCAST___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/network/messagestructures.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/timemanager.h>
#include <ghoul/designpattern/event.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/misc/exception.h>
#include <atomic>
#include <cstdint>
#include <deque>
#include <memory>
#include <mutex>
#include <optional>
#include <thread>
#include <vector>

namespace openspace {

struct LuaLibrary;

class Astrocast : public PropertyOwner {
public:
    enum class Status : uint32_t {
        Disconnected = 0,
        Connecting,
        ClientWithoutHost,
        ClientWithHost,
        Host
    };

    enum class MessageType : uint8_t {
        Authentication = 0,
        Data,
        ConnectionStatus,
        HostshipRequest,
        HostshipResignation,
        NConnections
    };

    struct Message {
        Message() = default;
        Message(MessageType t, std::vector<char> c);

        MessageType type;
        std::vector<char> content;
    };

    struct DataMessage {
        DataMessage() = default;
        DataMessage(datamessagestructures::Type t, double time, std::vector<char> c);

        datamessagestructures::Type type;
        double timestamp;
        std::vector<char> content;
    };

    class ConnectionLostError final : public ghoul::RuntimeError {
    public:
        explicit ConnectionLostError(bool shouldLogError_ = true);

        bool shouldLogError;
    };

    Astrocast();
    ~Astrocast() override;

    void connect();
    void setPort(std::string port);
    void setAddress(std::string address);
    void setServerName(std::string name);
    void setName(std::string name);
    bool isHost();
    const std::string& hostName();
    void requestHostship(std::optional<std::string> hostPassword = std::nullopt);
    void resignHostship();
    void setPassword(std::string password);
    void setHostPassword(std::string hostPassword);
    void disconnect();
    void preSynchronization();
    void sendScript(std::string script);
    void resetTimeOffset();
    double latencyStandardDeviation() const;

    /**
     * Returns the Lua library that contains all Lua functions available to affect the
     * remote OpenSpace astrocast connection.
     */
    static LuaLibrary luaLibrary();
    Status status();
    int nConnections();
    ghoul::Event<>& connectionEvent();

private:
    void queueInMessage(const Message& message);

    void sendAuthentication();
    void handleCommunication();

    void handleMessage(const Message&);
    void dataMessageReceived(const std::vector<char>& message);
    void connectionStatusMessageReceived(const std::vector<char>& message);
    void nConnectionsMessageReceived(const std::vector<char>& message);

    void sendCameraKeyframe();
    void sendTimeTimeline();

    void setStatus(Status status);
    void setHostName(const std::string& hostName);
    void setNConnections(size_t nConnections);

    double convertTimestamp(double messageTimestamp);
    void analyzeTimeDifference(double messageTimestamp);

    bool isConnectedOrConnecting() const;
    void sendDataMessage(const DataMessage& dataMessage);
    bool sendMessage(const Message& message);

    Message receiveMessage();

    // Gonna do some UTF-like magic once we reach 255 to introduce a second byte or so
    static constexpr uint8_t ProtocolVersion = 7;


    StringProperty _password;
    StringProperty _hostPassword;
    StringProperty _serverName;

    // While the port should in theory be an int, we use a StringProperty to avoid a
    // slider in the GUI
    StringProperty _port;
    StringProperty _address;
    StringProperty _name;
    FloatProperty _bufferTime;
    FloatProperty _timeKeyframeInterval;
    FloatProperty _cameraKeyframeInterval;

    double _lastTimeKeyframeTimestamp = 0.0;
    double _lastCameraKeyframeTimestamp = 0.0;

    std::atomic_bool _shouldDisconnect = false;

    std::atomic<size_t> _nConnections = 0;
    std::atomic<Status> _status = Status::Disconnected;

    std::string _hostName;

    std::deque<Message> _receiveBuffer;
    std::mutex _receiveBufferMutex;

    std::atomic<bool> _timeJumped;
    std::atomic<bool> _timeTimelineChanged;
    std::mutex _latencyMutex;
    std::deque<double> _latencyDiffs;
    double _initialTimeDiff = 0.0;

    std::unique_ptr<std::thread> _receiveThread = nullptr;
    std::shared_ptr<ghoul::Event<>> _connectionEvent;

    TimeManager::CallbackHandle _timeJumpCallback = -1;
    TimeManager::CallbackHandle _timeTimelineChangeCallback = -1;

    std::unique_ptr<ghoul::io::TcpSocket> _socket;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASTROCAST___H__
