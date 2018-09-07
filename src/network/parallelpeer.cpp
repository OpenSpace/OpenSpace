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

#include <openspace/network/parallelpeer.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/camera.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/socket/tcpsocket.h>

#include "parallelpeer_lua.inl"

namespace {
    constexpr const uint32_t ProtocolVersion = 3;
    constexpr const size_t MaxLatencyDiffs = 64;
    constexpr const char* _loggerCat = "ParallelPeer";

    constexpr openspace::properties::Property::PropertyInfo PasswordInfo = {
        "Password",
        "Password",
        "The general password that allows this OpenSpace instance access to the Wormhole "
        "server."
    };

    constexpr openspace::properties::Property::PropertyInfo HostPasswordInfo = {
        "HostPassword",
        "Host Password",
        "The password that is required to take control of the joint session and thus "
        "send all commands to connected clients."
    };

    constexpr openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The port on which the Wormhole server is listening to connections from "
        "OpenSpace."
    };

    constexpr openspace::properties::Property::PropertyInfo AddressInfo = {
        "Address",
        "Address",
        "The address of the Wormhole server either as a DNS name or an IP address."
    };

    constexpr openspace::properties::Property::PropertyInfo NameInfo = {
        "Name",
        "Connection Name",
        "The name of this OpenSpace instance that will be potentially broadcast to other "
        "connected instances."
    };

    constexpr openspace::properties::Property::PropertyInfo BufferTimeInfo = {
        "BufferTime",
        "Buffer Time",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TimeKeyFrameInfo = {
        "TimeKeyframeInterval",
        "Time keyframe interval",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo CameraKeyFrameInfo = {
        "CameraKeyframeInterval",
        "Camera Keyframe interval",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

ParallelPeer::ParallelPeer()
    : properties::PropertyOwner({ "ParallelPeer", "Parallel Peer" })
    , _password(PasswordInfo)
    , _hostPassword(HostPasswordInfo)
    , _port(PortInfo)
    , _address(AddressInfo)
    , _name(NameInfo)
    , _bufferTime(BufferTimeInfo, 0.2f, 0.01f, 5.0f)
    , _timeKeyframeInterval(TimeKeyFrameInfo, 0.1f, 0.f, 1.f)
    , _cameraKeyframeInterval(CameraKeyFrameInfo, 0.1f, 0.f, 1.f)
    , _connectionEvent(std::make_shared<ghoul::Event<>>())
    , _connection(nullptr)
{
    addProperty(_name);
    addProperty(_port);
    addProperty(_address);
    addProperty(_bufferTime);

    addProperty(_password);
    addProperty(_hostPassword);

    addProperty(_timeKeyframeInterval);
    addProperty(_cameraKeyframeInterval);
}

ParallelPeer::~ParallelPeer() {
    disconnect();
    if (_timeJumpCallback != -1) {
        global::timeManager.removeTimeJumpCallback(_timeJumpCallback);
    }
    if (_timeJumpCallback != -1) {
        global::timeManager.removeTimeJumpCallback(_timeJumpCallback);
    }

}

void ParallelPeer::connect() {
    disconnect();

    setStatus(ParallelConnection::Status::Connecting);

    std::unique_ptr<ghoul::io::TcpSocket> socket = std::make_unique<ghoul::io::TcpSocket>(
        _address,
        atoi(_port.value().c_str())
    );

    socket->connect();
    _connection = ParallelConnection(std::move(socket));

    sendAuthentication();

    _receiveThread = std::make_unique<std::thread>([this]() { handleCommunication(); });
}

void ParallelPeer::disconnect() {
    _shouldDisconnect = true;
    _connection.disconnect();
    if (_receiveThread && _receiveThread->joinable()) {
        _receiveThread->join();
        _receiveThread = nullptr;
    }
    _shouldDisconnect = false;
    setStatus(ParallelConnection::Status::Disconnected);
}

void ParallelPeer::sendAuthentication() {
    std::string name = _name;
    // Length of this nodes name
    const uint32_t nameLength = static_cast<uint32_t>(name.length());

    // Total size of the buffer: (passcode + namelength + name)
    const size_t size = sizeof(uint64_t) + sizeof(uint32_t) + nameLength;

    // Create and reserve buffer
    std::vector<char> buffer;
    buffer.reserve(size);

    const uint64_t passCode = std::hash<std::string>{}(_password.value());

    // Write the hashed password to buffer
    buffer.insert(
        buffer.end(),
        reinterpret_cast<const char*>(&passCode),
        reinterpret_cast<const char*>(&passCode) + sizeof(uint64_t)
    );

    // Write the length of the nodes name to buffer
    buffer.insert(
        buffer.end(),
        reinterpret_cast<const char*>(&nameLength),
        reinterpret_cast<const char*>(&nameLength) + sizeof(uint32_t)
    );

    // Write this node's name to buffer
    buffer.insert(buffer.end(), name.begin(), name.end());

    // Send message
    _connection.sendMessage(ParallelConnection::Message(
        ParallelConnection::MessageType::Authentication,
        buffer
    ));
}

void ParallelPeer::queueInMessage(const ParallelConnection::Message& message) {
    std::lock_guard<std::mutex> unqlock(_receiveBufferMutex);
    _receiveBuffer.push_back(message);
}

void ParallelPeer::handleMessage(const ParallelConnection::Message& message) {
    switch (message.type) {
        case ParallelConnection::MessageType::Data:
            dataMessageReceived(message.content);
            break;
        case ParallelConnection::MessageType::ConnectionStatus:
            connectionStatusMessageReceived(message.content);
            break;
        case ParallelConnection::MessageType::NConnections:
            nConnectionsMessageReceived(message.content);
            break;
        default:
            //unknown message type
            break;
    }
}

void ParallelPeer::analyzeTimeDifference(double messageTimestamp) {
    std::lock_guard<std::mutex> latencyLock(_latencyMutex);

    const double timeDiff = global::windowDelegate.applicationTime() - messageTimestamp;
    if (_latencyDiffs.empty()) {
        _initialTimeDiff = timeDiff;
    }
    const double latencyDiff = timeDiff - _initialTimeDiff;
    if (_latencyDiffs.size() >= MaxLatencyDiffs) {
        _latencyDiffs.pop_front();
    }
    _latencyDiffs.push_back(latencyDiff);
}

double ParallelPeer::convertTimestamp(double messageTimestamp) {
    std::lock_guard<std::mutex> latencyLock(_latencyMutex);
    return messageTimestamp + _initialTimeDiff + _bufferTime;
}

double ParallelPeer::latencyStandardDeviation() const {
    double accumulatedLatencyDiffSquared = 0;
    double accumulatedLatencyDiff = 0;
    for (double diff : _latencyDiffs) {
        accumulatedLatencyDiff += diff;
        accumulatedLatencyDiffSquared += diff*diff;
    }
    const double expectedLatencyDiffSquared =
        accumulatedLatencyDiffSquared / _latencyDiffs.size();

    const double expectedLatencyDiff = accumulatedLatencyDiff / _latencyDiffs.size();

    // V(X) = E(x^2) - E(x)^2
    const double latencyVariance = expectedLatencyDiffSquared -
        expectedLatencyDiff * expectedLatencyDiff;

    return std::sqrt(latencyVariance);
}

void ParallelPeer::dataMessageReceived(const std::vector<char>& message)
{
    size_t offset = 0;

    // The type of data message received
    const uint32_t type = *(reinterpret_cast<const uint32_t*>(message.data() + offset));
    offset += sizeof(uint32_t);

    const double timestamp = *(reinterpret_cast<const double*>(message.data() + offset));
    offset += sizeof(double);

    analyzeTimeDifference(timestamp);

    std::vector<char> buffer(message.begin() + offset, message.end());

    switch (static_cast<datamessagestructures::Type>(type)) {
        case datamessagestructures::Type::CameraData: {
            datamessagestructures::CameraKeyframe kf(buffer);
            const double convertedTimestamp = convertTimestamp(kf._timestamp);

            global::navigationHandler.keyframeNavigator().removeKeyframesAfter(
                convertedTimestamp
            );

            interaction::KeyframeNavigator::CameraPose pose;
            pose.focusNode = kf._focusNode;
            pose.position = kf._position;
            pose.rotation = kf._rotation;
            pose.scale = kf._scale;
            pose.followFocusNodeRotation = kf._followNodeRotation;

            global::navigationHandler.keyframeNavigator().addKeyframe(convertedTimestamp,
                                                                      pose);
            break;
        }
        case datamessagestructures::Type::TimelineData: {
            const double now = global::windowDelegate.applicationTime();
            datamessagestructures::TimeTimeline timelineMessage(buffer);

            if (timelineMessage._clear) {
                global::timeManager.removeKeyframesAfter(
                    convertTimestamp(timestamp),
                    true
                );
            }

            const std::vector<datamessagestructures::TimeKeyframe>& keyframesMessage =
                timelineMessage._keyframes;

            // If there are new keyframes incoming, make sure to erase all keyframes
            // that already exist after the first new keyframe.
            if (keyframesMessage.size() > 0) {
                const double convertedTimestamp =
                    convertTimestamp(keyframesMessage[0]._timestamp);

                global::timeManager.removeKeyframesAfter(convertedTimestamp, true);
            }

            for (const datamessagestructures::TimeKeyframe& kfMessage : keyframesMessage)
            {
                TimeKeyframeData timeKeyframeData;
                timeKeyframeData.delta = kfMessage._dt;
                timeKeyframeData.pause = kfMessage._paused;
                timeKeyframeData.time = kfMessage._time;
                timeKeyframeData.jump = kfMessage._requiresTimeJump;

                const double kfTimestamp = convertTimestamp(kfMessage._timestamp);

                // We only need at least one keyframe before the current timestamp,
                // so we can remove any other previous ones
                if (kfTimestamp < now) {
                    global::timeManager.removeKeyframesBefore(kfTimestamp, true);
                }
                global::timeManager.addKeyframe(
                    kfTimestamp,
                    timeKeyframeData
                );
            }
            break;
        }
        case datamessagestructures::Type::ScriptData: {
            datamessagestructures::ScriptMessage sm;
            sm.deserialize(buffer);

            global::scriptEngine.queueScript(
                sm._script,
                scripting::ScriptEngine::RemoteScripting::No
            );
            break;
        }
        default: {
            LERROR(fmt::format(
                "Unidentified message with identifier {} received in parallel connection",
                type
            ));
            break;
        }
    }
}

void ParallelPeer::connectionStatusMessageReceived(const std::vector<char>& message)
 {
    if (message.size() < 2 * sizeof(uint32_t)) {
        LERROR("Malformed connection status message.");
        return;
    }
    size_t pointer = 0;
    uint32_t statusIn = *(reinterpret_cast<const uint32_t*>(&message[pointer]));
    const ParallelConnection::Status status = static_cast<ParallelConnection::Status>(
        statusIn
    );
    pointer += sizeof(uint32_t);

    const size_t hostNameSize = *(reinterpret_cast<const uint32_t*>(&message[pointer]));
    pointer += sizeof(uint32_t);

    if (hostNameSize > message.size() - pointer) {
        LERROR("Malformed connection status message.");
        return;
    }

    std::string hostName;
    if (hostNameSize > 0) {
        hostName = std::string(&message[pointer], hostNameSize);
    }
    pointer += hostNameSize;

    if (status > ParallelConnection::Status::Host) {
        LERROR("Invalid status.");
        return;
    }

    _latencyMutex.lock();
    _latencyDiffs.clear();
    _latencyMutex.unlock();
    setHostName(hostName);

    if (status == _status) {
        // Status remains unchanged.
        return;
    }

    setStatus(status);

    global::navigationHandler.keyframeNavigator().clearKeyframes();
    global::timeManager.clearKeyframes();
}

void ParallelPeer::nConnectionsMessageReceived(const std::vector<char>& message)
{
    if (message.size() < sizeof(uint32_t)) {
        LERROR("Malformed host info message.");
        return;
    }
    const uint32_t nConnections = *(reinterpret_cast<const uint32_t*>(&message[0]));
    setNConnections(nConnections);
}

void ParallelPeer::handleCommunication() {
    while (!_shouldDisconnect && _connection.isConnectedOrConnecting()) {
        try {
            ParallelConnection::Message m = _connection.receiveMessage();
            queueInMessage(m);
        } catch (const ParallelConnection::ConnectionLostError&) {
            LERROR("Parallel connection lost");
        }
    }
    setStatus(ParallelConnection::Status::Disconnected);
}

void ParallelPeer::setPort(std::string port) {
    _port = std::move(port);
}

void ParallelPeer::setAddress(std::string address) {
    _address = std::move(address);
}

void ParallelPeer::setName(std::string name) {
    _name = std::move(name);
}

void ParallelPeer::requestHostship() {
    std::vector<char> buffer;
    uint64_t passwordHash = std::hash<std::string>{}(_hostPassword);
    buffer.insert(
        buffer.end(),
        reinterpret_cast<char*>(&passwordHash),
        reinterpret_cast<char*>(&passwordHash) + sizeof(uint64_t)
    );

    const double now = global::windowDelegate.applicationTime();
    _connection.sendMessage(ParallelConnection::Message(
        ParallelConnection::MessageType::HostshipRequest,
        buffer
    ));
}

void ParallelPeer::resignHostship() {
    const double now = global::windowDelegate.applicationTime();

    std::vector<char> buffer;
    _connection.sendMessage(ParallelConnection::Message(
        ParallelConnection::MessageType::HostshipResignation,
        buffer
    ));
}

void ParallelPeer::setPassword(std::string password) {
    _password = std::move(password);
}

void ParallelPeer::setHostPassword(std::string hostPassword) {
    _hostPassword = std::move(hostPassword);
}

void ParallelPeer::sendScript(std::string script) {
    if (!isHost()) {
        return;
    }

    datamessagestructures::ScriptMessage sm;
    sm._script = std::move(script);

    std::vector<char> buffer;
    sm.serialize(buffer);

    double timestamp = global::windowDelegate.applicationTime();
    ParallelConnection::DataMessage message(
        datamessagestructures::Type::ScriptData,
        timestamp,
        buffer
    );
    _connection.sendDataMessage(message);
}

void ParallelPeer::resetTimeOffset() {
    global::navigationHandler.keyframeNavigator().clearKeyframes();
    global::timeManager.clearKeyframes();
    std::lock_guard<std::mutex> latencyLock(_latencyMutex);
    _latencyDiffs.clear();
}

void ParallelPeer::preSynchronization() {
    std::unique_lock<std::mutex> unqlock(_receiveBufferMutex);
    while (!_receiveBuffer.empty()) {
        ParallelConnection::Message& message = _receiveBuffer.front();
        handleMessage(message);
        _receiveBuffer.pop_front();
    }

    if (isHost()) {
        double now = global::windowDelegate.applicationTime();

        if (_lastCameraKeyframeTimestamp + _cameraKeyframeInterval < now) {
            sendCameraKeyframe();
            _lastCameraKeyframeTimestamp = now;
        }
        if (_timeTimelineChanged ||
            _lastTimeKeyframeTimestamp + _timeKeyframeInterval < now)
        {
            sendTimeTimeline();
            _lastTimeKeyframeTimestamp = now;
            _timeJumped = false;
            _timeTimelineChanged = false;
        }
    }
    if (_shouldDisconnect) {
        disconnect();
    }
}

void ParallelPeer::setStatus(ParallelConnection::Status status) {
    if (_status != status) {
        _status = status;
        _timeJumped = true;
        _connectionEvent->publish("statusChanged");
    }
    if (isHost()) {
        global::timeManager.addTimeJumpCallback([this]() {
            _timeJumped = true;
        });
        global::timeManager.addTimelineChangeCallback([this]() {
            _timeTimelineChanged = true;
        });
    } else {
        if (_timeJumpCallback != -1) {
            global::timeManager.removeTimeJumpCallback(_timeJumpCallback);
        }
        if (_timeTimelineChangeCallback != -1) {
            global::timeManager.removeTimelineChangeCallback(_timeTimelineChangeCallback);
        }
    }
}

ParallelConnection::Status ParallelPeer::status() {
    return _status;
}

void ParallelPeer::setNConnections(size_t nConnections) {
    if (_nConnections != nConnections) {
        _nConnections = nConnections;
        _connectionEvent->publish("nConnectionsChanged");
    }
}

int ParallelPeer::nConnections() {
    return static_cast<int>(_nConnections);
}

bool ParallelPeer::isHost() {
    return _status == ParallelConnection::Status::Host;
}

void ParallelPeer::setHostName(const std::string& hostName) {
    if (_hostName != hostName) {
        _hostName = hostName;
        _connectionEvent->publish("hostNameChanged");
    }
}

const std::string& ParallelPeer::hostName() {
    return _hostName;
}

void ParallelPeer::sendCameraKeyframe() {
    SceneGraphNode* focusNode = global::navigationHandler.focusNode();
    if (!focusNode) {
        return;
    }

    // Create a keyframe with current position and orientation of camera
    datamessagestructures::CameraKeyframe kf;
    kf._position = global::navigationHandler.focusNodeToCameraVector();

    kf._followNodeRotation =
        global::navigationHandler.orbitalNavigator().followingNodeRotation();
    if (kf._followNodeRotation) {
        kf._position = glm::inverse(focusNode->worldRotationMatrix()) * kf._position;
        kf._rotation = global::navigationHandler.focusNodeToCameraRotation();
    }
    else {
        kf._rotation = global::navigationHandler.camera()->rotationQuaternion();
    }

    kf._focusNode = focusNode->identifier();
    kf._scale = global::navigationHandler.camera()->scaling();

    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = global::windowDelegate.applicationTime();

    // Create a buffer for the keyframe
    std::vector<char> buffer;

    // Fill the keyframe buffer
    kf.serialize(buffer);

    const double timestamp = global::windowDelegate.applicationTime();
    // Send message
    _connection.sendDataMessage(ParallelConnection::DataMessage(
        datamessagestructures::Type::CameraData,
        timestamp,
        buffer
    ));
}

void ParallelPeer::sendTimeTimeline() {
    // Create a keyframe with current position and orientation of camera
    const Timeline<TimeKeyframeData> timeline = global::timeManager.timeline();
    std::deque<Keyframe<TimeKeyframeData>> keyframes = timeline.keyframes();

    datamessagestructures::TimeTimeline timelineMessage;
    timelineMessage._clear = true;
    timelineMessage._keyframes.reserve(timeline.nKeyframes());

    // Case 1: Copy all keyframes from the native timeline
    for (size_t i = 0; i < timeline.nKeyframes(); ++i) {
        const Keyframe<TimeKeyframeData>& kf = keyframes.at(i);

        datamessagestructures::TimeKeyframe kfMessage;
        kfMessage._time = kf.data.time.j2000Seconds();
        kfMessage._dt = kf.data.delta;
        kfMessage._paused = kf.data.pause;
        kfMessage._requiresTimeJump = kf.data.jump;
        kfMessage._timestamp = kf.timestamp;

        timelineMessage._keyframes.push_back(kfMessage);
    }

    // Case 2: Send one keyframe to represent the curernt time.
    // If time jumped this frame, this is represented in the keyframe.
    if (timeline.nKeyframes() == 0) {
        datamessagestructures::TimeKeyframe kfMessage;
        kfMessage._time = global::timeManager.time().j2000Seconds();
        kfMessage._dt = global::timeManager.targetDeltaTime();
        kfMessage._paused = global::timeManager.isPaused();
        kfMessage._timestamp = global::windowDelegate.applicationTime();
        kfMessage._requiresTimeJump = _timeJumped;
        timelineMessage._keyframes.push_back(kfMessage);
    }
    // Create a buffer for the keyframe
    std::vector<char> buffer;

    // Fill the timeline buffer
    timelineMessage.serialize(buffer);

    double timestamp = global::windowDelegate.applicationTime();
    // Send message
    _connection.sendDataMessage(ParallelConnection::DataMessage(
        datamessagestructures::Type::TimelineData,
        timestamp,
        buffer
    ));
}

ghoul::Event<>& ParallelPeer::connectionEvent() {
    return *_connectionEvent;
}

scripting::LuaLibrary ParallelPeer::luaLibrary() {
    return {
        "parallel",
        {
            {
                "connect",
                &luascriptfunctions::connect,
                {},
                "",
                "Connect to parallel"
            },
            {
                "disconnect",
                &luascriptfunctions::disconnect,
                {},
                "",
                "Disconnect from parallel"
            },
            {
                "requestHostship",
                &luascriptfunctions::requestHostship,
                {},
                "",
                "Request to be the host for this session"
            },
            {
                "resignHostship",
                &luascriptfunctions::resignHostship,
                {},
                "",
                "Resign hostship"
            },
        }
    };
}

} // namespace openspace
