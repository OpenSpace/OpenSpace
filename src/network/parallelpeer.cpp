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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
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

    const openspace::properties::Property::PropertyInfo PasswordInfo = {
        "Password",
        "Password",
        "The general password that allows this OpenSpace instance access to the Wormhole "
        "server."
    };

    const openspace::properties::Property::PropertyInfo HostPasswordInfo = {
        "HostPassword",
        "Host Password",
        "The password that is required to take control of the joint session and thus "
        "send all commands to connected clients."
    };

    const openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The port on which the Wormhole server is listening to connections from "
        "OpenSpace."
    };

    const openspace::properties::Property::PropertyInfo AddressInfo = {
        "Address",
        "Address",
        "The address of the Wormhole server either as a DNS name or an IP address."
    };

    const openspace::properties::Property::PropertyInfo NameInfo = {
        "Name",
        "Connection Name",
        "The name of this OpenSpace instance that will be potentially broadcast to other "
        "connected instances."
    };

    const openspace::properties::Property::PropertyInfo BufferTimeInfo = {
        "BufferTime",
        "Buffer Time",
        "" // @TODO Missing documentation
    };

    const openspace::properties::Property::PropertyInfo TimeKeyFrameInfo = {
        "TimeKeyframeInterval",
        "Time keyframe interval",
        "" // @TODO Missing documentation
    };

    const openspace::properties::Property::PropertyInfo CameraKeyFrameInfo = {
        "CameraKeyframeInterval",
        "Camera Keyframe interval",
        "" // @TODO Missing documentation
    };

    const openspace::properties::Property::PropertyInfo TimeToleranceInfo = {
        "TimeTolerance",
        "Time tolerance",
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
    , _timeTolerance(TimeToleranceInfo, 1.f, 0.5f, 5.f)
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
    addProperty(_timeTolerance);
}

ParallelPeer::~ParallelPeer() {
    disconnect();
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
    // Length of this nodes name
    const uint32_t nameLength = static_cast<uint32_t>(_name.value().length());

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
    buffer.insert(buffer.end(), _name.value().begin(), _name.value().end());

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

void ParallelPeer::handleMessage(const  ParallelConnection::Message& message) {
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

double ParallelPeer::calculateBufferedKeyframeTime(double originalTime) {
    std::lock_guard<std::mutex> latencyLock(_latencyMutex);

    const double timeDiff = OsEng.windowWrapper().applicationTime() - originalTime;
    if (_latencyDiffs.empty()) {
        _initialTimeDiff = timeDiff;
    }
    const double latencyDiff = timeDiff - _initialTimeDiff;
    if (_latencyDiffs.size() >= MaxLatencyDiffs) {
        _latencyDiffs.pop_front();
    }
    _latencyDiffs.push_back(latencyDiff);

    return originalTime + timeDiff + latencyDiff + _bufferTime;
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

double ParallelPeer::timeTolerance() const {
    return _timeTolerance;
}

void ParallelPeer::dataMessageReceived(const std::vector<char>& message) {
    // The type of data message received
    const uint32_t type = *(reinterpret_cast<const uint32_t*>(message.data()));
    std::vector<char> buffer(message.begin() + sizeof(uint32_t), message.end());

    switch (static_cast<datamessagestructures::Type>(type)) {
        case datamessagestructures::Type::CameraData: {
            datamessagestructures::CameraKeyframe kf(buffer);
            kf._timestamp = calculateBufferedKeyframeTime(kf._timestamp);

            OsEng.navigationHandler().keyframeNavigator().removeKeyframesAfter(
                kf._timestamp
            );
            interaction::KeyframeNavigator::CameraPose pose;
            pose.focusNode = kf._focusNode;
            pose.position = kf._position;
            pose.rotation = kf._rotation;
            pose.followFocusNodeRotation = kf._followNodeRotation;

            OsEng.navigationHandler().keyframeNavigator().addKeyframe(
                kf._timestamp,
                pose
            );
            break;
        }
        case datamessagestructures::Type::TimeData: {
            datamessagestructures::TimeKeyframe kf(buffer);
            kf._timestamp = calculateBufferedKeyframeTime(kf._timestamp);

            OsEng.timeManager().removeKeyframesAfter(kf._timestamp);
            Time time(kf._time);
            time.setDeltaTime(kf._dt);
            time.setPause(kf._paused);
            time.setTimeJumped(kf._requiresTimeJump);

            OsEng.timeManager().addKeyframe(kf._timestamp, time);
            break;
        }
        case datamessagestructures::Type::ScriptData: {
            datamessagestructures::ScriptMessage sm;
            sm.deserialize(buffer);

            OsEng.scriptEngine().queueScript(
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

void ParallelPeer::connectionStatusMessageReceived(const std::vector<char>& message) {
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

    OsEng.navigationHandler().keyframeNavigator().clearKeyframes();
    OsEng.timeManager().clearKeyframes();
}

void ParallelPeer::nConnectionsMessageReceived(const std::vector<char>& message) {
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
    _connection.sendMessage(ParallelConnection::Message(
        ParallelConnection::MessageType::HostshipRequest,
        buffer
    ));
}

void ParallelPeer::resignHostship() {
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

    ParallelConnection::DataMessage message(
        datamessagestructures::Type::ScriptData,
        buffer
    );
    _connection.sendDataMessage(message);
}

void ParallelPeer::resetTimeOffset() {
    OsEng.navigationHandler().keyframeNavigator().clearKeyframes();
    OsEng.timeManager().clearKeyframes();
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

    if (status() == ParallelConnection::Status::Host) {
        if (OsEng.timeManager().time().timeJumped()) {
            _timeJumped = true;
        }
        double now = OsEng.windowWrapper().applicationTime();

        if (_lastCameraKeyframeTimestamp + _cameraKeyframeInterval < now) {
            sendCameraKeyframe();
            _lastCameraKeyframeTimestamp = now;
        }
        if (_lastTimeKeyframeTimestamp + _timeKeyframeInterval < now) {
            sendTimeKeyframe();
            _lastTimeKeyframeTimestamp = now;
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
    SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
    if (!focusNode) {
        return;
    }

    // Create a keyframe with current position and orientation of camera
    datamessagestructures::CameraKeyframe kf;
    kf._position = OsEng.navigationHandler().focusNodeToCameraVector();

    kf._followNodeRotation =
        OsEng.navigationHandler().orbitalNavigator().followingNodeRotation();
    if (kf._followNodeRotation) {
        kf._position = glm::inverse(focusNode->worldRotationMatrix()) * kf._position;
        kf._rotation = OsEng.navigationHandler().focusNodeToCameraRotation();
    }
    else {
        kf._rotation = OsEng.navigationHandler().camera()->rotationQuaternion();
    }

    kf._focusNode = focusNode->identifier();
    kf._scale = OsEng.navigationHandler().camera()->scaling();

    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.windowWrapper().applicationTime();

    // Create a buffer for the keyframe
    std::vector<char> buffer;

    // Fill the keyframe buffer
    kf.serialize(buffer);

    // Send message
    _connection.sendDataMessage(ParallelConnection::DataMessage(
        datamessagestructures::Type::CameraData,
        buffer
    ));
}

void ParallelPeer::sendTimeKeyframe() {
    // Create a keyframe with current position and orientation of camera
    datamessagestructures::TimeKeyframe kf;

    const Time& time = OsEng.timeManager().time();

    kf._dt = time.deltaTime();
    kf._paused = time.paused();
    kf._requiresTimeJump = _timeJumped;
    kf._time = time.j2000Seconds();

    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.windowWrapper().applicationTime();

    // Create a buffer for the keyframe
    std::vector<char> buffer;

    // Fill the keyframe buffer
    kf.serialize(buffer);

    // Send message
    _connection.sendDataMessage(ParallelConnection::DataMessage(
        datamessagestructures::Type::TimeData,
        buffer
    ));
    _timeJumped = false;
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
