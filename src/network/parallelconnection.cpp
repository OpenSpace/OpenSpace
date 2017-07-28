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

#include <openspace/network/parallelconnection.h>

#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/script_helper.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>

#include <ghoul/logging/logmanager.h>

#include "parallelconnection_lua.inl"

namespace {
    const uint32_t ProtocolVersion = 3;
    const size_t MaxLatencyDiffs = 64;
    const char* _loggerCat = "ParallelConnection";

    static const openspace::properties::Property::PropertyInfo PasswordInfo = {
        "Password",
        "Password",
        "The general password that allows this OpenSpace instance access to the Wormhole "
        "server."
    };

    static const openspace::properties::Property::PropertyInfo HostPasswordInfo = {
        "HostPassword",
        "Host Password",
        "The password that is required to take control of the joint session and thus "
        "send all commands to connected clients."
    };

    static const openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The port on which the Wormhole server is listening to connections from "
        "OpenSpace."
    };

    static const openspace::properties::Property::PropertyInfo AddressInfo = {
        "Address",
        "Address",
        "The address of the Wormhole server either as a DNS name or an IP address."
    };

    static const openspace::properties::Property::PropertyInfo NameInfo = {
        "Name",
        "Connection Name",
        "The name of this OpenSpace instance that will be potentially broadcast to other "
        "connected instances."
    };

    static const openspace::properties::Property::PropertyInfo BufferTimeInfo = {
        "BufferTime",
        "Buffer Time",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TimeKeyFrameInfo = {
        "TimeKeyframeInterval",
        "Time keyframe interval",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo CameraKeyFrameInfo = {
        "CameraKeyframeInterval",
        "Camera Keyframe interval",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TimeToleranceInfo = {
        "TimeTolerance",
        "Time tolerance",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

ParallelConnection::ParallelConnection()
    : properties::PropertyOwner("ParallelConnection")
    , _password(PasswordInfo)
    , _hostPassword(HostPasswordInfo)
    , _port(PortInfo, "20501")
    , _address(AddressInfo, "localhost")
    , _name(NameInfo, "Anonymous")
    , _bufferTime(BufferTimeInfo, 1, 0.5, 10)
    , _timeKeyframeInterval(TimeKeyFrameInfo, 0.1f, 0.f, 1.f)
    , _cameraKeyframeInterval(CameraKeyFrameInfo, 0.1f, 0.f, 1.f)
    , _timeTolerance(TimeToleranceInfo, 1.f, 0.5f, 5.f)
    , _lastTimeKeyframeTimestamp(0)
    , _lastCameraKeyframeTimestamp(0)
    , _nConnections(0)
    , _status(Status::Disconnected)
    , _hostName("")
    , _receiveThread(nullptr)
    , _socket(nullptr)
    , _shouldDisconnect(false)
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

    _connectionEvent = std::make_shared<ghoul::Event<>>();
}
        
ParallelConnection::~ParallelConnection(){
    disconnect();
}

void ParallelConnection::connect() {
    disconnect();

    setStatus(Status::Connecting);
    std::string port = _port;
    _socket = std::make_unique<ghoul::io::TcpSocket>(_address, atoi(port.c_str()));
    _socket->connect();
    sendAuthentication();

    _receiveThread = std::make_unique<std::thread>(
        [this]() { handleCommunication(); }
    );
}

void ParallelConnection::disconnect(){
    if (_socket) {
        _socket->disconnect();
    }
    if (_receiveThread && _receiveThread->joinable()) {
        _receiveThread->join();
        _receiveThread = nullptr;
    }
    _shouldDisconnect = false;
    _socket = nullptr;
    setStatus(Status::Disconnected);
}

void ParallelConnection::sendAuthentication() {
    std::string name = _name.value();
    // Length of this nodes name
    uint32_t nameLength = static_cast<uint32_t>(name.length());
            
    // Total size of the buffer: (passcode + namelength + name)
    size_t size = 2 * sizeof(uint32_t) + nameLength;

    // Create and reserve buffer
    std::vector<char> buffer;
    buffer.reserve(size);

    uint32_t passCode = hash(_password.value());

    // Write passcode to buffer
    buffer.insert(
        buffer.end(),
        reinterpret_cast<char*>(&passCode),
        reinterpret_cast<char*>(&passCode) + sizeof(uint32_t)
    );

    // Write the length of the nodes name to buffer
    buffer.insert(
        buffer.end(),
        reinterpret_cast<char*>(&nameLength),
        reinterpret_cast<char*>(&nameLength) + sizeof(uint32_t)
    );

    // Write this node's name to buffer
    buffer.insert(buffer.end(), name.begin(), name.end());
            
    // Send message
    sendMessage(Message(MessageType::Authentication, buffer));
}

void ParallelConnection::queueInMessage(const Message& message) {
    std::unique_lock<std::mutex> unqlock(_receiveBufferMutex);
    _receiveBuffer.push_back(message);
}

void ParallelConnection::handleMessage(const Message& message) {
    switch (message.type){
        case MessageType::Data:
            dataMessageReceived(message.content);
            break;
        case MessageType::ConnectionStatus:
            connectionStatusMessageReceived(message.content);
            break;
        case MessageType::NConnections:
            nConnectionsMessageReceived(message.content);
            break;
        default:
            //unknown message type
            break;
    }
}

double ParallelConnection::calculateBufferedKeyframeTime(double originalTime) {
    std::lock_guard<std::mutex> latencyLock(_latencyMutex);

    double timeDiff = OsEng.runTime() - originalTime;
    if (_latencyDiffs.size() == 0) {
        _initialTimeDiff = timeDiff;
    }
    double latencyDiff = timeDiff - _initialTimeDiff;
    if (_latencyDiffs.size() >= MaxLatencyDiffs) {
        _latencyDiffs.pop_front();
    }
    _latencyDiffs.push_back(latencyDiff);

    return originalTime + timeDiff + latencyDiff + _bufferTime;
}

double ParallelConnection::latencyStandardDeviation() const {
    double accumulatedLatencyDiffSquared = 0;
    double accumulatedLatencyDiff = 0;
    for (double diff : _latencyDiffs) {
        accumulatedLatencyDiff += diff;
        accumulatedLatencyDiffSquared += diff*diff;
    }
    double expectedLatencyDiffSquared = accumulatedLatencyDiffSquared / _latencyDiffs.size();
    double expectedLatencyDiff = accumulatedLatencyDiff / _latencyDiffs.size();

    // V(X) = E(x^2) - E(x)^2
    double latencyVariance = expectedLatencyDiffSquared - expectedLatencyDiff*expectedLatencyDiff;
    return std::sqrt(latencyVariance);
}

double ParallelConnection::timeTolerance() const {
    return _timeTolerance;
}

void ParallelConnection::dataMessageReceived(const std::vector<char>& messageContent) {
    // The type of data message received
    uint32_t type = *(reinterpret_cast<const uint32_t*>(messageContent.data()));   
    std::vector<char> buffer(
        messageContent.begin() + sizeof(uint32_t),
        messageContent.end()
    );
 
    switch (static_cast<datamessagestructures::Type>(type)) {
        case datamessagestructures::Type::CameraData: {
            datamessagestructures::CameraKeyframe kf(buffer);
            kf._timestamp = calculateBufferedKeyframeTime(kf._timestamp);

            OsEng.navigationHandler().keyframeNavigator().removeKeyframesAfter(
                kf._timestamp);
            interaction::KeyframeNavigator::CameraPose pose;
            pose.focusNode = kf._focusNode;
            pose.position = kf._position;
            pose.rotation = kf._rotation;
            pose.followFocusNodeRotation = kf._followNodeRotation;

            OsEng.navigationHandler().keyframeNavigator().addKeyframe(
                kf._timestamp, pose);
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
            LERROR("Unidentified data message with identifier " << type <<
                   " received in parallel connection."
            );
            break;
        }
    }
}

void ParallelConnection::sendDataMessage(const DataMessage& dataMessage) {
    uint32_t dataMessageTypeOut = static_cast<uint32_t>(dataMessage.type);

    std::vector<char> messageContent;
    messageContent.insert(messageContent.end(),
        reinterpret_cast<const char*>(&dataMessageTypeOut),
        reinterpret_cast<const char*>(&dataMessageTypeOut) + sizeof(uint32_t));

    messageContent.insert(messageContent.end(),
        dataMessage.content.begin(),
        dataMessage.content.end());

    sendMessage(Message(MessageType::Data, messageContent));
}

void ParallelConnection::sendMessage(const Message& message) {
    uint32_t messageTypeOut = static_cast<uint32_t>(message.type);
    uint32_t messageSizeOut = static_cast<uint32_t>(message.content.size());

    std::vector<char> messageContent;
    messageContent.insert(messageContent.end(),
        reinterpret_cast<const char*>(&messageTypeOut),
        reinterpret_cast<const char*>(&messageTypeOut) + sizeof(uint32_t));

    messageContent.insert(messageContent.end(),
        message.content.begin(),
        message.content.end());

    std::vector<char> header;

    //insert header into buffer
    header.push_back('O');
    header.push_back('S');


    header.insert(header.end(),
        reinterpret_cast<const char*>(&ProtocolVersion),
        reinterpret_cast<const char*>(&ProtocolVersion) + sizeof(uint32_t)
        );

    header.insert(header.end(),
        reinterpret_cast<const char*>(&messageTypeOut),
        reinterpret_cast<const char*>(&messageTypeOut) + sizeof(uint32_t)
        );

    header.insert(header.end(),
        reinterpret_cast<const char*>(&messageSizeOut),
        reinterpret_cast<const char*>(&messageSizeOut) + sizeof(uint32_t)
        );

//            if (result == SOCKET_ERROR) {
//                LERROR(
//                    "Failed to send message.\nError: " <<
//                    _ERRNO << " detected in connection, disconnecting."
//                );
//                signalDisconnect();
//            }

    if (!_socket->put<char>(header.data(), header.size())) {
        LERROR("Failed to send message.\nError: " <<
            _ERRNO << " detected in connection, disconnecting."
            );
        disconnect();
        return;
    }
    if (!_socket->put<char>(message.content.data(), message.content.size())) {
        LERROR("Failed to send message.\nError: " <<
            _ERRNO << " detected in connection, disconnecting."
            );
        disconnect();
        return;
    }
}
        
void ParallelConnection::connectionStatusMessageReceived(const std::vector<char>& message)
{
    if (message.size() < 2 * sizeof(uint32_t)) {
        LERROR("Malformed connection status message.");
        return;
    }
    size_t pointer = 0;
    uint32_t statusIn = *(reinterpret_cast<const uint32_t*>(&message[pointer]));
    ParallelConnection::Status status = static_cast<ParallelConnection::Status>(statusIn);
    pointer += sizeof(uint32_t);

    size_t hostNameSize = *(reinterpret_cast<const uint32_t*>(&message[pointer]));
    pointer += sizeof(uint32_t);

    if (hostNameSize > message.size() - pointer) {
        LERROR("Malformed connection status message.");
        return;
    }

    std::string hostName = "";
    if (hostNameSize > 0) {
        hostName = std::string(&message[pointer], hostNameSize);
    }
    pointer += hostNameSize;
    
    if (status > Status::Host) {
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

void ParallelConnection::nConnectionsMessageReceived(const std::vector<char>& message) {
    if (message.size() < sizeof(uint32_t)) {
        LERROR("Malformed host info message.");
        return;
    }
    uint32_t nConnections = *(reinterpret_cast<const uint32_t*>(&message[0]));
    setNConnections(nConnections);
}

void ParallelConnection::handleCommunication() {
    // Header consists of 'OS' + majorVersion + minorVersion + messageSize
    constexpr size_t headerSize = 2 * sizeof(char) + 3 * sizeof(uint32_t);

    // Create basic buffer for receiving first part of messages
    std::vector<char> headerBuffer(headerSize);
    std::vector<char> messageBuffer;
           
    while (!_shouldDisconnect && _socket && _socket->isConnected()) {
        // Receive the header data
        if (!_socket->get(headerBuffer.data(), headerSize)) {
            _shouldDisconnect = true;
            break;
        }

        // Make sure that header matches this version of OpenSpace
        if (!(headerBuffer[0] == 'O' && headerBuffer[1] && 'S')) {
            LERROR("Expected to read message header 'OS' from socket.");
            _shouldDisconnect = true;
            break;
        }

        uint32_t* ptr = reinterpret_cast<uint32_t*>(&headerBuffer[2]);

        uint32_t protocolVersionIn = *(ptr++);
        uint32_t messageTypeIn = *(ptr++);
        uint32_t messageSizeIn = *(ptr++);

        if (protocolVersionIn != ProtocolVersion) {
            LERROR("Protocol versions do not match. Server version: " << protocolVersionIn << ", Client version: " << ProtocolVersion);
            _shouldDisconnect = true;
            break;
        }

        size_t messageSize = messageSizeIn;

        // Receive the payload
        messageBuffer.resize(messageSize);
        if (!_socket->get(messageBuffer.data(), messageSize)) {
            _shouldDisconnect = true;
            break;
        }

        // And delegate decoding depending on type
        queueInMessage(Message(static_cast<MessageType>(messageTypeIn), messageBuffer));
    }
}
        
void ParallelConnection::setPort(std::string port){
    _port = std::move(port);
}

void ParallelConnection::setAddress(std::string address){
    _address = std::move(address);
}
        
void ParallelConnection::setName(std::string name){
    _name = std::move(name);
}

void ParallelConnection::requestHostship(){
    std::vector<char> buffer;
    uint32_t passcode = hash(_hostPassword);
    buffer.insert(
        buffer.end(),
        reinterpret_cast<char*>(&passcode),
        reinterpret_cast<char*>(&passcode) + sizeof(uint32_t)
    );
    sendMessage(Message(MessageType::HostshipRequest, buffer));
}

void ParallelConnection::resignHostship() {
    std::vector<char> buffer;
    sendMessage(Message(MessageType::HostshipResignation, buffer));
}

void ParallelConnection::setPassword(std::string pwd) {
    _password = std::move(pwd);
}

void ParallelConnection::setHostPassword(std::string pwd) {
    _hostPassword = std::move(pwd);
}

void ParallelConnection::sendScript(std::string script) {
    if (!isHost()) {
        return;
    }

    datamessagestructures::ScriptMessage sm;
    sm._script = std::move(script);
    
    std::vector<char> buffer;
    sm.serialize(buffer);
    
    DataMessage message(datamessagestructures::Type::ScriptData, buffer);
    sendDataMessage(message);
}

void ParallelConnection::resetTimeOffset() {
    OsEng.navigationHandler().keyframeNavigator().clearKeyframes();
    OsEng.timeManager().clearKeyframes();
    std::lock_guard<std::mutex> latencyLock(_latencyMutex);
    _latencyDiffs.clear();
}

void ParallelConnection::preSynchronization() {
    std::unique_lock<std::mutex> unqlock(_receiveBufferMutex);
    while (!_receiveBuffer.empty()) {
        Message& message = _receiveBuffer.front();
        handleMessage(message);
        _receiveBuffer.pop_front();
    }
    
    if (status() == Status::Host) {
        if (OsEng.timeManager().time().timeJumped()) {
            _timeJumped = true;
        }
        double now = OsEng.runTime();

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
         
void ParallelConnection::setStatus(Status status) {
    if (_status != status) {
        _status = status;
        _timeJumped = true;
        _connectionEvent->publish("statusChanged");
    }
}

ParallelConnection::Status ParallelConnection::status() {
    return _status;
}

void ParallelConnection::setNConnections(size_t nConnections) {
    if (_nConnections != nConnections) {
        _nConnections = nConnections;
        _connectionEvent->publish("nConnectionsChanged");
    }
}

int ParallelConnection::nConnections() {
    return static_cast<int>(_nConnections);
}

bool ParallelConnection::isHost() {
    return _status == Status::Host;
}

void ParallelConnection::setHostName(const std::string& hostName) {
    if (_hostName != hostName) {
        _hostName = hostName;
        _connectionEvent->publish("hostNameChanged");
    }
}

const std::string& ParallelConnection::hostName() {
    return _hostName;
}

void ParallelConnection::sendCameraKeyframe() {
    SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
    if (!focusNode) {
        return;
    }

    // Create a keyframe with current position and orientation of camera
    datamessagestructures::CameraKeyframe kf;
    kf._position = OsEng.navigationHandler().focusNodeToCameraVector();

    kf._followNodeRotation = OsEng.navigationHandler().orbitalNavigator().followingNodeRotation();
    if (kf._followNodeRotation) {
        kf._position = glm::inverse(focusNode->worldRotationMatrix()) * kf._position;
        kf._rotation = OsEng.navigationHandler().focusNodeToCameraRotation();
    } else {
        kf._rotation = OsEng.navigationHandler().camera()->rotationQuaternion();
    }

    kf._focusNode = focusNode->name();

    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.runTime();

    // Create a buffer for the keyframe
    std::vector<char> buffer;

    // Fill the keyframe buffer
    kf.serialize(buffer);

    // Send message
    sendDataMessage(DataMessage(datamessagestructures::Type::CameraData, buffer));
}

void ParallelConnection::sendTimeKeyframe() {
    // Create a keyframe with current position and orientation of camera
    datamessagestructures::TimeKeyframe kf;
    
    Time& time = OsEng.timeManager().time();

    kf._dt = time.deltaTime();
    kf._paused = time.paused();
    kf._requiresTimeJump = _timeJumped;
    kf._time = time.j2000Seconds();

    // Timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.runTime();

    // Create a buffer for the keyframe
    std::vector<char> buffer;

    // Fill the keyframe buffer
    kf.serialize(buffer);

    // Send message
    sendDataMessage(DataMessage(datamessagestructures::Type::TimeData, buffer));
    _timeJumped = false;
}

uint32_t ParallelConnection::hash(const std::string& val) {
    uint32_t hashVal = 0, i;
    size_t len = val.length();

    for (hashVal = i = 0; i < len; ++i) {
        hashVal += val.c_str()[i];
        hashVal += (hashVal << 10);
        hashVal ^= (hashVal >> 6);
    }

    hashVal += (hashVal << 3);
    hashVal ^= (hashVal >> 11);
    hashVal += (hashVal << 15);

    return hashVal;
};


std::shared_ptr<ghoul::Event<>> ParallelConnection::connectionEvent() {
    return _connectionEvent;
}
        
scripting::LuaLibrary ParallelConnection::luaLibrary() {
    return {
        "parallel",
        {
            {
                "connect",
                &luascriptfunctions::connect,
                "",
                "Connect to parallel"
            },
            {
                "disconnect",
                &luascriptfunctions::disconnect,
                "",
                "Disconnect from parallel"
            },
            {
                "requestHostship",
                &luascriptfunctions::requestHostship,
                "",
                "Request to be the host for this session"
            },
            {
                "resignHostship",
                &luascriptfunctions::resignHostship,
                "",
                "Resign hostship"
            },
        }
    };
}

} // namespace openspace
