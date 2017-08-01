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

#ifdef WIN32
#ifndef _ERRNO
#define _ERRNO WSAGetLastError()
#endif
#else //Use BSD sockets
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#ifndef SOCKET_ERROR
#define SOCKET_ERROR (-1)
#endif

#ifndef INVALID_SOCKET
#define INVALID_SOCKET (_SOCKET)(~0)
#endif

#ifndef NO_ERROR
#define NO_ERROR 0L
#endif

#ifndef _ERRNO
#define _ERRNO errno
#endif
#endif

#ifdef WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#pragma warning (push)
#pragma warning (disable: 4574) // 'INCL_WINSOCK_API_TYPEDEFS' is defined to be '0'

#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>

#pragma warning (pop)
#endif

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
    const uint32_t ProtocolVersion = 2;
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
    : properties::PropertyOwner({ "ParallelConnection" })
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
    , _clientSocket(INVALID_SOCKET)
    , _isConnected(false)
    , _isRunning(true)
    , _tryConnect(false)
    , _disconnect(false)
    , _initializationTimejumpRequired(false)
    , _nConnections(0)
    , _status(Status::Disconnected)
    , _hostName("")
    , _connectionThread(nullptr)
    , _sendThread(nullptr)
    , _listenThread(nullptr)
    , _handlerThread(nullptr)
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
    _handlerThread = std::make_unique<std::thread>(
        &ParallelConnection::threadManagement,
        this
    );
}
        
ParallelConnection::~ParallelConnection() {
    // signal that a disconnect should occur
    signalDisconnect();
            
    // signal that execution has stopped
    _isRunning.store(false);
            
    // join handler
    _handlerThread->join();
}
        
void ParallelConnection::threadManagement() {
    // The _disconnectCondition.wait(unqlock) stalls
    // How about moving this out of the thread and into the destructor? ---abock
    
    // while we're still running
    while(_isRunning){
        // lock disconnect mutex mutex
        // not really needed since no data is modified but conditions need a mutex
        std::unique_lock<std::mutex> disconnectLock(_disconnectMutex);
        // wait for a signal to disconnect
        _disconnectCondition.wait(
            disconnectLock,
            [this]() { return _disconnect.load(); }
        );
                
        // perform actual disconnect
        disconnect();
    }
}
        
void ParallelConnection::signalDisconnect() {
    //signal handler thread to disconnect
    _disconnect = true;
    _sendCondition.notify_all(); // Allow send function to terminate.
    _disconnectCondition.notify_all(); // Unblock thread management thread.
}
        
void ParallelConnection::closeSocket() {
    /*
        Windows shutdown options
        * SD_RECIEVE
        * SD_SEND
        * SD_BOTH
             
        Linux & Mac shutdown options
        * SHUT_RD (Disables further receive operations)
        * SHUT_WR (Disables further send operations)
        * SHUT_RDWR (Disables further send and receive operations)
        */
            
#ifdef WIN32
    shutdown(_clientSocket, SD_BOTH);
    closesocket(_clientSocket);
#else
    shutdown(_clientSocket, SHUT_RDWR);
    close(_clientSocket);
#endif
            
    _clientSocket = INVALID_SOCKET;
}
        
void ParallelConnection::disconnect() {
    // We're disconnecting
    if (_clientSocket != INVALID_SOCKET) {
        // Must be run before trying to join communication threads, else the threads are
        // stuck trying to receive data
        closeSocket();
                
        // Ttell connection thread to stop trying to connect
        _tryConnect = false;
                
        // Tell send thread to stop sending and listen thread to stop listenin
        _isConnected = false;

        setStatus(Status::Disconnected);
               
        // join connection thread and delete it
        if (_connectionThread != nullptr) {
            _connectionThread->join();
            _connectionThread = nullptr;
        }
                
        // join send thread and delete it
        if (_sendThread != nullptr) {
            _sendThread->join();
            _sendThread = nullptr;
        }
                
        // join listen thread and delete it
        if (_listenThread != nullptr) {
            _listenThread->join();
            _listenThread = nullptr;
        }

        // disconnect and cleanup completed
        _disconnect = false;
    }
}
        
void ParallelConnection::clientConnect() {
    // We're already connected (or already trying to connect), do nothing (dummy check)
    if (_isConnected.load() || _tryConnect.load()) {
        return;
    }
            
    if (!initNetworkAPI()) {
        LERROR("Failed to initialize network API for Parallel Connection");
        return;
    }
            
    struct addrinfo* addresult = NULL;
    struct addrinfo hints;
    
    memset(&hints, 0, sizeof(hints));
    
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
    hints.ai_flags = AI_PASSIVE;

    // Resolve the local address and port to be used by the server
    int result = getaddrinfo(
        _address.value().c_str(),
        _port.value().c_str(),
        &hints,
        &addresult
    );
    if (result != 0) {
        LERROR("Failed to parse hints for Parallel Connection");
        return;
    }
            
    // We're not connected
    _isConnected = false;
            
    // We want to try and establish a connection
    _tryConnect = true;
            
    // Start connection thread
    _connectionThread = std::make_unique<std::thread>(
        &ParallelConnection::establishConnection,
        this,
        addresult
    );
}

void ParallelConnection::establishConnection(addrinfo *info) {
    _clientSocket = socket(info->ai_family, info->ai_socktype, info->ai_protocol);
            
    if (_clientSocket == INVALID_SOCKET) {
        freeaddrinfo(info);
        LERROR("Failed to create client socket, disconnecting.");

        // Signal a disconnect
        signalDisconnect();
    }
            
    int trueFlag = 1;
    int falseFlag = 0;
    int result;
            
    // Set no delay
    result = setsockopt(
        _clientSocket,                      // socket affected
        IPPROTO_TCP,                        // set option at TCP level
        TCP_NODELAY,                        // name of option
        reinterpret_cast<char*>(&trueFlag), // the cast is historical cruft
        sizeof(int)                         // length of option value
    );
    
    // Set send timeout
    int timeout = 0;
    result = setsockopt(
        _clientSocket,
        SOL_SOCKET,
        SO_SNDTIMEO,
        reinterpret_cast<char*>(&timeout),
        sizeof(timeout)
    );
    
    // Set receive timeout
    result = setsockopt(
        _clientSocket,
        SOL_SOCKET,
        SO_RCVTIMEO,
        reinterpret_cast<char*>(&timeout),
        sizeof(timeout)
    );

    result = setsockopt(
        _clientSocket,
        SOL_SOCKET,
        SO_REUSEADDR,
        reinterpret_cast<char*>(&falseFlag),
        sizeof(int)
    );
    if (result == SOCKET_ERROR) {
        LERROR("Failed to set socket option 'reuse address'. Error code: " << _ERRNO);
    }
    
    result = setsockopt(
        _clientSocket,
        SOL_SOCKET,
        SO_KEEPALIVE,
        reinterpret_cast<char*>(&trueFlag),
        sizeof(int)
    );
    if (result == SOCKET_ERROR) {
        LERROR("Failed to set socket option 'keep alive'. Error code: " << _ERRNO);
    }
    
    LINFO("Attempting to connect to server "<< _address << " on port " << _port);
                
    // Try to connect
    result = connect(_clientSocket, info->ai_addr, static_cast<int>(info->ai_addrlen));
                
    // If the connection was successfull
    if (result != SOCKET_ERROR) {
        LINFO("Connection established with server at ip: "<< _address);
                    
        // We're connected
        _isConnected = true;
                    
        // Start sending messages
        _sendThread = std::make_unique<std::thread>(&ParallelConnection::sendFunc, this);
                    
        // Start listening for communication
        _listenThread = std::make_unique<std::thread>(
            &ParallelConnection::listenCommunication,
            this
        );
                    
        // We no longer need to try to establish connection
        _tryConnect = false;
                    
        _sendBufferMutex.lock();
        _sendBuffer.clear();
        _sendBufferMutex.unlock();

        // Send authentication
        sendAuthentication();
    } else {
        LINFO("Connection attempt failed.");
        signalDisconnect();
    }
            
    // Cleanup
    freeaddrinfo(info);
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
            
    // Send buffer
    queueOutMessage(Message(MessageType::Authentication, buffer));
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

/*
void ParallelConnection::initializationMessageReceived(){
            
    int result;
            
    uint32_t id, datasize;
    uint16_t numscripts;
            
    std::vector<char> buffer;
    buffer.resize(sizeof(id));

    //read id
    result = receiveData(_clientSocket, buffer, sizeof(id), 0);
    if (result < 0){
        //error
    }
    id = *(reinterpret_cast<uint32_t*>(buffer.data()));

    //read datalength
    result = receiveData(_clientSocket, buffer, sizeof(datasize), 0);
    if (result < 0){
        //error
    }
    datasize = *(reinterpret_cast<uint32_t*>(buffer.data()));
            
    buffer.clear();
    buffer.resize(sizeof(uint16_t));
    //read number of scripts
    result = receiveData(_clientSocket, buffer, sizeof(numscripts), 0);
    if(result < 0){
        //error
    }        
    numscripts = *(reinterpret_cast<uint16_t*>(buffer.data()));
            
    //length of current script
    uint16_t scriptlen;
            
    buffer.clear();
    buffer.resize(sizeof(scriptlen));
            
    //holder for current script
    std::string script;
            
    for(int n = 0; n < numscripts; ++n){
        //read length of script
        result = receiveData(_clientSocket, buffer, sizeof(numscripts), 0);
        if(result < 0){
            //error
        }
        scriptlen = *(reinterpret_cast<uint16_t*>(buffer.data()));
                
        //resize buffer
        buffer.clear();
        buffer.resize(scriptlen);
                
        //read script
        result = receiveData(_clientSocket, buffer, scriptlen, 0);

        //assign current script
        script.clear();
        script.assign(buffer.begin(), buffer.end());
                
        //queue received script
        OsEng.scriptEngine().queueScript(script);
    }
            
    //we've gone through all scripts, initialization is done
    buffer.clear();
    writeHeader(buffer, MessageTypes::InitializationCompleted);
            
    //let the server know
    std::cout << "initialization message recieved queue" << std::endl;
    queueMessage(InitializationCompleted, buffer);
            
    //we also need to force a time jump just to ensure that the server and client are synced
    _initializationTimejumpRequired.store(true);
            
}
*/

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

void ParallelConnection::queueOutDataMessage(const DataMessage& dataMessage) {
    uint32_t dataMessageTypeOut = static_cast<uint32_t>(dataMessage.type);

    std::vector<char> messageContent;
    messageContent.insert(messageContent.end(),
        reinterpret_cast<const char*>(&dataMessageTypeOut),
        reinterpret_cast<const char*>(&dataMessageTypeOut) + sizeof(uint32_t));

    messageContent.insert(messageContent.end(),
        dataMessage.content.begin(),
        dataMessage.content.end());

    queueOutMessage(Message(MessageType::Data, messageContent));
}

void ParallelConnection::queueOutMessage(const Message& message) {
    std::unique_lock<std::mutex> unqlock(_sendBufferMutex);
    _sendBuffer.push_back(message);
    _sendCondition.notify_all();
}
        
void ParallelConnection::sendFunc(){
    int result;
    // While we're connected
    while (_isConnected && !_disconnect) {
        // Wait for signal then lock mutex and send first queued message
        std::unique_lock<std::mutex> unqlock(_sendBufferMutex);
        _sendCondition.wait(unqlock);

        if (_disconnect) {
            break;
        }

        while (!_sendBuffer.empty()) {
            Message message = _sendBuffer.front();
            unqlock.unlock();
            std::vector<char> header;

            //insert header into buffer
            header.push_back('O');
            header.push_back('S');

            uint32_t messageTypeOut = static_cast<uint32_t>(message.type);
            uint32_t messageSizeOut = static_cast<uint32_t>(message.content.size());


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

            result = send(
                _clientSocket,
                header.data(),
                static_cast<int>(header.size()),
                0
            );
            result = send(
                _clientSocket,
                message.content.data(),
                static_cast<int>(message.content.size()),
                0
            );

            if (result == SOCKET_ERROR) {
                LERROR(
                    "Failed to send message.\nError: " <<
                    _ERRNO << " detected in connection, disconnecting."
                );
                signalDisconnect();
            }

            unqlock.lock();
            _sendBuffer.erase(_sendBuffer.begin());
        }
    }
    std::lock_guard<std::mutex> sendLock(_sendBufferMutex);
    _sendBuffer.clear();

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




//void ParallelConnection::initializationRequestMessageReceived(const std::vector<char>& message){
            /*
    //get current state as scripts
    std::vector<std::string> scripts;
    std::map<std::string, std::string>::iterator state_it;
    {
        //mutex protect
        std::lock_guard<std::mutex> lock(_currentStateMutex);
                
        for(state_it = _currentState.begin();
            state_it != _currentState.end();
            ++state_it){
            scripts.push_back(scriptFromPropertyAndValue(state_it->first, state_it->second));
        }
    }
            
    //get requester ID
    std::vector<char> buffer;
    buffer.resize(sizeof(uint32_t));
    receiveData(_clientSocket, buffer, sizeof(uint32_t), 0);
    uint32_t requesterID = *reinterpret_cast<uint32_t*>(buffer.data());
            
    //total number of scripts sent
    uint16_t numscripts = 0;
            
    //temporary buffers
    std::vector<char> scriptbuffer;
    std::vector<char> tmpbuffer;
            
    //serialize and encode all scripts into scriptbuffer
    std::vector<std::string>::iterator script_it;
    datamessagestructures::ScriptMessage sm;
    for(script_it = scripts.begin();
        script_it != scripts.end();
        ++script_it){
        sm._script = *script_it;
        sm._scriptlen = script_it->length();

        //serialize current script
        tmpbuffer.clear();
        sm.serialize(tmpbuffer);
                
        //and insert into full buffer
        scriptbuffer.insert(scriptbuffer.end(), tmpbuffer.begin(), tmpbuffer.end());
                
        //increment number of scripts
        numscripts++;
    }
            
    //write header
    buffer.clear();
    writeHeader(buffer, MessageTypes::Initialization);
            
    //write client ID to receive init message
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&requesterID), reinterpret_cast<char*>(&requesterID) + sizeof(uint32_t));
            
    //write total size of data chunk
    uint32_t totlen = static_cast<uint32_t>(scriptbuffer.size());
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&totlen), reinterpret_cast<char*>(&totlen) + sizeof(uint32_t));
            
    //write number of scripts
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&numscripts), reinterpret_cast<char*>(&numscripts) + sizeof(uint16_t));
            
    //write all scripts
    buffer.insert(buffer.end(), scriptbuffer.begin(), scriptbuffer.end());
            
    //queue message
    std::cout << "initializationRequest queue" << std::endl;
    queueMessage(MessageType::Initialization, buffer);
    */
//}

void ParallelConnection::listenCommunication() {
    constexpr size_t headerSize = 2 * sizeof(char) + 3 * sizeof(uint32_t);

    // Create basic buffer for receiving first part of messages
    std::vector<char> headerBuffer(headerSize);
    std::vector<char> messageBuffer;

    int nBytesRead = 0;
            
    // While we're still connected
    while (_isConnected.load()) {
        // Receive the header data
        nBytesRead = receiveData(_clientSocket, headerBuffer, headerSize, 0);

        // If enough data was received
        if (nBytesRead <= 0) {
            if (!_disconnect) {
                LERROR(
                    "Error " << _ERRNO <<
                    " detected in connection when reading header, disconnecting!"
                );
                signalDisconnect();
            }
            break;
        }

        // Make sure that header matches this version of OpenSpace
        if (!(headerBuffer[0] == 'O' && headerBuffer[1] && 'S')) {
            LERROR("Expected to read message header 'OS' from socket.");
            signalDisconnect();
            break;
        }

        uint32_t* ptr = reinterpret_cast<uint32_t*>(&headerBuffer[2]);

        uint32_t protocolVersionIn = *(ptr++);
        uint32_t messageTypeIn = *(ptr++);
        uint32_t messageSizeIn = *(ptr++);

        if (protocolVersionIn != ProtocolVersion) {
            LERROR(
                "Protocol versions do not match. Server version: " <<
                protocolVersionIn << ", Client version: " << ProtocolVersion
            );
            signalDisconnect();
            break;
        }

        size_t messageSize = messageSizeIn;

        // Receive the payload
        messageBuffer.resize(messageSize);
        nBytesRead = receiveData(
            _clientSocket,
            messageBuffer,
            static_cast<int>(messageSize),
            0
        );

        if (nBytesRead <= 0) {
            if (!_disconnect) {
                LERROR(
                    "Error " << _ERRNO <<
                    " detected in connection when reading message, disconnecting!");
                signalDisconnect();
            }
            break;
        }

        // And delegate decoding depending on type
        queueInMessage(Message(static_cast<MessageType>(messageTypeIn), messageBuffer));
    }

}

int ParallelConnection::receiveData(_SOCKET& socket, std::vector<char>& buffer,
                                    int length, int flags)
{
    int result = 0;
    int received = 0;
    while (result < length) {
        received = recv(socket, buffer.data() + result, length - result, flags);

        if (received > 0) {
            result += received;
            received = 0;
        }
        else {
            // Error receiving
            result = received;
            break;
        }
    }

    return result;
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
    queueOutMessage(Message(MessageType::HostshipRequest, buffer));
}

void ParallelConnection::resignHostship() {
    std::vector<char> buffer;
    queueOutMessage(Message(MessageType::HostshipResignation, buffer));
}

void ParallelConnection::setPassword(std::string pwd) {
    _password = std::move(pwd);
}

void ParallelConnection::setHostPassword(std::string pwd) {
    _hostPassword = std::move(pwd);
}

bool ParallelConnection::initNetworkAPI() {
#if defined(WIN32)
    WORD version = MAKEWORD(2, 2);
    WSADATA wsaData;
    const int error = WSAStartup(version, &wsaData);

    if (error != 0 || LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2) {
        // Incorrect WinSock version
        LERROR("Failed to init winsock API.");
        // WSACleanup();
        return false;
    }
#else
    // No init needed on unix
#endif

    return true;
}

void ParallelConnection::sendScript(std::string script) {
    if (!isHost()) {
        return;
    }

    datamessagestructures::ScriptMessage sm;
    sm._script = std::move(script);
    
    std::vector<char> buffer;
    sm.serialize(buffer);

    queueOutDataMessage(DataMessage(datamessagestructures::Type::ScriptData, buffer));
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
    queueOutDataMessage(DataMessage(datamessagestructures::Type::CameraData, buffer));
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
    queueOutDataMessage(DataMessage(datamessagestructures::Type::TimeData, buffer));
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
