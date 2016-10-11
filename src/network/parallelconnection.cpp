/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

//openspace includes
#include <openspace/network/parallelconnection.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>
#include <openspace/openspace.h>
#include <ghoul/logging/logmanager.h>

//lua functions
#include "parallelconnection_lua.inl"

namespace {
    const uint32_t ProtocolVersion = 2;
    const size_t MaxLatencyDiffs = 64;
    const double BroadcastIntervalMilliseconds = 100;
    const std::string _loggerCat = "ParallelConnection";
    const int nFrametimesBuffer = 4;
    const int nBroadcastIntervalsBuffer = 2;
}

namespace openspace {

ParallelConnection::ParallelConnection()
    : _passCode(0)
    , _port("20501")
    , _address("127.0.0.1")
    , _name("Local Connection")
    , _clientSocket(INVALID_SOCKET)
    , _connectionThread(nullptr)
    , _broadcastThread(nullptr)
    , _sendThread(nullptr)
    , _listenThread(nullptr)
    , _handlerThread(nullptr)
    , _isRunning(true)
    , _nConnections(0)
    , _status(Status::Disconnected)
    , _hostName("")
    , _isConnected(false)
    , _tryConnect(false)
    , _disconnect(false)
    , _initializationTimejumpRequired(false)
{
    _connectionEvent = std::make_shared<ghoul::Event<>>();
    _handlerThread = std::make_unique<std::thread>(&ParallelConnection::threadManagement, this);
}
        
ParallelConnection::~ParallelConnection(){
            
    //signal that a disconnect should occur
    signalDisconnect();
            
    //signal that execution has stopped
    _isRunning.store(false);
            
    //join handler
    _handlerThread->join();
}
        
void ParallelConnection::threadManagement(){
    // The _disconnectCondition.wait(unqlock) stalls
    // How about moving this out of the thread and into the destructor? ---abock
    
    //while we're still running
    while(_isRunning){
        {
            //lock disconnect mutex mutex
            //not really needed since no data is modified but conditions need a mutex
            std::unique_lock<std::mutex> disconnectLock(_disconnectMutex);
            //wait for a signal to disconnect
            _disconnectCondition.wait(disconnectLock, [this]() { return _disconnect.load(); });
                
            //perform actual disconnect
            disconnect();
                    
        }
    }
}
        
void ParallelConnection::signalDisconnect(){
    //signal handler thread to disconnect
    _disconnect = true;
    _sendCondition.notify_all(); // Allow send function to terminate.
    _disconnectCondition.notify_all(); // Unblock thread management thread.
}
        
void ParallelConnection::closeSocket(){
            
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
        
void ParallelConnection::disconnect(){
    //we're disconnecting
            
    if (_clientSocket != INVALID_SOCKET){
                
        //must be run before trying to join communication threads, else the threads are stuck trying to receive data
        closeSocket();
                
        //tell connection thread to stop trying to connect
        _tryConnect.store(false);
                
        //tell send thread to stop sending and listen thread to stop listenin
        _isConnected.store(false);
                
        //tell broadcast thread to stop broadcasting (we're no longer host)
        setStatus(Status::Disconnected);
               
        //join connection thread and delete it
        if(_connectionThread != nullptr){
            _connectionThread->join();
            _connectionThread = nullptr;
        }
                
        //join send thread and delete it
        if (_sendThread != nullptr){
            _sendThread->join();
            _sendThread = nullptr;
        }
                
        //join listen thread and delete it
        if( _listenThread != nullptr){
            _listenThread->join();
            _listenThread = nullptr;
        }
                
        //join broadcast thread and delete it
        if(_broadcastThread != nullptr){
            _broadcastThread->join();
            _broadcastThread = nullptr;
        }
                
        // disconnect and cleanup completed
        _disconnect = false;
    }
}
        
void ParallelConnection::clientConnect(){

    //we're already connected (or already trying to connect), do nothing (dummy check)
    if(_isConnected.load() || _tryConnect.load()){
        return;
    }
            
    if (!initNetworkAPI()){
        LERROR("Failed to initialize network API for Parallel Connection");
        return;
    }
            
    struct addrinfo *addresult = NULL, *ptr = NULL, hints;
    
    memset(&hints, 0, sizeof(hints));
    
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
    hints.ai_flags = AI_PASSIVE;

    // Resolve the local address and port to be used by the server
    int result = getaddrinfo(_address.c_str(), _port.c_str(), &hints, &addresult);
    if (result != 0)
    {
        LERROR("Failed to parse hints for Parallel Connection");
        return;
    }
            
    //we're not connected
    _isConnected.store(false);
            
    //we want to try and establish a connection
    _tryConnect.store(true);
            
    //start connection thread
    _connectionThread = std::make_unique<std::thread>(&ParallelConnection::establishConnection, this, addresult);
            
}

void ParallelConnection::establishConnection(addrinfo *info){

    _clientSocket = socket(info->ai_family, info->ai_socktype, info->ai_protocol);
            
    if (_clientSocket == INVALID_SOCKET){
        freeaddrinfo(info);
        LERROR("Failed to create client socket, disconnecting.");

        //signal a disconnect
        signalDisconnect();
    }
            
    int trueFlag = 1;
    int falseFlag = 0;
    int result;
            
    //set no delay
    result = setsockopt(_clientSocket,      /* socket affected */
                        IPPROTO_TCP,        /* set option at TCP level */
                        TCP_NODELAY,        /* name of option */
                        (char *)&trueFlag,  /* the cast is historical cruft */
                        sizeof(int));       /* length of option value */
            
    //set send timeout
    int timeout = 0;
    result = setsockopt(
                        _clientSocket,
                        SOL_SOCKET,
                        SO_SNDTIMEO,
                        (char *)&timeout,
                        sizeof(timeout));
            
    //set receive timeout
    result = setsockopt(
                        _clientSocket,
                        SOL_SOCKET,
                        SO_RCVTIMEO,
                        (char *)&timeout,
                        sizeof(timeout));

    result = setsockopt(_clientSocket, SOL_SOCKET, SO_REUSEADDR, (char*)&falseFlag, sizeof(int));
    if (result == SOCKET_ERROR)
        LERROR("Failed to set socket option 'reuse address'. Error code: " << _ERRNO);
            
    result = setsockopt(_clientSocket, SOL_SOCKET, SO_KEEPALIVE, (char*)&trueFlag, sizeof(int));
    if (result == SOCKET_ERROR)
        LERROR("Failed to set socket option 'keep alive'. Error code: " << _ERRNO);


    //while the connection thread is still running
    while (_tryConnect.load()){
                
        LINFO("Attempting to connect to server "<< _address << " on port " << _port);
                
        //try to connect
        result = connect(_clientSocket, info->ai_addr, (int)info->ai_addrlen);
                
        //if the connection was successfull
        if (result != SOCKET_ERROR)
        {
                    
            LINFO("Connection established with server at ip: "<< _address);
                    
            //we're connected
            _isConnected.store(true);
                    
            //start sending messages
            _sendThread = std::make_unique<std::thread>(&ParallelConnection::sendFunc, this);
                    
            //start listening for communication
            _listenThread = std::make_unique<std::thread>(&ParallelConnection::listenCommunication, this);
                    
            //we no longer need to try to establish connection
            _tryConnect.store(false);
                    
            _sendBufferMutex.lock();
            _sendBuffer.clear();
            _sendBufferMutex.unlock();

            //send authentication
            sendAuthentication();
        } else {
            LINFO("Connection attempt failed.");
        }
                
#ifdef WIN32
        //on windows: try to connect once per second
        std::this_thread::sleep_for(std::chrono::seconds(1));
#else
        if(!_isConnected.load()){
            //on unix disconnect and display error message if we're not connected
            LERROR("Failed to establish a connection with server "<< _address << " on port " << _port<<", terminating connection.");
                    
            //signal disconnect
            signalDisconnect();
                    
            //stop loop
            break;
        }
#endif
    }
            
    //cleanup
    freeaddrinfo(info);
}

void ParallelConnection::sendAuthentication() {
    //length of this nodes name
    uint32_t nameLength = static_cast<uint32_t>(_name.length());
            
    //total size of the buffer: (passcode + namelength + name)
    size_t size = 2 * sizeof(uint32_t) + nameLength;

    //create and reserve buffer
    std::vector<char> buffer;
    buffer.reserve(size);

    //write passcode to buffer
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_passCode), reinterpret_cast<char*>(&_passCode) + sizeof(uint32_t));

    //write the length of the nodes name to buffer
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&nameLength), reinterpret_cast<char*>(&nameLength) + sizeof(uint32_t));

    //write this node's name to buffer
    buffer.insert(buffer.end(), _name.begin(), _name.end());
            
    //send buffer
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
    double latencyStandardDeviation = std::sqrt(latencyVariance);

    double frametime = OsEng.windowWrapper().averageDeltaTime();

    double latencyCompensation = std::max(expectedLatencyDiff + 2 * latencyStandardDeviation, latencyDiff);

    return originalTime + timeDiff + nBroadcastIntervalsBuffer * BroadcastIntervalMilliseconds / 1000 + latencyCompensation + nFrametimesBuffer * frametime;

}

void ParallelConnection::dataMessageReceived(const std::vector<char>& messageContent) {
    
    //the type of data message received
    uint32_t type = *(reinterpret_cast<const uint32_t*>(messageContent.data()));   
    std::vector<char> buffer(messageContent.begin() + sizeof(uint32_t), messageContent.end());
 
    switch(static_cast<datamessagestructures::Type>(type)) {
        case datamessagestructures::Type::CameraData: {

            datamessagestructures::CameraKeyframe kf(buffer);
            kf._timestamp = calculateBufferedKeyframeTime(kf._timestamp);

            OsEng.interactionHandler().addKeyframe(kf);
            break;
        }
        case datamessagestructures::Type::TimeData: {

            datamessagestructures::TimeKeyframe kf(buffer);
            kf._timestamp = calculateBufferedKeyframeTime(kf._timestamp);

            OsEng.timeManager().addKeyframe(kf);
            break;
        }
        case datamessagestructures::Type::ScriptData: {
            datamessagestructures::ScriptMessage sm;
            sm.deserialize(buffer);
            OsEng.scriptEngine().queueScript(sm._script, scripting::ScriptEngine::RemoteScripting::No);
            
            break;
        }
        default:{
            LERROR("Unidentified data message with identifier " << type << " received in parallel connection.");
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
    //while we're connected
    while(_isConnected && !_disconnect) {
        //wait for signal then lock mutex and send first queued message
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
                reinterpret_cast<const char*>(&ProtocolVersion) + sizeof(uint32_t));

            header.insert(header.end(),
                reinterpret_cast<const char*>(&messageTypeOut),
                reinterpret_cast<const char*>(&messageTypeOut) + sizeof(uint32_t));

            header.insert(header.end(),
                reinterpret_cast<const char*>(&messageSizeOut),
                reinterpret_cast<const char*>(&messageSizeOut) + sizeof(uint32_t));

            result = send(_clientSocket, header.data(), header.size(), 0);
            result = send(_clientSocket, message.content.data(), message.content.size(), 0);

            if (result == SOCKET_ERROR) {
                LERROR("Failed to send message.\nError: " << _ERRNO << " detected in connection, disconnecting.");
                signalDisconnect();
            }

            unqlock.lock();
            _sendBuffer.erase(_sendBuffer.begin());
        }
    }
    std::lock_guard<std::mutex> sendLock(_sendBufferMutex);
    _sendBuffer.clear();

}
        
void ParallelConnection::connectionStatusMessageReceived(const std::vector<char>& message) {
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
        //status remains unchanged.
        return;
    }

    setStatus(status);

    if (status == Status::Host) { // assigned as host 
        _broadcastThread = std::make_unique<std::thread>(&ParallelConnection::broadcast, this);
    } else { // assigned as client
        
        // delete broadcasting thread
        // (the thread is allowed to terminate once the status is set to non-host.)
        if (_broadcastThread != nullptr) {
            _broadcastThread->join();
            _broadcastThread = nullptr;
        }
    }
    OsEng.interactionHandler().clearKeyframes();
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

    //create basic buffer for receiving first part of messages
    std::vector<char> headerBuffer(headerSize);
    std::vector<char> messageBuffer;

    int nBytesRead = 0;
            
    //while we're still connected
    while (_isConnected.load()) {
        //receive the header data
        nBytesRead = receiveData(_clientSocket, headerBuffer, headerSize, 0);

        //if enough data was received
        if (nBytesRead <= 0) {
            if (!_disconnect) {
                LERROR("Error " << _ERRNO << " detected in connection when reading header, disconnecting!");
                signalDisconnect();
            }
            break;
        }

        //make sure that header matches this version of OpenSpace
        if (!(headerBuffer[0] == 'O' && headerBuffer[1] && 'S')) {
            LERROR("Expected to read message header 'OS' from socket.");
            signalDisconnect();
            break;
        }

        uint32_t *ptr = reinterpret_cast<uint32_t*>(&headerBuffer[2]);

        uint32_t protocolVersionIn = *(ptr++);
        uint32_t messageTypeIn = *(ptr++);
        uint32_t messageSizeIn = *(ptr++);

        if (protocolVersionIn != ProtocolVersion) {
            LERROR("Protocol versions do not match. Server version: " << protocolVersionIn << ", Client version: " << ProtocolVersion);
            signalDisconnect();
            break;
        }

        size_t messageSize = messageSizeIn;

        // receive the payload
        messageBuffer.resize(messageSize);
        nBytesRead = receiveData(_clientSocket, messageBuffer, messageSize, 0);

        if (nBytesRead <= 0) {
            if (!_disconnect) {
                LERROR("Error " << _ERRNO << " detected in connection when reading message, disconnecting!");
                signalDisconnect();
            }
            break;
        }

        //and delegate decoding depending on type
        queueInMessage(Message(static_cast<MessageType>(messageTypeIn), messageBuffer));
    }

}

int ParallelConnection::receiveData(_SOCKET & socket, std::vector<char> &buffer, int length, int flags){
    int result = 0;
    int received = 0;
    while (result < length){
        received = recv(socket, buffer.data() + result, length - result, flags);

        if (received > 0){
            result += received;
            received = 0;
        }
        else{
            //error receiving
            result = received;
            break;
        }
    }

    return result;
}
        
void ParallelConnection::setPort(const std::string  &port){
    _port = port;
}

void ParallelConnection::setAddress(const std::string &address){
    _address = address;
}
        
void ParallelConnection::setName(const std::string& name){
    _name = name;
}
        
       
void ParallelConnection::requestHostship(const std::string &password){
    std::vector<char> buffer;
    uint32_t passcode = hash(password);
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&passcode), reinterpret_cast<char*>(&passcode) + sizeof(uint32_t));
    queueOutMessage(Message(MessageType::HostshipRequest, buffer));
}

void ParallelConnection::resignHostship() {
    std::vector<char> buffer;
    queueOutMessage(Message(MessageType::HostshipResignation, buffer));
}

void ParallelConnection::setPassword(const std::string& pwd){
    _passCode = hash(pwd);
}

bool ParallelConnection::initNetworkAPI(){
    #if defined(WIN32)
        WSADATA wsaData;
        WORD version;
        int error;

        version = MAKEWORD(2, 2);

        error = WSAStartup(version, &wsaData);

        if (error != 0 ||
            LOBYTE(wsaData.wVersion) != 2 ||
            HIBYTE(wsaData.wVersion) != 2)
        {
            /* incorrect WinSock version */
            LERROR("Failed to init winsock API.");
            //WSACleanup();
            return false;
        }
    #else
                //No init needed on unix
    #endif

    return true;
}

void ParallelConnection::sendScript(const std::string& script) {
    if (!isHost()) return;

    datamessagestructures::ScriptMessage sm;
    sm._script = script;
    
    std::vector<char> buffer;
    sm.serialize(buffer);

    queueOutDataMessage(DataMessage(datamessagestructures::Type::ScriptData, buffer));
}

void ParallelConnection::preSynchronization(){

    std::unique_lock<std::mutex> unqlock(_receiveBufferMutex);
    while (!_receiveBuffer.empty()) {
        Message& message = _receiveBuffer.front();
        handleMessage(message);
        _receiveBuffer.pop_front();
    }
    
    if (status() == Status::Host) {
        if (Time::ref().timeJumped()) {
            _timeJumped = true;
        }
    }
}
         
void ParallelConnection::setStatus(Status status) {
    if (_status != status) {
        _status = status;
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

size_t ParallelConnection::nConnections() {
    return _nConnections;
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
    //create a keyframe with current position and orientation of camera
    datamessagestructures::CameraKeyframe kf;
    kf._position = OsEng.interactionHandler().camera()->positionVec3();
    kf._rotation = OsEng.interactionHandler().camera()->rotationQuaternion();

    //timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.runTime();

    //create a buffer for the keyframe
    std::vector<char> buffer;

    //fill the keyframe buffer
    kf.serialize(buffer);

    //send message
    queueOutDataMessage(DataMessage(datamessagestructures::Type::CameraData, buffer));
}

void ParallelConnection::sendTimeKeyframe() {
    //create a keyframe with current position and orientation of camera
    datamessagestructures::TimeKeyframe kf;
    
    kf._dt = Time::ref().deltaTime();
    kf._paused = Time::ref().paused();
    kf._requiresTimeJump = _timeJumped;
    kf._time = Time::ref().j2000Seconds();

    //timestamp as current runtime of OpenSpace instance
    kf._timestamp = OsEng.runTime();

    //create a buffer for the keyframe
    std::vector<char> buffer;

    //fill the keyframe buffer
    kf.serialize(buffer);

    //send message
    queueOutDataMessage(DataMessage(datamessagestructures::Type::TimeData, buffer));
    _timeJumped = false;
}


void ParallelConnection::broadcast(){
    _timeJumped = true;
    //while we're still connected and we're the host
    while (_isConnected && isHost()) {
        sendCameraKeyframe();
        sendTimeKeyframe();

        //100 ms sleep - send keyframes 10 times per second
        std::this_thread::sleep_for(std::chrono::milliseconds(static_cast<int>(BroadcastIntervalMilliseconds)));
    }
}

uint32_t ParallelConnection::hash(const std::string &val) {
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
                "setPort",
                &luascriptfunctions::setPort,
                "number",
                "Set the port for the parallel connection"
            },
            {
                "setAddress",
                &luascriptfunctions::setAddress,
                "string",
                "Set the address for the parallel connection"
            },
            {
                "setPassword",
                &luascriptfunctions::setPassword,
                "string",
                "Set the password for the parallel connection"
            },
            {
                "setDisplayName",
                &luascriptfunctions::setDisplayName,
                "string",
                "Set your display name for the parallel connection"
            },
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
                "string",
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
