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

#ifdef __WIN32__
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
#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/time.h>
#include <openspace/openspace.h>
#include <ghoul/logging/logmanager.h>

//lua functions
#include "parallelconnection_lua.inl"

namespace {
    const std::string _loggerCat = "ParallelConnection";
}

namespace openspace {

namespace network {
        
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
    , _isHost(false)
    , _isConnected(false)
    , _tryConnect(false)
    , _performDisconnect(false)
    , _latestTimeKeyframeValid(false)
    , _initializationTimejumpRequired(false)
{
    //create handler thread
    _handlerThread = new (std::nothrow) std::thread(&ParallelConnection::threadManagement, this);
}
        
ParallelConnection::~ParallelConnection(){
            
    //signal that a disconnect should occur
    signalDisconnect();
            
    //signal that execution has stopped
    _isRunning.store(false);
            
    //join handler
    _handlerThread->join();
            
    //and delete handler
    delete _handlerThread;
}
        
void ParallelConnection::threadManagement(){
    // The _disconnectCondition.wait(unqlock) stalls
    // How about moving this out of the thread and into the destructor? ---abock
    
    //while we're still running
    while(_isRunning.load()){
        {
            //lock disconnect mutex mutex
            //not really needed since no data is modified but conditions need a mutex
            std::unique_lock<std::mutex> unqlock(_disconnectMutex);
            //wait for a signal to disconnect
            _disconnectCondition.wait(unqlock);
                
            //perform actual disconnect
            disconnect();
                    
        }
    }
}
        
void ParallelConnection::signalDisconnect(){
    //signal handler thread to disconnect
    _disconnectCondition.notify_all();
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
            
#ifdef __WIN32__
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
        _isHost.store(false);
                
        //signal send thread to stop waiting and finish current run
        _sendCondition.notify_all();
                
        //join connection thread and delete it
        if(_connectionThread != nullptr){
            _connectionThread->join();
            delete _connectionThread;
            _connectionThread = nullptr;
        }
                
        //join send thread and delete it
        if (_sendThread != nullptr){
            _sendThread->join();
            delete _sendThread;
            _sendThread = nullptr;
        }
                
        //join listen thread and delete it
        if( _listenThread != nullptr){
            _listenThread->join();
            delete _listenThread;
            _listenThread = nullptr;
        }
                
        //join broadcast thread and delete it
        if(_broadcastThread != nullptr){
            _broadcastThread->join();
            delete _broadcastThread;
            _broadcastThread = nullptr;
        }
                
        // disconnect and cleanup completed
        _performDisconnect.store(false);
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
    #ifdef __WIN32__ //WinSock
        ZeroMemory(&hints, sizeof(hints));
    #else
        memset(&hints, 0, sizeof(hints));
    #endif
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
    _connectionThread = new (std::nothrow) std::thread(&ParallelConnection::establishConnection, this, addresult);
            
}

void ParallelConnection::establishConnection(addrinfo *info){

    _clientSocket = socket(info->ai_family, info->ai_socktype, info->ai_protocol);
            
    if (_clientSocket == INVALID_SOCKET){
        freeaddrinfo(info);
        LERROR("Failed to create client socket, disconnecting.");

        //signal a disconnect
        signalDisconnect();
    }
            
    int flag = 1;
    int result;
            
    //set no delay
    result = setsockopt(_clientSocket,      /* socket affected */
                        IPPROTO_TCP,        /* set option at TCP level */
                        TCP_NODELAY,        /* name of option */
                        (char *)&flag,      /* the cast is historical cruft */
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

    result = setsockopt(_clientSocket, SOL_SOCKET, SO_REUSEADDR, (char*)&flag, sizeof(int));
    if (result == SOCKET_ERROR)
        LERROR("Failed to set socket option 'reuse address'. Error code: " << _ERRNO);
            
    result = setsockopt(_clientSocket, SOL_SOCKET, SO_KEEPALIVE, (char*)&flag, sizeof(int));
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
            _sendThread = new (std::nothrow) std::thread(&ParallelConnection::sendFunc, this);
                    
            //start listening for communication
            _listenThread = new (std::nothrow) std::thread(&ParallelConnection::listenCommunication, this);
                    
            //we no longer need to try to establish connection
            _tryConnect.store(false);
                    
            //send authentication
            sendAuthentication();
        }
                
#ifdef __WIN32__
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

void ParallelConnection::sendAuthentication(){
    //length of this nodes name
    uint16_t namelen = static_cast<uint16_t>(_name.length());
            
    //total size of the buffer, header + size of passcodde + namelength + size of namelength
    int size = headerSize() + sizeof(uint32_t) + sizeof(namelen) + static_cast<int>(namelen);

    //create and reserve buffer
    std::vector<char> buffer;
    buffer.reserve(size);

    //write header to buffer
    writeHeader(buffer, MessageTypes::Authentication);

    //write passcode to buffer
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_passCode), reinterpret_cast<char*>(&_passCode) + sizeof(uint32_t));

    //write the length of the nodes name to buffer
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&namelen), reinterpret_cast<char*>(&namelen) + sizeof(uint16_t));

    //write this nodes name to buffer
    buffer.insert(buffer.end(), _name.begin(), _name.end());
            
    //send buffer
    queueMessage(buffer);
}

void ParallelConnection::delegateDecoding(uint32_t type){
    switch (type){
    case MessageTypes::Authentication:
        //not used
        break;
    case MessageTypes::Initialization:
        initializationMessageReceived();
        break;
    case MessageTypes::Data:
        dataMessageReceived();
        break;
    case MessageTypes::Script:
            //not used
        break;
    case MessageTypes::HostInfo:
        hostInfoMessageReceived();
        break;
    case MessageTypes::InitializationRequest:
        initializationRequestMessageReceived();
        break;
    default:
        //unknown message type
        break;
    }
}

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
    queueMessage(buffer);
            
    //we also need to force a time jump just to ensure that the server and client are synced
    _initializationTimejumpRequired.store(true);
            
}

void ParallelConnection::dataMessageReceived(){
    int result;
    uint16_t msglen;
    uint16_t type;

    //create a buffer to hold the size of streamdata message
    std::vector<char> buffer;
    buffer.resize(sizeof(type));
            
    //read type of data message
    result = receiveData(_clientSocket, buffer, sizeof(type), 0);

    if (result <= 0){
        //error
        LERROR("Failed to read type of data message received.");
        return;
    }
            
    //the type of data message received
    type =(*(reinterpret_cast<uint16_t*>(buffer.data())));
            
    //read size of streamdata message
    result = receiveData(_clientSocket, buffer, sizeof(msglen), 0);
            
    if (result <= 0){
        //error
        LERROR("Failed to read size of data message received.");
        return;
    }
            
    //the size in bytes of the streamdata message
    msglen = (*(reinterpret_cast<uint16_t*>(buffer.data())));

    //resize the buffer to be able to read the streamdata
    buffer.clear();
    buffer.resize(msglen);

    //read the data into buffer
    result = receiveData(_clientSocket, buffer, msglen, 0);
            
    if (result <= 0){
        //error
        LERROR("Failed to read data message.");
        return;
    }
            
    //which type of data message was received?
    switch(type){
        case network::datamessagestructures::PositionData:{
            //position data message
            //create and read a position keyframe from the data buffer
            network::datamessagestructures::PositionKeyframe kf;
            kf.deserialize(buffer);
                    
            //add the keyframe to the interaction handler
            OsEng.interactionHandler().addKeyframe(kf);
            break;
        }
        case network::datamessagestructures::TimeData:{
            //time data message
            //create and read a time keyframe from the data buffer
            network::datamessagestructures::TimeKeyframe tf;
            tf.deserialize(buffer);
                    
            //lock mutex and assign latest time keyframe parameters
            _timeKeyframeMutex.lock();
                    
            _latestTimeKeyframe._dt = tf._dt;
            _latestTimeKeyframe._time = tf._time;
            _latestTimeKeyframe._paused = tf._paused;
                    
            //ensure that we never miss a timejump
            //if last keyframe required a jump and that keyframe has not been used yet
            if(_latestTimeKeyframe._requiresTimeJump && _latestTimeKeyframeValid){
                //do nothing to the boolean. Old value must be executed
            }else{
                //either the latest keyframe didnt require a jump, or we have already spent that keyframe.
                //in either case we can go ahead and write the bool value of newest frame
                _latestTimeKeyframe._requiresTimeJump = tf._requiresTimeJump;
            }
                    
            //if we're just initialized we need to perform a time jump as soon as a valid keyframe has been received
            if(_initializationTimejumpRequired.load() && _latestTimeKeyframeValid){
                _latestTimeKeyframe._requiresTimeJump = true;
                _initializationTimejumpRequired.store(false);
            }
                    
            //unlock mutex
            _timeKeyframeMutex.unlock();
                    
            //the keyframe is now valid for use
            _latestTimeKeyframeValid.store(true);
                    
            break;
        }
        case network::datamessagestructures::ScriptData:{
            //script data message
            //create and read a script message from data buffer
            network::datamessagestructures::ScriptMessage sm;
            sm.deserialize(buffer);
                    
            //Que script to be executed by script engine
            OsEng.scriptEngine().queueScript(sm._script);
            break;
        }
        default:{
            LERROR("Unidentified data message with identifier " << type << " received in parallel connection.");
            break;
        }
    }
}

void ParallelConnection::queueMessage(std::vector<char> message){
    {
        std::unique_lock<std::mutex> unqlock(_sendBufferMutex);
        _sendBuffer.push_back(message);
        _sendCondition.notify_all();
    }
}
        
void ParallelConnection::sendFunc(){
    int result;
    //while we're connected
    while(_isConnected.load()){
        {
            //wait for signal then lock mutex and send first queued message
            std::unique_lock<std::mutex> unqlock(_sendBufferMutex);
            _sendCondition.wait_for(unqlock, std::chrono::milliseconds(500));
                    
            if(!_sendBuffer.empty()){
                while(!_sendBuffer.empty()){
                    result = send(_clientSocket, _sendBuffer.front().data(), _sendBuffer.front().size(), 0);
                    _sendBuffer.erase(_sendBuffer.begin());
                            
                    //make sure everything went well
                    if (result == SOCKET_ERROR){
                        //failed to send message
                        LERROR("Failed to send message.\nError: " << _ERRNO << " detected in connection, disconnecting.");
                                
                        //signal that a disconnect should be performed
                        signalDisconnect();
                    }
                }
            }
        }
                
    }
}
        
void ParallelConnection::hostInfoMessageReceived(){
    //create buffer
    std::vector<char> hostflag;
    //resize to hold a flag saying if we're host or not
    hostflag.resize(1);
            
    //read data into buffer
    int result = receiveData(_clientSocket, hostflag, 1, 0);

    //enough data was read
    if (result > 0){
                
        //we've been assigned as host
        if (hostflag.at(0) == 1){
                    
            //we're already host, do nothing (dummy check)
            if (_isHost.load()){
                return;
            }
            else{
                        
                //we're the host
                _isHost.store(true);

                //start broadcasting
                _broadcastThread = new (std::nothrow) std::thread(&ParallelConnection::broadcast, this);
            }
        }
        else{   //we've been assigned as client
                    
            //we were broadcasting but should stop now
            if (_isHost.load()){

                //stop broadcast loop
                _isHost.store(false);
                        
                //and delete broadcasting thread
                if (_broadcastThread != nullptr){
                    _broadcastThread->join();
                    delete _broadcastThread;
                    _broadcastThread = nullptr;
                }                       
            }
            else{
                //we were not broadcasting so nothing to do
            }
                    
            //clear buffered any keyframes
            OsEng.interactionHandler().clearKeyframes();
                    
            //request init package from the host
            int size = headerSize();
            std::vector<char> buffer;
            buffer.reserve(size);
                    
            //write header
            writeHeader(buffer, MessageTypes::InitializationRequest);

            //send message
            queueMessage(buffer);
        }
    }
    else{
        LERROR("Error " << _ERRNO << " detected in connection, disconnecting.");
        signalDisconnect();
    }
}

void ParallelConnection::initializationRequestMessageReceived(){
            
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
    network::datamessagestructures::ScriptMessage sm;
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
    queueMessage(buffer);
}

void ParallelConnection::listenCommunication(){
            
    //create basic buffer for receiving first part of messages
    std::vector<char> buffer;
    //size of the header
    buffer.resize(headerSize());
            
    int result;
            
    //while we're still connected
    while (_isConnected.load()){
        //receive the first parts of a message
        result = receiveData(_clientSocket, buffer, headerSize(), 0);

        //if enough data was received
        if (result > 0){
                
            //make sure that header matches this version of OpenSpace
            if (buffer[0] == 'O' &&                     //Open
                buffer[1] == 'S' &&                     //Space
                buffer[2] == OPENSPACE_VERSION_MAJOR && // major version
                buffer[3] == OPENSPACE_VERSION_MINOR    // minor version
                )
            {
                //parse type
                uint32_t type = (*(reinterpret_cast<uint32_t*>(&buffer[4])));

                //and delegate decoding depending on type
                delegateDecoding(type);
            }
            else{
                LERROR("Error: Client OpenSpace version " << OPENSPACE_VERSION_MAJOR << ", " << OPENSPACE_VERSION_MINOR << " does not match server version " << buffer[2] <<", " << buffer[3] << std::endl << "Message not decoded.");
            }
        }
        else{
            if (result == 0){
                //connection rejected
                LERROR("Parallel connection rejected, disconnecting...");
            }
            else{
                LERROR("Error " << _ERRNO << " detected in connection, disconnecting!");
            }

            //signal that a disconnect should be performed
            signalDisconnect();
            break;
        }
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
        
bool ParallelConnection::isHost(){
    return _isHost.load();
}
        
void ParallelConnection::requestHostship(const std::string &password){
    std::vector<char> buffer;
    buffer.reserve(headerSize());
          
    uint32_t passcode = hash(password);
            
    //write header
    writeHeader(buffer, MessageTypes::HostshipRequest);
            
    //write passcode
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&passcode), reinterpret_cast<char*>(&passcode) + sizeof(uint32_t));
            
    //send message
    queueMessage(buffer);
}

void ParallelConnection::setPassword(const std::string& pwd){
    _passCode = hash(pwd);
}

bool ParallelConnection::initNetworkAPI(){
    #if defined(__WIN32__)
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

void ParallelConnection::preSynchronization(){
            
    //if we're the host
    if(_isHost){
        //get current time parameters and create a keyframe
        network::datamessagestructures::TimeKeyframe tf;
        tf._dt = Time::ref().deltaTime();
        tf._paused = Time::ref().paused();
        tf._requiresTimeJump = Time::ref().timeJumped();
        tf._time = Time::ref().currentTime();
                
        //create a buffer and serialize message
        std::vector<char> tbuffer;
        tf.serialize(tbuffer);
                
        //get the size of the keyframebuffer
        uint16_t msglen = static_cast<uint16_t>(tbuffer.size());
                
        //the type of data message
        uint16_t type = static_cast<uint16_t>(network::datamessagestructures::TimeData);
                
        //create the full buffer
        std::vector<char> buffer;
        buffer.reserve(headerSize() + sizeof(type) + sizeof(msglen) + msglen);
                
        //write header
        writeHeader(buffer, MessageTypes::Data);
                
        //type of message
        buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(type));
                
        //size of message
        buffer.insert(buffer.end(), reinterpret_cast<char*>(&msglen), reinterpret_cast<char*>(&msglen) + sizeof(msglen));
                
        //actual message
        buffer.insert(buffer.end(), tbuffer.begin(), tbuffer.end());
                
        //send message
        queueMessage(buffer);
    }
    else{
        //if we're not the host and we have a valid keyframe (one that hasnt been used before)
        if(_latestTimeKeyframeValid.load()){
                    
            //lock mutex and retrieve parameters from latest keyframe
            _timeKeyframeMutex.lock();
                    
            double dt = _latestTimeKeyframe._dt;
            double time = _latestTimeKeyframe._time;
            bool jump = _latestTimeKeyframe._requiresTimeJump;
            bool paused = _latestTimeKeyframe._paused;
                    
            _timeKeyframeMutex.unlock();
                    
            //this keyframe is now spent
            _latestTimeKeyframeValid.store(false);
                    
            //assign latest params
            Time::ref().setDeltaTime(dt);
            Time::ref().setTime(time, jump);
            Time::ref().setPause(paused);
                    
        }
    }
}
        
void ParallelConnection::scriptMessage(const std::string propIdentifier, const std::string propValue){
            
    //save script as current state
    {
        //mutex protect
        std::lock_guard<std::mutex> lock(_currentStateMutex);
        _currentState[propIdentifier] = propValue;
    }
            
    //if we're connected and we're the host, also send the script
    if(_isConnected.load() && _isHost.load()){
        //construct script
        std::string script = scriptFromPropertyAndValue(propIdentifier, propValue);
                
        //create a script message
        network::datamessagestructures::ScriptMessage sm;
        sm._script = script;
        sm._scriptlen = static_cast<uint16_t>(script.length());
                
        //create a buffer for the script
        std::vector<char> sbuffer;
                
        //fill the script buffer
        sm.serialize(sbuffer);
                
        //get the size of the keyframebuffer
        uint16_t msglen = static_cast<uint16_t>(sbuffer.size());
                
        //the type of message
        uint16_t type = static_cast<uint16_t>(network::datamessagestructures::ScriptData);
                
        //create the full buffer
        std::vector<char> buffer;
        buffer.reserve(headerSize() + sizeof(type) + sizeof(msglen) + msglen);
                
        //write header
        writeHeader(buffer, MessageTypes::Data);
                
        //type of message
        buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(type));
                
        //size of message
        buffer.insert(buffer.end(), reinterpret_cast<char*>(&msglen), reinterpret_cast<char*>(&msglen) + sizeof(msglen));
                
        //actual message
        buffer.insert(buffer.end(), sbuffer.begin(), sbuffer.end());
                
        //send message
        queueMessage(buffer);
    }

}
        
std::string ParallelConnection::scriptFromPropertyAndValue(const std::string property, const std::string value){
    //consruct script
    std::string script = "openspace.setPropertyValue(\"" + property + "\"," + value + ");";
    return script;
}
        
void ParallelConnection::broadcast(){
            
    //while we're still connected and we're the host
    while (_isConnected.load() && _isHost.load()){

        //create a keyframe with current position and orientation of camera
        network::datamessagestructures::PositionKeyframe kf;
        kf._position = OsEng.interactionHandler().camera()->position();
        kf._viewRotationQuat = glm::quat_cast(OsEng.interactionHandler().camera()->viewRotationMatrix());
                
        //timestamp as current runtime of OpenSpace instance
        kf._timeStamp = OsEng.runTime();
                
        //create a buffer for the keyframe
        std::vector<char> kfBuffer;
                
        //fill the keyframe buffer
        kf.serialize(kfBuffer);
                
        //get the size of the keyframebuffer
        uint16_t msglen = static_cast<uint16_t>(kfBuffer.size());
                
        //the type of message
        uint16_t type = static_cast<uint16_t>(network::datamessagestructures::PositionData);
                
        //create the full buffer
        std::vector<char> buffer;
        buffer.reserve(headerSize() + sizeof(type) + sizeof(msglen) + msglen);
                
        //write header
        writeHeader(buffer, MessageTypes::Data);
                
        //type of message
        buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(type));

        //size of message
        buffer.insert(buffer.end(), reinterpret_cast<char*>(&msglen), reinterpret_cast<char*>(&msglen) + sizeof(msglen));

        //actual message
        buffer.insert(buffer.end(), kfBuffer.begin(), kfBuffer.end());

        //send message
        queueMessage(buffer);

        //100 ms sleep - send keyframes 10 times per second
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
}
        
void ParallelConnection::writeHeader(std::vector<char> &buffer, uint32_t messageType){
    //make sure the buffer is large enough to hold at least the header
    if(buffer.size() < headerSize()){
        buffer.reserve(headerSize());
    }
            
    //get the current running version of openspace
    uint8_t versionMajor = static_cast<uint8_t>(OPENSPACE_VERSION_MAJOR);
    uint8_t versionMinor = static_cast<uint8_t>(OPENSPACE_VERSION_MINOR);
            
    //insert header into buffer
    buffer.insert(buffer.end(), 'O');
    buffer.insert(buffer.end(), 'S');
    buffer.insert(buffer.end(), versionMajor);
    buffer.insert(buffer.end(), versionMinor);
    buffer.insert(buffer.end(), reinterpret_cast<char*>(&messageType), reinterpret_cast<char*>(&messageType) + sizeof(messageType));
}
        
int ParallelConnection::headerSize(){
    //minor and major version (as uint8_t -> 1 byte) + two bytes for the chars 'O' and 'S' + 4 bytes for type of message
    return 2 * sizeof(uint8_t) + 2 + sizeof(uint32_t);
}

scripting::ScriptEngine::LuaLibrary ParallelConnection::luaLibrary() {
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
        }
    };
}
        
} // namespace network
    
} // namespace openspace
