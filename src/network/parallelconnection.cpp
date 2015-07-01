/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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
#ifdef _XCODE
#include <unistd.h>
#endif
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
#include <openspace/network/ParallelConnection.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/time.h>
#include <openspace/version.h>
#include <ghoul/logging/logmanager.h>

//lua functions
#include "ParallelConnection_lua.inl"

namespace{
	const std::string _loggerCat = "ParallelConnection";
}

namespace openspace {
    namespace network{
        
        ParallelConnection::ParallelConnection():
        _passCode(0),
        _port("20501"),
        _address("127.0.0.1"),
        _name("Local Connection"),
        _clientSocket(INVALID_SOCKET),
		_connectionThread(nullptr),
		_broadcastThread(nullptr),
        _sendThread(nullptr),
        _isHost(false),
        _isConnected(false),
        _isListening(false)
        {

        }
        
        ParallelConnection::~ParallelConnection(){
            disconnect();
        }
        
		void ParallelConnection::clientConnect(){
            //we're already connected, do nothing
            if(_isConnected.load()){
                return;
            }
            
			if (!initNetworkAPI()){
                LERROR("Failed to initialize network API for Parallel Connection");
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
			#if defined(__WIN32__)
				WSACleanup();
			#endif
                LERROR("Failed to parse hints for Parallel Connection");
			}

            //we're not connected
			_isConnected.store(false);
            
            //start connection thread
			_connectionThread = new (std::nothrow) std::thread(&ParallelConnection::tryConnect, this, addresult);
			
        }

		void ParallelConnection::tryConnect(addrinfo *info){

            _clientSocket = socket(info->ai_family, info->ai_socktype, info->ai_protocol);
            
            if (_clientSocket == INVALID_SOCKET){
                freeaddrinfo(info);
#if defined(__WIN32__)
                WSACleanup();
#endif
                LERROR("Failed to create client socket, shutting down connection thread");
                return;
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
            while (!_isConnected.load()){
                
                LINFO("Attempting to connect to server "<< _address << " on port " << _port);
                
                //try to connect
                result = connect(_clientSocket, info->ai_addr, (int)info->ai_addrlen);
                
                //if the connection was successfull
                if (result != SOCKET_ERROR)
                {
                    //we're connected
                    _isConnected.store(true);
                    
                    //and ready to start receiving messages
                    _isListening.store(true);
                    
                    //send authentication
                    sendAuthentication();
                    
                    //start listening for communication
                    listenCommunication();
                    
                }
                
                //try to connect once per second
                std::this_thread::sleep_for(std::chrono::seconds(1));
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
			int result = send(_clientSocket, buffer.data(), size, 0);

            //if send failed
			if (result == SOCKET_ERROR){
				//failed to send auth msg.
                LERROR("Failed to send authentication message.");
			}
		}

		void ParallelConnection::delegateDecoding(uint32_t type){
			switch (type){
			case MessageTypes::Authentication:
                //do nothing for now
				break;
			case MessageTypes::Initialization:
				decodeInitializationMessage();
				break;
			case MessageTypes::StreamData:
				decodeStreamDataMessage();
				break;
            case MessageTypes::Script:
                decodeScriptMessage();
                break;
			case MessageTypes::HostInfo:
				decodeHostInfoMessage();
				break;
			case MessageTypes::InitializationRequest:
				decodeInitializationRequestMessage();
				break;
			default:
				//unknown message type
				break;
			}
		}

		void ParallelConnection::decodeInitializationMessage(){
            
            int result;
            uint16_t numScripts;
            
            //create a buffer to hold the number of scripts in the initialization message
            std::vector<char> buffer;
            buffer.resize(sizeof(numScripts));
            
            //read number of scripts in the initialization message
            result = receiveData(_clientSocket, buffer, sizeof(numScripts), 0);
            
            if (result <= 0){
                //error
                return;
            }
            
            //assign number of scripts
            numScripts = *reinterpret_cast<uint16_t*>(buffer.data());
            
            //declare placeholder for all received scripts
            std::vector<std::string> initScripts;
            initScripts.reserve(numScripts);
            
            //length of each script and resize receiveing buffer
            uint16_t scriptlen;
            buffer.clear();
            buffer.resize(sizeof(scriptlen));
            
            //buffer for holding received scripts
            std::vector<char> scriptbuffer;
            
            for(int n = 0; n < numScripts; ++n){
                
                //read size in chars of next script
                result = receiveData(_clientSocket, buffer, sizeof(scriptlen), 0);
                
                if (result <= 0){
                    //error
                    return;
                }
                
                //assign size of next script
                scriptlen = *reinterpret_cast<uint16_t*>(buffer.data());
                
                //resize buffer
                scriptbuffer.clear();
                scriptbuffer.resize(scriptlen);
                
                //read next script
                result = receiveData(_clientSocket, scriptbuffer, scriptlen, 0);
                
                if (result <= 0){
                    //error
                    return;
                }
                
                //create a string from received chars in buffer
                std::string script;
                script.assign(scriptbuffer.begin(), scriptbuffer.end());
                
                //que script with the script engine
                OsEng.scriptEngine()->queueScript(script);
                
            }
            
		}

		void ParallelConnection::decodeStreamDataMessage(){
			int result;
			uint16_t msglen;

            //create a buffer to hold the size of streamdata message
            std::vector<char> buffer;
			buffer.resize(sizeof(msglen));
            
            //read size of streamdata message
			result = receiveData(_clientSocket, buffer, sizeof(msglen), 0);

			if (result <= 0){
				//error
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
				return;
			}
            
            //construct a keyframe ffrom the streamdata
            network::StreamDataKeyframe kf;
            kf.deserialize(buffer);
            
            //and add the keyframe to the interaction handler
            OsEng.interactionHandler()->addKeyframe(kf);            
		}

        void ParallelConnection::decodeScriptMessage(){
            int result;
            uint16_t msglen;
            
            //create buffer to decode size of script
            std::vector<char> buffer;
            buffer.resize(sizeof(msglen));
            
            //read size of received script
            result = receiveData(_clientSocket, buffer, sizeof(msglen), 0);
            
            if (result <= 0){
                //error
                return;
            }
            
            //size of recived script
            msglen = (*(reinterpret_cast<uint16_t*>(buffer.data())));
            
            //clear and resize buffer to decode actual script
            buffer.clear();
            buffer.resize(msglen);
            
            //decode script
            result = receiveData(_clientSocket, buffer, msglen, 0);
            
            if (result <= 0){
                //error
                return;
            }
            
            //construct a script (string) from the data contained in the buffer
            std::string script;
            script.assign(buffer.begin(), buffer.end());

            //tell the script engine to execute the script when appropriate
            OsEng.scriptEngine()->queueScript(script);
        }
        
        void ParallelConnection::queMessage(std::vector<char> message){
            _sendBufferMutex.lock();
            _sendBuffer.push_back(message);
            _sendBufferMutex.unlock();
        }
        
        void ParallelConnection::sendLoop(){
            while(_isHost.load()){
                _sendBufferMutex.lock();
                if(_sendBuffer.size() > 0){
                    //send first queued message
                    send(_clientSocket, _sendBuffer.front().data(), _sendBuffer.front().size(), 0);
                    _sendBuffer.erase(_sendBuffer.begin());
                }
                _sendBufferMutex.unlock();
            }
        }
        
		void ParallelConnection::decodeHostInfoMessage(){
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
                        
						//start broadcasting
						_isHost.store(true);
						_broadcastThread = new (std::nothrow) std::thread(&ParallelConnection::broadcast, this);
                        
                        //and sending messages
                        _sendThread = new (std::nothrow) std::thread(&ParallelConnection::sendLoop, this);
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
                        
                        //delete send thread
                        if(_sendThread != nullptr){
                            _sendThread->join();
                            delete _sendThread;
                            _sendThread = nullptr;
                            
                            //and clear all queued messages
                            _sendBufferMutex.lock();
                            _sendBuffer.clear();
                            _sendBufferMutex.unlock();
                        }
                        
					}
					else{
						//we were not broadcasting so nothing to do
					}
                    
                    //clear buffered any keyframes
                    OsEng.interactionHandler()->clearKeyframes();
                    
                    //request init package from the host
                    int size = headerSize();
                    std::vector<char> buffer;
                    buffer.reserve(size);
                    
                    //write header
                    writeHeader(buffer, MessageTypes::InitializationRequest);

                    //send message
                    send(_clientSocket, buffer.data(), buffer.size(), 0);

				}
			}
			else{
				LERROR("Error " << _ERRNO << " detected in connection.");
				disconnect();
			}
		}

		void ParallelConnection::decodeInitializationRequestMessage(){
            std::vector<char> buffer;
            buffer.resize(sizeof(uint32_t));
            receiveData(_clientSocket, buffer, sizeof(uint32_t), 0);
            uint32_t requesterID = *reinterpret_cast<uint32_t*>(buffer.data());
			printf("InitRequest message received from client %d!\n", requesterID);
            
            //construct init msg
            std::vector<std::string> scripts = OsEng.scriptEngine()->cachedScripts();
            uint16_t numScrips = scripts.size();
            
            //write header
            buffer.clear();
            writeHeader(buffer, MessageTypes::Initialization);
            
            //write number of scripts
            buffer.insert(buffer.end(), reinterpret_cast<char*>(&numScrips), reinterpret_cast<char*>(&numScrips) + sizeof(numScrips));
            
            uint16_t msglen;
            std::vector<std::string>::const_iterator it;
            
            //write all scripts
            for(it = scripts.cbegin();
                it != scripts.cend();
                ++it){
                //write size of script in chars
                msglen = (*it).size();
                buffer.insert(buffer.end(), reinterpret_cast<char*>(msglen), reinterpret_cast<char*>(msglen) + sizeof(msglen));
                buffer.insert(buffer.end(), (*it).begin(), (*it).end());
            }
            
            //send initialization message
            send(_clientSocket, buffer.data(), buffer.size(), 0);
		}

		void ParallelConnection::listenCommunication(){
			
            //create basic buffer for receiving first part of messages
			std::vector<char> buffer;
            //size of the header
			buffer.resize(headerSize());
            
			int result;
            //while we're still connected and listening
			while (_isListening.load()){
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
				}
				else{
					if (result == 0){
						//connection rejected
						_isConnected.store(false);
                        _isListening.store(false);
					}
					else{
						LERROR("Error " << _ERRNO << " detected in connection!");
					}
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
        
        std::string ParallelConnection::port(){
            return _port;
        }
        
        void ParallelConnection::setAddress(const std::string &address){
            _address = address;
        }
        
        std::string ParallelConnection::address(){
            return _address;
        }
        
        void ParallelConnection::setName(const std::string& name){
            _name = name;
        }
        
        std::string ParallelConnection::name(){
            return  _name;
        }
        
        _SOCKET ParallelConnection::clientSocket(){
            return _clientSocket;
        }
        
        bool ParallelConnection::isHost(){
            return _isHost.load();
        }
        
        void ParallelConnection::requestHostship(){
            std::vector<char> buffer;
            buffer.reserve(headerSize());
          
            //write header
            writeHeader(buffer, MessageTypes::HostshipRequest);
            
            //send message
            send(_clientSocket, buffer.data(), buffer.size(), 0);
        }

		void ParallelConnection::setPassword(const std::string& pwd){
			_passCode = hash(pwd);
		}
        
        void ParallelConnection::sendScript(const std::string script){
            uint16_t msglen = static_cast<uint16_t>(script.length());
            std::vector<char> buffer;
            buffer.reserve(headerSize() + sizeof(msglen) + msglen);
            
            //write header
            writeHeader(buffer, MessageTypes::Script);

            //size of message
            buffer.insert(buffer.end(), reinterpret_cast<char*>(&msglen), reinterpret_cast<char*>(&msglen) + sizeof(msglen));
            
            //actual message
            buffer.insert(buffer.end(), script.begin(), script.end());
            
            //send message
            send(_clientSocket, buffer.data(), buffer.size(), 0);
        }


		void ParallelConnection::disconnect(){
            
            //must be run before trying to join communication threads, else the threads are stuck trying to receive data
            closeSocket();
            
            //tell broadcast thread to stop broadcasting
            _isHost.store(false);
            
            //tell connection thread to stop trying to connect and stop listening for communication
            _isConnected.store(true);
            _isListening.store(false);
            
            //join connection thread and delete it
            if (_connectionThread != nullptr){
                _connectionThread->join();
                delete _connectionThread;
                _connectionThread = nullptr;
            }
            
            //join broadcast thread and delete it
            if (_broadcastThread != nullptr){
                _broadcastThread->join();
                delete _broadcastThread;
                _broadcastThread = nullptr;
            }
            
            //join send thread and delete it
            if (_sendThread != nullptr){
                _sendThread->join();
                delete _sendThread;
                _sendThread = nullptr;
            }
            
#if defined(__WIN32__)
            WSACleanup();
#endif
		}

		void ParallelConnection::closeSocket(){
			if (_clientSocket != INVALID_SOCKET)
			{
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
					WSACleanup();
					return false;
				}
			#else
						//No init needed on unix
			#endif

			return true;
		}

		void ParallelConnection::broadcast(){
			
            //while we're still the host
			while (_isHost.load()){

                //create a keyframe with current position and orientation of camera
				network::StreamDataKeyframe kf;
				kf._position = OsEng.interactionHandler()->camera()->position();
				kf._viewRotationQuat = glm::quat_cast(OsEng.interactionHandler()->camera()->viewRotationMatrix());
				
                //timestamp as current runtime of OpenSpace instance
                kf._timeStamp = OsEng.runTime();
				
                //create a buffer for the keyframe
                std::vector<char> kfBuffer;
                
                //fill the keyframe buffer
                kf.serialize(kfBuffer);
                
                //get the size of the keyframebuffer
				uint16_t msglen = static_cast<uint16_t>(kfBuffer.size());
                
                //create the full buffer
				std::vector<char> buffer;
				buffer.reserve(headerSize() + sizeof(msglen) + msglen);
				
				//write header
                writeHeader(buffer, MessageTypes::StreamData);

				//size of message
				buffer.insert(buffer.end(), reinterpret_cast<char*>(&msglen), reinterpret_cast<char*>(&msglen) + sizeof(msglen));

				//actual message
				buffer.insert(buffer.end(), kfBuffer.begin(), kfBuffer.end());

				//send message
				send(_clientSocket, buffer.data(), buffer.size(), 0);

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
                        "",
                        "Request to be the host for this session"
                    },
                }
            };
        }
        
    } // namespace network
    
} // namespace openspace
