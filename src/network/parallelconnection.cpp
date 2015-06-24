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
        _isRunning(false),
        _isHost(false),
        _headerSize(headerSize())
        {
            
        }
        
        ParallelConnection::~ParallelConnection(){
            disconnect();
        }
        
		void ParallelConnection::clientConnect(){
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

			int result;

			// Resolve the local address and port to be used by the server
			result = getaddrinfo(_address.c_str(), _port.c_str(), &hints, &addresult);
			if (result != 0)
			{
			#if defined(__WIN32__)
				WSACleanup();
			#endif
                LERROR("Failed to parse hints for Parallel Connection");
			}

            LINFO("Attempting to connect to address "<< _address << " on port " << _port);

			//start connection thread
			_isRunning.store(true);
			_connectionThread = new (std::nothrow) std::thread(&ParallelConnection::connection, this, addresult);
			
        }

		void ParallelConnection::connection(addrinfo *info){

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
            result = setsockopt(_clientSocket, /* socket affected */
                                IPPROTO_TCP,     /* set option at TCP level */
                                TCP_NODELAY,     /* name of option */
                                (char *)&flag,  /* the cast is historical cruft */
                                sizeof(int));    /* length of option value */
            
            //set send timeout
            int timeout = 0; //infinite
            result = setsockopt(
                                _clientSocket,
                                SOL_SOCKET,
                                SO_SNDTIMEO,
                                (char *)&timeout,
                                sizeof(timeout));
            
            //set receive timeout
            timeout = 0; //infinite
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
			while (_isRunning.load()){
				
                //try to connect
				result = connect(_clientSocket, info->ai_addr, (int)info->ai_addrlen);
                
                //if the connection was successfull
				if (result != SOCKET_ERROR)
				{
					//send authentication
					authenticate();

					//start listening for communication
					//communicate();
				}

				//try to connect once per second
				std::this_thread::sleep_for(std::chrono::seconds(1));
			}
            
            //make sure to join the broadcast thread if started
            //dont delete it, that will be done in disconnect() function
//            if(_broadcastThread != nullptr && _isHost.load()){
//                _isHost.store(false);
//                _broadcastThread->join();
//            }
            
			//cleanup
			freeaddrinfo(info);
		}

		void ParallelConnection::authenticate(){
			uint16_t namelen = static_cast<uint16_t>(_name.length());
			int size = headerSize() + sizeof(uint32_t) + sizeof(uint16_t) + static_cast<int>(namelen);
			std::vector<char> buffer;
			buffer.reserve(size);

			//version
			buffer.insert(buffer.end(), 'O');
			buffer.insert(buffer.end(), 'S');
			buffer.insert(buffer.end(), 0);
			buffer.insert(buffer.end(), 0);

			//msg type, 0 = auth
			int type = MessageTypes::Authentication;
			buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(int));

			//passcode
			buffer.insert(buffer.end(), reinterpret_cast<char*>(&_passCode), reinterpret_cast<char*>(&_passCode) + sizeof(uint32_t));

			//name length
			buffer.insert(buffer.end(), reinterpret_cast<char*>(&namelen), reinterpret_cast<char*>(&namelen) + sizeof(uint16_t));

			//name
			buffer.insert(buffer.end(), _name.begin(), _name.end());

			int result = send(_clientSocket, buffer.data(), size, 0);

			if (result == SOCKET_ERROR){
				//failed to send auth msg.
				std::cerr << "Failed to send authentication message!" << std::endl;
			}
		}

		void ParallelConnection::delegateDecoding(int type){
			switch (type){
			case MessageTypes::Authentication:
				decodeAuthenticationMessage();
				break;
			case MessageTypes::Initialization:
				decodeInitializationMessage();
				break;
			case MessageTypes::StreamData:
				decodeDataMessage();
				break;
            case MessageTypes::Script:
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

		void ParallelConnection::decodeAuthenticationMessage(){
			printf("Auth OK!\n");	//more stuff here later
		}

		void ParallelConnection::decodeInitializationMessage(){
			printf("Init message received!\n");
		}

		void ParallelConnection::decodeDataMessage(){
			int result;
			uint16_t msglen;
			std::vector<char> buffer;
			buffer.resize(sizeof(msglen));
			result = receiveData(_clientSocket, buffer, sizeof(msglen), 0);

			if (result <= 0){
				//error
				return;
			}

			msglen = (*(reinterpret_cast<uint16_t*>(buffer.data())));

			buffer.clear();
			buffer.resize(msglen);

			result = receiveData(_clientSocket, buffer, msglen, 0);
			if (result <= 0){
				//error
				return;
			}
            
            network::StreamDataKeyframe kf;
            kf.deserialize(buffer);
            OsEng.interactionHandler()->addKeyframe(kf);            
		}

        void ParallelConnection::decodeScript(){
            int result;
            uint16_t msglen;
            std::vector<char> buffer;
            buffer.resize(sizeof(msglen));
            result = receiveData(_clientSocket, buffer, sizeof(msglen), 0);
            
            if (result <= 0){
                //error
                return;
            }
            
            msglen = (*(reinterpret_cast<uint16_t*>(buffer.data())));
            
            buffer.clear();
            buffer.resize(msglen);
            
            result = receiveData(_clientSocket, buffer, msglen, 0);
            if (result <= 0){
                //error
                return;
            }
            
            std::string script(buffer.data());
            OsEng.scriptEngine()->queueScript(script);
        }
        
		void ParallelConnection::decodeHostInfoMessage(){
			std::vector<char> hostflag;
			hostflag.resize(1);
			int result = receiveData(_clientSocket, hostflag, 1, 0);

			if (result > 0){
				if (hostflag.at(0) == 1){
					//we're already host, do nothing (dummy check)
					if (_isHost.load()){
						return;
					}
					else{
						//start broadcasting
						_isHost.store(true);
						_broadcastThread = new (std::nothrow) std::thread(&ParallelConnection::broadcast, this);
					}
				}
				else{
					//we were broadcasting but should stop now
					if (_isHost.load()){
						_isHost.store(false);
						if (_broadcastThread != nullptr){
							_broadcastThread->join();
							delete _broadcastThread;
							_broadcastThread = nullptr;
						}
					}
					else{
						//we were not host so nothing to do
					}
                    
                    //request init packages from the host
                    int size = headerSize() + sizeof(uint32_t);
                    std::vector<char> buffer;
                    buffer.reserve(size);
                    
                    //version
                    buffer.insert(buffer.end(), 'O');
                    buffer.insert(buffer.end(), 'S');
                    buffer.insert(buffer.end(), 0);
                    buffer.insert(buffer.end(), 0);
                    
                    //msg type, 0 = auth
                    int type = MessageTypes::InitializationRequest;
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(int));
                    
                    //send message
                    send(_clientSocket, buffer.data(), buffer.size(), 0);

				}
			}
			else{
				std::cerr << "Error " << _ERRNO << " detected in connection!" << std::endl;
				disconnect();
			}
		}

		void ParallelConnection::decodeInitializationRequestMessage(){
			printf("InitRequest message received!\n");
		}

		void ParallelConnection::communicate(){
			
			std::vector<char> buffer;
			buffer.resize(8);
			int result;

			while (_isRunning.load()){
				result = receiveData(_clientSocket, buffer, headerSize(), 0);

				if (result > 0){
					if (buffer[0] == 'O' && //Open
						buffer[1] == 'S' && //Space
						buffer[2] == 0 && //version
						buffer[3] == 0 //version
						)
					{
						//parse type
						int type = (*(reinterpret_cast<int*>(&buffer[4])));
						delegateDecoding(type);
					}
				}
				else{
					if (result == 0){
						//connection rejected
						_isRunning.store(false);
					}
					else{
						std::cerr << "Error " << _ERRNO << " detected in connection!" << std::endl;
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
        
        void ParallelConnection::setSocket(_SOCKET socket){
            _clientSocket = socket;
        }
        
        _SOCKET ParallelConnection::clientSocket(){
            return _clientSocket;
        }
        
        void ParallelConnection::setHost(bool host){
            _isHost.store(host);
        }
        
        bool ParallelConnection::isHost(){
            return _isHost.load();
        }
        
        bool ParallelConnection::isRunning(){
            return _isRunning.load();
        }
        
        void ParallelConnection::requestHostship(){
            std::vector<char> buffer;
            buffer.reserve(headerSize() + sizeof(int));
            //header
            buffer.insert(buffer.end(), 'O');
            buffer.insert(buffer.end(), 'S');
            buffer.insert(buffer.end(), 0);
            buffer.insert(buffer.end(), 0);
            
            //type of message
            int type = ParallelConnection::MessageTypes::HostshipRequest;
            buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(type));
            
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
            
            //header
            buffer.insert(buffer.end(), 'O');
            buffer.insert(buffer.end(), 'S');
            buffer.insert(buffer.end(), 0);
            buffer.insert(buffer.end(), 0);
            
            //type of message
            int type = ParallelConnection::MessageTypes::Script;
            buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(type));
            
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
            
            _isRunning.store(false);
            _isHost.store(false);
            if (_connectionThread != nullptr){
                _connectionThread->join();
                delete _connectionThread;
                _connectionThread = nullptr;
            }
            
            if (_broadcastThread != nullptr){
                _broadcastThread->join();
                delete _broadcastThread;
                _broadcastThread = nullptr;
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
					std::cerr << "Failed to init winsock API!" << std::endl;
					WSACleanup();
					return false;
				}
			#else
						//No init needed on unix
			#endif

			return true;
		}

		void ParallelConnection::broadcast(){
			
			while (_isHost.load()){

				network::StreamDataKeyframe kf;
				kf._position = OsEng.interactionHandler()->camera()->position();
				kf._viewRotationQuat = glm::quat_cast(OsEng.interactionHandler()->camera()->viewRotationMatrix());
				
				//@TODO, implement method in openspace engine for this
				kf._timeStamp = sgct::Engine::getTime();
				

                std::vector<char> kfBuffer;
                kf.serialize(kfBuffer);
                
				uint16_t msglen = static_cast<uint16_t>(kfBuffer.size());
				std::vector<char> buffer;
				buffer.reserve(headerSize() + sizeof(msglen) + msglen);
				
				//header
				buffer.insert(buffer.end(), 'O');
				buffer.insert(buffer.end(), 'S');
				buffer.insert(buffer.end(), 0);
				buffer.insert(buffer.end(), 0);

				//type of message
				int type = ParallelConnection::MessageTypes::StreamData;
				buffer.insert(buffer.end(), reinterpret_cast<char*>(&type), reinterpret_cast<char*>(&type) + sizeof(type));

				//size of message
				buffer.insert(buffer.end(), reinterpret_cast<char*>(&msglen), reinterpret_cast<char*>(&msglen) + sizeof(msglen));

				//actual message
				buffer.insert(buffer.end(), kfBuffer.begin(), kfBuffer.end());

				//send message
				send(_clientSocket, buffer.data(), buffer.size(), 0);

				//100 ms sleep
				std::this_thread::sleep_for(std::chrono::milliseconds(100));
			}
		}
        
        void ParallelConnection::writeHeader(std::vector<char> &buffer){
            //make sure the buffer is large enough to hold at least the header
            if(buffer.size() < headerSize()){
                buffer.reserve(headerSize());
            }
            
            //get the current running version of openspace
            uint8_t versionMajor = static_cast<uint8_t>(OPENSPACE_VERSION_MAJOR);
            uint8_t versionMinor = static_cast<uint8_t>(OPENSPACE_VERSION_MINOR);
            
            //insert header into buffer
            buffer.insert(buffer.begin(), 'O');
            buffer.insert(buffer.begin(), 'S');
            buffer.insert(buffer.begin(), static_cast<char>(versionMajor));
            buffer.insert(buffer.begin(), static_cast<char>(versionMinor));
        }
        
        int ParallelConnection::headerSize(){
            //minor and major version (as uint8_t) + two bytes for the chars 'O' and 'S'
            return 2 * sizeof(uint8_t) + 2;
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
