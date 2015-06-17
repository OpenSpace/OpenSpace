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

//@TODO CHANGE THIS!
const int headerSize = 8;

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

#include <openspace/network/osparallelconnection.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/time.h>

#include "osparallelconnection_lua.inl"

namespace{
	const std::string _loggerCat = "Parallel";
}

namespace openspace {
    namespace network{
        
        OSParallelConnection::OSParallelConnection():
        _passCode(0),
        _port("20501"),
        _address("127.0.0.1"),
        _name("Local Connection"),
        _clientSocket(INVALID_SOCKET),
		_connectionThread(nullptr),
		_broadcastThread(nullptr),
        _isRunning(false),
        _isHost(false)
        {
            
        }
        
        OSParallelConnection::~OSParallelConnection(){
			_isRunning.store(false);

			disconnect();

			#if defined(__WIN32__)
						WSACleanup();
			#endif
        }
        
		void OSParallelConnection::clientConnect(){
			if (!initNetworkAPI()){
				//error, handle this
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
				std::cerr << "Failed to parse hints for connection!" << std::endl;
			}

			// Attempt to connect to the first address returned by
			// the call to getaddrinfo
			ptr = addresult;

			std::cout << "Client started on port " << _port << std::endl;

			//start accept connections thread
			_isRunning.store(true);
			_connectionThread = new (std::nothrow) std::thread(&OSParallelConnection::connection, this, addresult);
			
        }

		void OSParallelConnection::connection(addrinfo *info){
			int result;

			while (_isRunning.load()){
				_clientSocket = socket(info->ai_family, info->ai_socktype, info->ai_protocol);

				if (_clientSocket == INVALID_SOCKET){
					freeaddrinfo(info);
					#if defined(__WIN32__)
						WSACleanup();
					#endif
					std::cerr << "Failed to init client socket!" << std::endl;
					return;
				}

				int flag = 1;
				int result;

				//set send timeout
				int timeout = 0; //infinite
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
					std::cout << "Failed to set reuse address with error:" << _ERRNO << std::endl;

				result = setsockopt(_clientSocket, SOL_SOCKET, SO_KEEPALIVE, (char*)&flag, sizeof(int));
				if (result == SOCKET_ERROR)
					std::cout << "Failed to set keep alive with error: " << _ERRNO << std::endl;

				result = connect(_clientSocket, info->ai_addr, (int)info->ai_addrlen);
				if (result != SOCKET_ERROR)
				{
					//send authentication
					authenticate();

					//start listening for communication
					communicate();
				}

				//one sec sleep
				std::this_thread::sleep_for(std::chrono::seconds(1));
			}

			//cleanup
			freeaddrinfo(info);
		}

		void OSParallelConnection::authenticate(){
			int pos = 4;
			uint16_t namelen = static_cast<uint16_t>(_name.length());
			int size = headerSize + sizeof(uint32_t) + sizeof(uint16_t) + static_cast<int>(namelen);
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

		void OSParallelConnection::delegateDecoding(int type){
			switch (type){
			case MessageTypes::Authentication:
				decodeAuthenticationMessage();
				break;
			case MessageTypes::Initialization:
				decodeInitializationMessage();
				break;
			case MessageTypes::Data:
				decodeDataMessage();
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

		void OSParallelConnection::decodeAuthenticationMessage(){
			printf("Auth OK!\n");	//more stuff here later
		}

		void OSParallelConnection::decodeInitializationMessage(){
			printf("Init message received!\n");
		}

		void OSParallelConnection::decodeDataMessage(){

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
            
            network::Keyframe kf;
            kf.deserialize(buffer);
//            printf("--- %f ---\n", kf._timeStamp);
            OsEng.interactionHandler()->addKeyframe(kf);
            
		}

		void OSParallelConnection::decodeHostInfoMessage(){
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
						_broadcastThread = new (std::nothrow) std::thread(&OSParallelConnection::broadcast, this);
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
				}
			}
			else{
				std::cerr << "Error " << _ERRNO << " detected in connection!" << std::endl;
				disconnect();
			}
		}

		void OSParallelConnection::decodeInitializationRequestMessage(){
			printf("InitRequest message received!\n");
		}

		void OSParallelConnection::communicate(){
			
			std::vector<char> buffer;
			buffer.resize(8);
			int result;

			while (_isRunning.load()){
				result = receiveData(_clientSocket, buffer, headerSize, 0);

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

		int OSParallelConnection::receiveData(_SOCKET & socket, std::vector<char> &buffer, int length, int flags){
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
        
        void OSParallelConnection::setPort(const std::string  &port){
            _port = port;
        }
        
        std::string OSParallelConnection::port(){
            return _port;
        }
        
        void OSParallelConnection::setAddress(const std::string &address){
            _address = address;
        }
        
        std::string OSParallelConnection::address(){
            return _address;
        }
        
        void OSParallelConnection::setName(const std::string& name){
            _name = name;
        }
        
        std::string OSParallelConnection::name(){
            return  _name;
        }
        
        void OSParallelConnection::setSocket(_SOCKET socket){
            _clientSocket = socket;
        }
        
        _SOCKET OSParallelConnection::clientSocket(){
            return _clientSocket;
        }
        
        void OSParallelConnection::setHost(bool host){
            _isHost.store(host);
        }
        
        bool OSParallelConnection::isHost(){
            return _isHost.load();
        }
        
        bool OSParallelConnection::isRunning(){
            return _isRunning.load();
        }
        
        void OSParallelConnection::requestHostship(){
            
        }

		void OSParallelConnection::setPassword(const std::string& pwd){
			_passCode = hash(pwd);
		}

		void OSParallelConnection::disconnect(){
			_isHost.store(false);

			if (_broadcastThread != nullptr){
				_broadcastThread->join();
				delete _broadcastThread;
				_broadcastThread = nullptr;
			}

			_isRunning.store(false);

			if (_connectionThread != nullptr){
				_connectionThread->join();
				delete _connectionThread;
				_connectionThread = nullptr;
			}

			closeSocket();
		}

		void OSParallelConnection::closeSocket(){
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

		bool OSParallelConnection::initNetworkAPI(){
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

		void OSParallelConnection::broadcast(){
			
			while (_isHost.load()){

				network::Keyframe kf;
				kf._position = OsEng.interactionHandler()->camera()->position();
				kf._viewRotationQuat = glm::quat_cast(OsEng.interactionHandler()->camera()->viewRotationMatrix());
				kf._timeStamp = Time::ref().currentTime();

                std::vector<char> kfBuffer;
                kf.serialize(kfBuffer);
                
				uint16_t msglen = static_cast<uint16_t>(kfBuffer.size());
				std::vector<char> buffer;
				buffer.reserve(headerSize + sizeof(msglen) + msglen);
				
				//header
				buffer.insert(buffer.end(), 'O');
				buffer.insert(buffer.end(), 'S');
				buffer.insert(buffer.end(), 0);
				buffer.insert(buffer.end(), 0);

				//type of message
				int type = OSParallelConnection::MessageTypes::Data;
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

        scripting::ScriptEngine::LuaLibrary OSParallelConnection::luaLibrary() {
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
                }
            };
        }
        
    } // namespace network
    
} // namespace openspace
