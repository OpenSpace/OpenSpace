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

#ifndef __OSPARALLELCONNECTION_H__
#define __OSPARALLELCONNECTION_H__

//openspace includes
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/scripting/scriptengine.h>

//std includes
#include <string>
#include <vector>
#include <atomic>
#include <thread>

#ifdef __WIN32__
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#endif

#if defined(__WIN32__) || defined(__MING32__) || defined(__MING64__)
typedef size_t _SOCKET;
#else
typedef int _SOCKET;
#include <netdb.h>
#endif

namespace openspace{
    
    namespace network{
        
        class OSParallelConnection : public properties::PropertyOwner {
        public:
            
            OSParallelConnection();
            
            ~OSParallelConnection();
            
            void connect();
            
            void setPort(const std::string &port);
            
            std::string port();
            
            void setAddress(const std::string &address);
            
            std::string address();
            
            void setName(const std::string& name);
            
            std::string name();
            
            void setSocket(_SOCKET socket);
            
            _SOCKET socket();
            
            void setHost(bool host);
            
            bool isHost();
            
            bool isRunning();
            
            void requestHostship();
            
            enum MessageTypes{
                Authentication=0,
                Initialization,
                Data,
                HostInfo,
                InitRequest
            };
            
            /**
             * Returns the Lua library that contains all Lua functions available to affect the
             * remote OS parallel connection. The functions contained are
             * -
             * \return The Lua library that contains all Lua functions available to affect the
             * interaction
             */
            static scripting::ScriptEngine::LuaLibrary luaLibrary();
            
        protected:
            
        private:
            uint32_t _passCode;
            std::string _password;
            std::string _port;
            std::string _address;
            std::string _name;
            _SOCKET _clientSocket;
            std::thread *_thread;
            std::atomic<bool> _isRunning;
            std::atomic<bool> _isHost;
        };
    } // namespace network
    
} // namespace openspace

#endif // __OSPARALLELCONNECTION_H__