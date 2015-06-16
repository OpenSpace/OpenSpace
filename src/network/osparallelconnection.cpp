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

#include <openspace/network/osparallelconnection.h>
#include <openspace/engine/openspaceengine.h>

#include "osparallelconnection_lua.inl"

namespace openspace {
    namespace network{
        
        OSParallelConnection::OSParallelConnection():
        _passCode(0),
        _port(""),
        _address(""),
        _name("No name"),
        _password(""),
        _clientSocket(0),
        _thread(nullptr),
        _isRunning(false),
        _isHost(false)
        {
            
        }
        
        OSParallelConnection::~OSParallelConnection(){
            
        }
        
        void OSParallelConnection::connect(){
            
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
        
        _SOCKET OSParallelConnection::socket(){
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
        
        scripting::ScriptEngine::LuaLibrary OSParallelConnection::luaLibrary() {
            return {
                "",
                {
                    {
                        "setPort",
                        &luascriptfunctions::setPort,
                        "string",
                        "Set the port for the parallel connection"
                    },
//                    {
//                        "clearKeys",
//                        &luascriptfunctions::clearKeys,
//                        "",
//                        "Clear all key bindings"
//                    },
//                    {
//                        "bindKey",
//                        &luascriptfunctions::bindKey,
//                        "string, string",
//                        "Binds a key by name to a lua string command"
//                    },
//                    {
//                        "dt",
//                        &luascriptfunctions::dt,
//                        "",
//                        "Get current frame time"
//                    },
//                    {
//                        "distance",
//                        &luascriptfunctions::distance,
//                        "number",
//                        "Change distance to origin"
//                    },
//                    {
//                        "setInteractionSensitivity",
//                        &luascriptfunctions::setInteractionSensitivity,
//                        "number",
//                        "Sets the global interaction sensitivity"
//                    },
//                    {
//                        "interactionSensitivity",
//                        &luascriptfunctions::interactionSensitivity,
//                        "",
//                        "Gets the current global interaction sensitivity"
//                    },
//                    {
//                        "setInvertRoll",
//                        &luascriptfunctions::setInvertRoll,
//                        "bool",
//                        "Sets the setting if roll movements are inverted"
//                    },
//                    {
//                        "invertRoll",
//                        &luascriptfunctions::invertRoll,
//                        "",
//                        "Returns the status of roll movement inversion"
//                    },
//                    {
//                        "setInvertRotation",
//                        &luascriptfunctions::setInvertRotation,
//                        "bool",
//                        "Sets the setting if rotation movements are inverted"
//                    },
//                    {
//                        "invertRotation",
//                        &luascriptfunctions::invertRotation,
//                        "",
//                        "Returns the status of rotation movement inversion"
//                    }
                    
                }
            };
        }
        
    } // namespace network
    
} // namespace openspace
