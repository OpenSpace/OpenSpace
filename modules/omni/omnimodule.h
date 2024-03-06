/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_OMNI___OMNIMODULE___H__
#define __OPENSPACE_MODULE_OMNI___OMNIMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <modules/omni/include/poll.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/io/socket/tcpsocket.h>

#include <deque>
#include <mutex>
#include <set>
#include <websocketpp/config/core.hpp>
#include <websocketpp/server.hpp>

namespace ghoul::io { class Socket; }

namespace openspace::omni {
    enum class Type {
        ServerConnect = 0,
        ServerDisconnect,
        ServerAuthorized,
        Token,
        ServerCode,
        ServerJoin,
        ServerLeave,
        ServerError,
        OpenSpaceType
    };


    std::string_view toString(Type type);
    Type fromString(std::string_view str);

} // namespace openspace::omni

namespace openspace {

class OmniModule : public OpenSpaceModule {
public:

    constexpr static const char* Name = "Omni";

    OmniModule();
    ~OmniModule() override;

    void internalInitialize(const ghoul::Dictionary& config) override;

    void addScenario(std::unique_ptr<omni::Scenario> scenario);
    void enableScenario(std::string_view identifier);
    void disableScenario(std::string_view identifier);

    void sendMessage(const std::string& message);
    void sendJson(const nlohmann::json& json);

    scripting::LuaLibrary luaLibrary() const override;
    std::vector<documentation::Documentation> documentations() const override;

private:
    void preSync();
    void handleConnection();
    void handleJson(const nlohmann::json& json);
    void userJoin(const nlohmann::json& json);
    void userLeave(const nlohmann::json& json);


    websocketpp::server<websocketpp::config::core> _server;

    std::shared_ptr<ghoul::io::TcpSocket> _socket;
    std::thread _thread;

    std::mutex _messageQueueMutex;
    std::deque<std::string> _messageQueue;

    std::string _serverCode;

    std::set<int> _users;


    std::vector<std::unique_ptr<omni::Scenario>> _scenarios;
    omni::Scenario* _activeScenario = nullptr;
};

} // namespace openspace

#endif //__OPENSPACE_MODULE_OMNI___OMNIMODULE___H__
