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

#ifndef __OPENSPACE_MODULE_OMNI___SCENARIO___H__
#define __OPENSPACE_MODULE_OMNI___SCENARIO___H__

#include <openspace/json.h>
#include <ghoul/io/socket/tcpsocket.h>

namespace openspace::omni {

class Scenario {
public:
    Scenario(const std::string& identifier);
    virtual ~Scenario() = default;
    void initialize(std::shared_ptr<ghoul::io::TcpSocket> socket);

    void sendMessage(const std::string& message) const;
    void sendJson(const nlohmann::json& json) const;

    virtual void handleMessage(const nlohmann::json& obj) = 0;
    virtual void onHandleMessage() = 0;

    void enableScenario();
    void disableScenario();

    bool isActive() const;

    std::string_view identifier() const;

protected:
    virtual void onEnableScenario() = 0;
    virtual void onDisableScenario() = 0;

private:
    std::shared_ptr<ghoul::io::TcpSocket> _socket;
    const std::string _identifier;

    bool _isActive = false;

};

} // namespace openspace::omni

#endif // __OPENSPACE_MODULE_OMNI___SCENARIO___H__
