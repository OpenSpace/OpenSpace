/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_SERVER___SERVERINTERFACE___H__
#define __OPENSPACE_MODULE_SERVER___SERVERINTERFACE___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>

namespace ghoul::io { class SocketServer; }

namespace openspace {

class ServerInterface : public properties::PropertyOwner {
public:
    static std::unique_ptr<ServerInterface> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    explicit ServerInterface(const ghoul::Dictionary& dictionary);
    virtual ~ServerInterface() override = default;

    void initialize();
    void deinitialize();
    bool isEnabled() const;
    bool isActive() const;
    int port() const;
    std::string password() const;
    bool clientHasAccessWithoutPassword(const std::string& address) const;
    bool clientIsBlocked(const std::string& address) const;

    ghoul::io::SocketServer* server();

private:
    enum class InterfaceType : int {
        TcpSocket = 0,
        WebSocket
    };

    enum class Access : int {
        Deny = 0,
        RequirePassword,
        Allow
    };

    properties::OptionProperty _socketType;
    properties::IntProperty _port;
    properties::BoolProperty _enabled;
    properties::StringListProperty _allowAddresses;
    properties::StringListProperty _requirePasswordAddresses;
    properties::StringListProperty _denyAddresses;
    properties::OptionProperty _defaultAccess;
    properties::StringProperty _password;

    std::unique_ptr<ghoul::io::SocketServer> _socketServer;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___SERVERINTERFACE___H__
