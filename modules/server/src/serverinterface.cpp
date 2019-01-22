/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/server/include/serverinterface.h>
#include <ghoul/io/socket/socketserver.h>

#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/io/socket/websocketserver.h>

namespace {

    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* TcpSocketType = "TcpSocket";
    constexpr const char* WebSocketType = "WebSocket";
    constexpr const char* DenyAccess = "Deny";
    constexpr const char* RequirePassword = "RequirePassword";
    constexpr const char* AllowAccess = "Allow";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this server interface is enabled or not."
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "Whether the interface is using a Socket or a WebSocket"
    };

    constexpr openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The network port to use for this sevrer interface"
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultAccessInfo = {
        "DefaultAccess",
        "Default Access",
        "Sets the default access policy: Allow, RequirePassword or Deny"
    };

    constexpr openspace::properties::Property::PropertyInfo AllowAddressesInfo = {
        "AllowAddresses",
        "Allow Addresses",
        "Ip addresses or domains that should always be allowed access to this interface"
    };

    constexpr openspace::properties::Property::PropertyInfo RequirePasswordAddressesInfo = {
        "RequirePasswordAddresses",
        "Require Password Addresses",
        "Ip addresses or domains that should be allowed access if they provide a password"
    };

    constexpr openspace::properties::Property::PropertyInfo DenyAddressesInfo = {
        "DenyAddresses",
        "Deny Addresses",
        "Ip addresses or domains that should never be allowed access to this interface"
    };

    constexpr openspace::properties::Property::PropertyInfo PasswordInfo = {
        "Password",
        "Password",
        "Password for connecting to this interface"
    };
}

namespace openspace {

std::unique_ptr<ServerInterface> ServerInterface::createFromDictionary(
                                                          const ghoul::Dictionary& config)
{
    // TODO: Use documentation to verify dictionary
    std::unique_ptr<ServerInterface> si = std::make_unique<ServerInterface>(config);
    return si;
}

ServerInterface::ServerInterface(const ghoul::Dictionary& config) 
    : properties::PropertyOwner({ "", "", "" })
    , _type(TypeInfo)
    , _port(PortInfo, 0)
    , _enabled(EnabledInfo)
    , _allowAddresses(AllowAddressesInfo)
    , _requirePasswordAddresses(RequirePasswordAddressesInfo)
    , _denyAddresses(DenyAddressesInfo)
    , _defaultAccess(DefaultAccessInfo)
    , _password(PasswordInfo)
{

    _type.addOption(static_cast<int>(InterfaceType::TcpSocket), TcpSocketType);
    _type.addOption(static_cast<int>(InterfaceType::WebSocket), WebSocketType);

    _defaultAccess.addOption(static_cast<int>(Access::Deny), DenyAccess);
    _defaultAccess.addOption(static_cast<int>(Access::RequirePassword), RequirePassword);
    _defaultAccess.addOption(static_cast<int>(Access::Allow), AllowAccess);

    const std::string identifier = config.value<std::string>(KeyIdentifier);

    auto readList =
        [config](const std::string& key, properties::StringListProperty& list) {
            if (config.hasValue<ghoul::Dictionary>(key)) {
                const ghoul::Dictionary& dict = config.value<ghoul::Dictionary>(key);
                std::vector<std::string> v;
                for (const std::string& k : dict.keys()) {
                    v.push_back(dict.value<std::string>(k));
                }
                list.set(v);
            }
        };
    
    readList(AllowAddressesInfo.identifier, _allowAddresses);
    readList(DenyAddressesInfo.identifier, _denyAddresses);
    readList(RequirePasswordAddressesInfo.identifier, _requirePasswordAddresses);

    this->setIdentifier(identifier);
    this->setGuiName(identifier);
    this->setDescription("Settings for server interface " + identifier);

    const std::string type = config.value<std::string>(TypeInfo.identifier);
    if (type == TcpSocketType) {
        _type = static_cast<int>(InterfaceType::TcpSocket);
    } else if (type == WebSocketType) {
        _type = static_cast<int>(InterfaceType::WebSocket);
    }

    if (config.hasValue<std::string>(PasswordInfo.identifier)) {
        _password = config.value<std::string>(PasswordInfo.identifier);
    }

    _port = static_cast<int>(config.value<double>(PortInfo.identifier));
    _enabled = config.value<bool>(EnabledInfo.identifier);

    std::function<void()> reinitialize = [this]() {
        deinitialize();
        initialize();
    };

    _type.onChange(reinitialize);
    _port.onChange(reinitialize);
    _enabled.onChange(reinitialize);
    _defaultAccess.onChange(reinitialize);
    _allowAddresses.onChange(reinitialize);
    _requirePasswordAddresses.onChange(reinitialize);
    _denyAddresses.onChange(reinitialize);

    addProperty(_type);
    addProperty(_port);
    addProperty(_enabled);
    addProperty(_defaultAccess);
    addProperty(_allowAddresses);
    addProperty(_requirePasswordAddresses);
    addProperty(_denyAddresses);
    addProperty(_password);
}

ServerInterface::~ServerInterface() {}

void ServerInterface::initialize() {
    if (!_enabled) {
        return;
    }
    switch (static_cast<InterfaceType>(_type.value())) {
    case InterfaceType::TcpSocket:
        _socketServer = std::make_unique<ghoul::io::TcpSocketServer>();
        break;
    case InterfaceType::WebSocket:
        _socketServer = std::make_unique<ghoul::io::WebSocketServer>();
        break;
    }
    _socketServer->listen(_port);
}

void ServerInterface::deinitialize() {
    _socketServer->close();
}

bool ServerInterface::isEnabled() const {
    return _enabled.value();
}

bool ServerInterface::isActive() const {
    return _socketServer->isListening();
}

int ServerInterface::port() const {
    return _port;
}

std::string ServerInterface::password() const {
    return _password;
}

bool ServerInterface::clientHasAccessWithoutPassword(
    const std::string& clientAddress) const
{
    for (const std::string& address : _allowAddresses.value()) {
        if (clientAddress == address) {
            return true;
        }
    }
    Access access = static_cast<Access>(_defaultAccess.value());
    if (access == Access::Allow) {
        for (const std::string& address : _denyAddresses.value()) {
            if (clientAddress == address) {
                return false;
            }
        }
        return true;
    }
    return false;
}

bool ServerInterface::clientIsBlocked(const std::string& clientAddress) const {
    for (const std::string& address : _denyAddresses.value()) {
        if (clientAddress == address) {
            return true;
        }
    }
    Access access = static_cast<Access>(_defaultAccess.value());
    if (access == Access::Deny) {
        for (const std::string& address : _allowAddresses.value()) {
            if (clientAddress == address) {
                return false;
            }
        }
        for (const std::string& address : _requirePasswordAddresses.value()) {
            if (clientAddress == address) {
                return false;
            }
        }
        return true;
    }
    return false;
}

ghoul::io::SocketServer* ServerInterface::server() {
    return _socketServer.get();
}


}
