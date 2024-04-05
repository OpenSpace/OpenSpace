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

#include <modules/server/include/serverinterface.h>
#include <ghoul/io/socket/socketserver.h>

#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/io/socket/websocketserver.h>
#include <functional>

namespace {
    constexpr std::string_view KeyIdentifier = "Identifier";
    constexpr std::string_view TcpSocketType = "TcpSocket";
    constexpr std::string_view WebSocketType = "WebSocket";
    constexpr std::string_view DenyAccess = "Deny";
    constexpr std::string_view RequirePassword = "RequirePassword";
    constexpr std::string_view AllowAccess = "Allow";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "This setting determines whether this server interface is enabled or not",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "Whether the interface is using a Socket or a WebSocket",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The network port to use for this sevrer interface",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultAccessInfo = {
        "DefaultAccess",
        "Default Access",
        "Sets the default access policy: Allow, RequirePassword or Deny",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AllowAddressesInfo = {
        "AllowAddresses",
        "Allow Addresses",
        "IP addresses or domains that should always be allowed access to this interface",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        RequirePasswordAddressesInfo = {
        "RequirePasswordAddresses",
        "Require Password Addresses",
        "IP addresses or domains that should be allowed access if they provide a "
        "password",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DenyAddressesInfo = {
        "DenyAddresses",
        "Deny Addresses",
        "IP addresses or domains that should never be allowed access to this interface",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PasswordInfo = {
        "Password",
        "Password",
        "Password for connecting to this interface",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

std::unique_ptr<ServerInterface> ServerInterface::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    // TODO: Use documentation to verify dictionary
    auto si = std::make_unique<ServerInterface>(dictionary);
    return si;
}

ServerInterface::ServerInterface(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "", "", "" })
    , _socketType(TypeInfo)
    , _port(PortInfo, 0)
    , _enabled(EnabledInfo)
    , _allowAddresses(AllowAddressesInfo)
    , _requirePasswordAddresses(RequirePasswordAddressesInfo)
    , _denyAddresses(DenyAddressesInfo)
    , _defaultAccess(DefaultAccessInfo)
    , _password(PasswordInfo)
{

    _socketType.addOption(
        static_cast<int>(InterfaceType::TcpSocket),
        std::string(TcpSocketType)
    );
    _socketType.addOption(
        static_cast<int>(InterfaceType::WebSocket),
        std::string(WebSocketType)
    );

    _defaultAccess.addOption(
        static_cast<int>(Access::Deny),
        std::string(DenyAccess)
    );
    _defaultAccess.addOption(
        static_cast<int>(Access::RequirePassword),
        std::string(RequirePassword)
    );
    _defaultAccess.addOption(
        static_cast<int>(Access::Allow),
        std::string(AllowAccess)
    );

    if (dictionary.hasKey(DefaultAccessInfo.identifier)) {
        const std::string access = dictionary.value<std::string>(
            DefaultAccessInfo.identifier
        );
        if (access == DenyAccess) {
            _defaultAccess.setValue(static_cast<int>(Access::Deny));
        }
        else if (access == RequirePassword) {
            _defaultAccess.setValue(static_cast<int>(Access::RequirePassword));
        }
        else if (access == AllowAccess) {
            _defaultAccess.setValue(static_cast<int>(Access::Allow));
        }
    }

    const std::string identifier = dictionary.value<std::string>(KeyIdentifier);

    auto readList =
        [dictionary](const std::string& key, properties::StringListProperty& list) {
            if (dictionary.hasValue<ghoul::Dictionary>(key)) {
                const ghoul::Dictionary& dict = dictionary.value<ghoul::Dictionary>(key);
                std::vector<std::string> v;
                for (const std::string_view k : dict.keys()) {
                    v.push_back(dict.value<std::string>(k));
                }
                list = v;
            }
        };

    readList(AllowAddressesInfo.identifier, _allowAddresses);
    readList(DenyAddressesInfo.identifier, _denyAddresses);
    readList(RequirePasswordAddressesInfo.identifier, _requirePasswordAddresses);

    setIdentifier(identifier);
    setGuiName(identifier);
    setDescription("Settings for server interface " + identifier);

    const std::string type = dictionary.value<std::string>(TypeInfo.identifier);
    if (type == TcpSocketType) {
        _socketType = static_cast<int>(InterfaceType::TcpSocket);
    }
    else if (type == WebSocketType) {
        _socketType = static_cast<int>(InterfaceType::WebSocket);
    }

    if (dictionary.hasValue<std::string>(PasswordInfo.identifier)) {
        _password = dictionary.value<std::string>(PasswordInfo.identifier);
    }

    _port = static_cast<int>(dictionary.value<double>(PortInfo.identifier));
    _enabled = dictionary.value<bool>(EnabledInfo.identifier);

    auto reinitialize = [this]() {
        deinitialize();
        initialize();
    };

    _socketType.onChange(reinitialize);
    _port.onChange(reinitialize);
    _enabled.onChange(reinitialize);
    _defaultAccess.onChange(reinitialize);
    _allowAddresses.onChange(reinitialize);
    _requirePasswordAddresses.onChange(reinitialize);
    _denyAddresses.onChange(reinitialize);

    addProperty(_socketType);
    addProperty(_port);
    addProperty(_enabled);
    addProperty(_defaultAccess);
    addProperty(_allowAddresses);
    addProperty(_requirePasswordAddresses);
    addProperty(_denyAddresses);
    addProperty(_password);
}

void ServerInterface::initialize() {
    if (!_enabled) {
        return;
    }
    switch (static_cast<InterfaceType>(_socketType.value())) {
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
    const Access access = static_cast<Access>(_defaultAccess.value());
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
    const Access access = static_cast<Access>(_defaultAccess.value());
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

} // namespace openspace
