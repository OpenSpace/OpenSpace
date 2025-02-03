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

#include <modules/server/include/serverinterface.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/io/socket/socketserver.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/io/socket/websocketserver.h>
#include <functional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "This setting determines whether this server interface is enabled or not.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "Whether the interface is using a Socket or a WebSocket.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The network port to use for this sevrer interface.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultAccessInfo = {
        "DefaultAccess",
        "Default Access",
        "Sets the default access policy: Allow, RequirePassword or Deny.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AllowAddressesInfo = {
        "AllowAddresses",
        "Allow Addresses",
        "IP addresses or domains that should always be allowed access to this interface.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        RequirePasswordAddressesInfo = {
        "RequirePasswordAddresses",
        "Require Password Addresses",
        "IP addresses or domains that should be allowed access if they provide a "
        "password.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DenyAddressesInfo = {
        "DenyAddresses",
        "Deny Addresses",
        "IP addresses or domains that should never be allowed access to this interface.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PasswordInfo = {
        "Password",
        "Password",
        "Password for connecting to this interface.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(ServerInterface)]] Parameters {
        enum class [[codegen::stringify()]] Access {
            Allow,
            Deny,
            RequirePassword
        };

        // [[codegen::verbatim(DefaultAccessInfo.description)]]
        std::optional<Access> defaultAccess;

        std::string identifier;

        // [[codegen::verbatim(AllowAddressesInfo.description)]]
        std::optional<std::vector<std::string>> allowAddresses;

        // [[codegen::verbatim(DenyAddressesInfo.description)]]
        std::optional<std::vector<std::string>> denyAddresses;

        // [[codegen::verbatim(RequirePasswordAddressesInfo.description)]]
        std::optional<std::vector<std::string>> requirePasswordAddresses;

        enum class [[codegen::stringify()]] Type {
            TcpSocket,
            WebSocket
        };
        // [[codegen::verbatim(TypeInfo.description)]]
        Type type;

        // [[codegen::verbatim(PasswordInfo.description)]]
        std::optional<std::string> password;

        // [[codegen::verbatim(PortInfo.description)]]
        int port;

        // [[codegen::verbatim(EnabledInfo.description)]]
        bool enabled;
    };
#include "serverinterface_codegen.cpp"
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
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _socketType.addOption(
        static_cast<int>(InterfaceType::TcpSocket),
        "TcpSocket"
    );
    _socketType.addOption(
        static_cast<int>(InterfaceType::WebSocket),
        "WebSocket"
    );

    _defaultAccess.addOption(
        static_cast<int>(Access::Deny),
        "Deny"
    );
    _defaultAccess.addOption(
        static_cast<int>(Access::RequirePassword),
        "RequirePassword"
    );
    _defaultAccess.addOption(
        static_cast<int>(Access::Allow),
        "Allow"
    );

    if (p.defaultAccess.has_value()) {
        switch (*p.defaultAccess) {
            case Parameters::Access::Allow:
                _defaultAccess.setValue(static_cast<int>(Access::Allow));
                break;
            case Parameters::Access::Deny:
                _defaultAccess.setValue(static_cast<int>(Access::Deny));
                break;
            case Parameters::Access::RequirePassword:
                _defaultAccess.setValue(static_cast<int>(Access::RequirePassword));
                break;
        }
    }

    _allowAddresses = p.allowAddresses.value_or(_allowAddresses);
    _denyAddresses = p.denyAddresses.value_or(_denyAddresses);
    _requirePasswordAddresses =
        p.requirePasswordAddresses.value_or(_requirePasswordAddresses);

    setIdentifier(p.identifier);
    setGuiName(p.identifier);
    setDescription("Settings for server interface " + p.identifier);

    switch (p.type) {
        case Parameters::Type::TcpSocket:
            _socketType = static_cast<int>(InterfaceType::TcpSocket);
            break;
        case Parameters::Type::WebSocket:
            _socketType = static_cast<int>(InterfaceType::WebSocket);
            break;
    }

    _password = p.password.value_or(_password);
    _port = p.port;
    _enabled = p.enabled;

    auto reinitialize = [this]() {
        deinitialize();
        initialize();
    };

    _socketType.onChange(reinitialize);
    addProperty(_socketType);
    _port.onChange(reinitialize);
    addProperty(_port);
    _enabled.onChange(reinitialize);
    addProperty(_enabled);
    _defaultAccess.onChange(reinitialize);
    addProperty(_defaultAccess);
    _allowAddresses.onChange(reinitialize);
    addProperty(_allowAddresses);
    _requirePasswordAddresses.onChange(reinitialize);
    addProperty(_requirePasswordAddresses);
    _denyAddresses.onChange(reinitialize);
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
