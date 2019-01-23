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

#include <modules/webgui/webguimodule.h>

#include <modules/server/servermodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

namespace {
    constexpr const char* _loggerCat = "WebGuiModule";

    constexpr openspace::properties::Property::PropertyInfo ServerProcessEnabledInfo = {
        "ServerProcessEnabled",
        "Enable Server Process",
        "Enable the node js based process used to serve the Web GUI."
    };

    constexpr openspace::properties::Property::PropertyInfo AddressInfo = {
        "Address",
        "Address",
        "The network address to use when connecting to OpenSpace from the Web GUI."
    };

    constexpr openspace::properties::Property::PropertyInfo PortInfo = {
        "Port",
        "Port",
        "The network port to use when serving the Web GUI over HTTP."
    };

    constexpr openspace::properties::Property::PropertyInfo WebSocketInterfaceInfo = {
        "WebSocketInterface",
        "WebSocket Interface",
        "The identifier of the websocket interface to use when communicating."
    };

    constexpr openspace::properties::Property::PropertyInfo ServerProcessEntryPointInfo =
    {
        "ServerProcessEntryPoint",
        "Server Process Entry Point",
        "The node js command to invoke."
    };

    constexpr openspace::properties::Property::PropertyInfo
        WebDirectoryInfo =
    {
        "WebDirectory",
        "Web Directory",
        "Directory from which to to serve static content",
    };

    constexpr const char* DefaultAddress = "localhost";
    constexpr const int DefaultPort = 4680;
}

namespace openspace {

WebGuiModule::WebGuiModule()
    : OpenSpaceModule(WebGuiModule::Name)
    , _enabled(ServerProcessEnabledInfo, false)
    , _entryPoint(ServerProcessEntryPointInfo)
    , _webDirectory(WebDirectoryInfo)
    , _port(PortInfo, DefaultPort)
    , _address(AddressInfo, DefaultAddress)
    , _webSocketInterface(WebSocketInterfaceInfo, "")
{
    addProperty(_enabled);
    addProperty(_entryPoint);
    addProperty(_webDirectory);
    addProperty(_address);
    addProperty(_port);
}

int WebGuiModule::port() const {
    return _port;
}

std::string  WebGuiModule::address() const {
    return _address;
}

void WebGuiModule::internalInitialize(const ghoul::Dictionary& configuration) {
    if (configuration.hasValue<int>(PortInfo.identifier)) {
        _port = configuration.value<int>(PortInfo.identifier);
    }

    if (configuration.hasValue<std::string>(AddressInfo.identifier)) {
        _address = configuration.value<std::string>(AddressInfo.identifier);
    }

    if (configuration.hasValue<std::string>(WebSocketInterfaceInfo.identifier)) {
        _webSocketInterface =
            configuration.value<std::string>(WebSocketInterfaceInfo.identifier);
    }

    auto startOrStop = [this]() {
        if (_enabled) {
            startProcess();
        } else {
            stopProcess();
        }
    };

    auto restartIfEnabled = [this]() {
        stopProcess();
        if (_enabled) {
            startProcess();
        }
    };

    _enabled.onChange(startOrStop);
    _entryPoint.onChange(restartIfEnabled);
    _webDirectory.onChange(restartIfEnabled);
    _port.onChange(restartIfEnabled);
    startOrStop();
}

void WebGuiModule::startProcess() {
    ServerModule* serverModule = global::moduleEngine.module<ServerModule>();
    const ServerInterface* serverInterface =
        serverModule->serverInterfaceByIdentifier(_webSocketInterface);
    if (!serverInterface) {
        LERROR("Missing server interface. Server process could not start.");
        return;
    }
    const int webSocketPort = serverInterface->port();


#ifdef _MSC_VER
    const std::string nodePath = absPath("${MODULE_WEBGUI}/ext/nodejs/node.exe");
#else
    const std::string nodePath = absPath("${MODULE_WEBGUI}/ext/nodejs/node");
#endif

    const std::string command = "\"" + nodePath + "\" "
        + "\"" + _entryPoint.value() + "\"" +
        " --directory \"" + _webDirectory.value() + "\"" +
        " --http-port \"" + std::to_string(_port.value()) + "\" " +
        " --ws-address \"" + _address.value() + "\"" +
        " --ws-port \"" + std::to_string(webSocketPort) + "\"" +
        " --auto-close --local";

    _process = std::make_unique<ghoul::Process>(
        command,
        _webDirectory.value(),
        [](const char* data, size_t n) {
            const std::string str(data, n);
            LDEBUG(fmt::format("Web GUI server output: {}", str));
        }
    );
}

void WebGuiModule::stopProcess() {
    if (_process) {
        _process->kill();
    }
    _process = nullptr;
}

} // namespace openspace
