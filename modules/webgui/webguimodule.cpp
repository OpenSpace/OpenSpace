/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <openspace/documentation/documentationgenerator.h>
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
        DirectoriesInfo =
    {
        "Directories",
        "Directories",
        "Directories from which to to serve static content, as a string list "
        "with entries expressed as pairs, where every odd is the endpoint name and every "
        "even is the directory.",
    };

    constexpr openspace::properties::Property::PropertyInfo
        ServedDirectoriesInfo =
    {
        "ServedDirectories",
        "ServedDirectories",
        "Directories that are currently served. This value is set by the server process, "
        "as a verification of the actually served directories. For example, an onChange "
        "callback can be registered to this, to reload browsers when the server is ready."
        "Manual changes to this property have no effect."
    };

    constexpr const char* DefaultAddress = "localhost";
    constexpr const int DefaultPort = 4680;
}

namespace openspace {

WebGuiModule::WebGuiModule()
    : OpenSpaceModule(WebGuiModule::Name)
    , _enabled(ServerProcessEnabledInfo, true)
    , _entryPoint(ServerProcessEntryPointInfo)
    , _directories(DirectoriesInfo)
    , _servedDirectories(ServedDirectoriesInfo)
    , _port(PortInfo, DefaultPort)
    , _address(AddressInfo, DefaultAddress)
    , _webSocketInterface(WebSocketInterfaceInfo, "")
{
    addProperty(_enabled);
    addProperty(_entryPoint);
    addProperty(_directories);
    addProperty(_servedDirectories);
    addProperty(_address);
    addProperty(_port);
}

int WebGuiModule::port() const {
    return _port;
}

std::string  WebGuiModule::address() const {
    return _address;
}

WebGuiModule::CallbackHandle WebGuiModule::addEndpointChangeCallback(EndpointCallback cb)
{
    CallbackHandle handle = _nextCallbackHandle++;
    _endpointChangeCallbacks.push_back({ handle, std::move(cb) });
    return handle;
}

void WebGuiModule::removeEndpointChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _endpointChangeCallbacks.begin(),
        _endpointChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, EndpointCallback>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _deltaTimeChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _endpointChangeCallbacks.erase(it);
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
        if (_enabled && !_entryPoint.value().empty()) {
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
    _directories.onChange(restartIfEnabled);
    _servedDirectories.onChange([this]() {
        std::unordered_map<std::string, std::string> newEndpoints;
        std::vector<std::string> list = _servedDirectories.value();
        for (int i = 0; i < list.size() - 1; i += 2) {
            newEndpoints[list[i]] = newEndpoints[list[i + 1]];
        }
        for (const auto& e : _endpoints) {
            if (newEndpoints.find(e.first) == newEndpoints.end()) {
                // This endpoint existed before but does not exist anymore.
                notifyEndpointListeners(e.first, false);
            }
        }
        for (const auto& e : newEndpoints) {
            if (_endpoints.find(e.first) == _endpoints.end() ||
                newEndpoints[e.first] != e.second) {
                // This endpoint exists now but did not exist before,
                // or the directory has changed.
                notifyEndpointListeners(e.first, true);
            }
        }
        _endpoints = newEndpoints;
    });
    _port.onChange(restartIfEnabled);
    startOrStop();
}

void WebGuiModule::notifyEndpointListeners(const std::string& endpoint, bool exists) {
    using K = const CallbackHandle;
    using V = EndpointCallback;
    for (const std::pair<K, V>& it : _endpointChangeCallbacks) {
        it.second(endpoint, exists);
    }
}

void WebGuiModule::startProcess() {
    _endpoints.clear();

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

    std::string formattedDirectories = "[";

    std::vector<std::string> directories = _directories.value();
    bool first = true;

    for (std::string& str : directories) {
        if (!first) {
            formattedDirectories += ",";
        }
        first = false;

        // Escape twice: First json, and then bash (which needs same escape sequences)
        formattedDirectories += "\\\"" + escapedJson(escapedJson(str)) + "\\\"";
    }
    formattedDirectories += "]";

    const std::string command = "\"" + nodePath + "\" "
        + "\"" + absPath(_entryPoint.value()) + "\"" +
        " --directories \"" + formattedDirectories + "\"" +
        " --http-port \"" + std::to_string(_port.value()) + "\" " +
        " --ws-address \"" + _address.value() + "\"" +
        " --ws-port " + std::to_string(webSocketPort) +
        " --auto-close --local";

    LDEBUG(command);

    _process = std::make_unique<ghoul::Process>(
        command,
        absPath("${BIN}"),
        [](const char* data, size_t n) {
            const std::string str(data, n);
            LDEBUG(fmt::format("Web GUI server output: {}", str));
        },
        [](const char* data, size_t n) {
            const std::string str(data, n);
            LERROR(fmt::format("Web GUI server error: {}", str));
        }
    );
}

void WebGuiModule::stopProcess() {
    for (const auto& e : _endpoints) {
        notifyEndpointListeners(e.first, false);
    }
    _endpoints.clear();

    if (_process) {
        _process->kill();
    }
    _process = nullptr;
}

} // namespace openspace
