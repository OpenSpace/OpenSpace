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

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

#include <fmt/format.h>

namespace {
    constexpr const char* _loggerCat = "WebGuiModule";
    constexpr const char* ModeKey = "Mode";
    constexpr const char* ModeDevelopment = "Development";
    constexpr const char* ModeProduction = "Production";

    constexpr openspace::properties::Property::PropertyInfo ServerProcessEnabledInfo = {
        "ServerProcessEnabled",
        "Enable Server Process",
        "Enable the node js based process used to serve the Web GUI."
    };
}

namespace openspace {

WebGuiModule::WebGuiModule()
    : OpenSpaceModule(WebGuiModule::Name)
    , _serverProcessEnabled(ServerProcessEnabledInfo, false)
{
    addProperty(_serverProcessEnabled);
}

void WebGuiModule::internalInitialize(const ghoul::Dictionary& dictionary) {

    // Node js is only launched by OpenSpace when the GUI is in `Production` mode.
    // In `Development`, the developer is expected to run the GUI manually,
    // using `npm start`.
    _serverProcessEnabled =
        !dictionary.hasValue<std::string>(ModeKey) ||
        dictionary.value<std::string>(ModeKey) == ModeProduction;

    if (!_serverProcessEnabled) {
        LINFO(
            "Web GUI running in development mode. "
            "Use `npm start` in WebGui module to launch Web GUI."
        );
        return;
    }
    const std::function<void()> startOrStop = [this]() {
        if (_serverProcessEnabled) {
            startProcess();
        } else {
            stopProcess();
        }
    };

    _serverProcessEnabled.onChange(startOrStop);
    startOrStop();
}

void WebGuiModule::startProcess() {
    const std::string nodePath = absPath("${MODULE_WEBGUI}/ext/nodejs/node.exe");
    const std::string scriptPath = absPath("${MODULE_WEBGUI}/ext/backend/dist/server.js");
    const std::string workingDirectory = absPath("${MODULE_WEBGUI}/ext/backend");

    _process = std::make_unique<ghoul::Process>(
        "\"" + nodePath + "\" \"" + scriptPath + "\"",
        workingDirectory,
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

