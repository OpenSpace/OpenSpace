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

    constexpr openspace::properties::Property::PropertyInfo ServerProcessEntryPointInfo =
    {
        "ServerProcessEntryPoint",
        "Server Process Entry Point",
        "The node js command to invoke"
    };

    constexpr openspace::properties::Property::PropertyInfo
        ServerProcessWorkingDirectoryInfo =
    {
        "ServerProcessWorkingDirectory",
        "Server Process Working Directory",
        "The working directory of the process"
    };
}

namespace openspace {

WebGuiModule::WebGuiModule()
    : OpenSpaceModule(WebGuiModule::Name)
    , _enabled(ServerProcessEnabledInfo, false)
    , _entryPoint(ServerProcessEntryPointInfo)
    , _workingDirectory(ServerProcessWorkingDirectoryInfo)
{
    addProperty(_enabled);
    addProperty(_entryPoint);
    addProperty(_workingDirectory);
}

void WebGuiModule::internalInitialize(const ghoul::Dictionary&) {
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
    _workingDirectory.onChange(restartIfEnabled);
    startOrStop();
}

void WebGuiModule::startProcess() {
#ifdef _MSC_VER
    const std::string nodePath = absPath("${MODULE_WEBGUI}/ext/nodejs/node.exe");
#else
    const std::string nodePath = absPath("${MODULE_WEBGUI}/ext/nodejs/node");
#endif

    _process = std::make_unique<ghoul::Process>(
        "\"" + nodePath + "\" \"" + _entryPoint.value() + "\"",
        _workingDirectory.value(),
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
