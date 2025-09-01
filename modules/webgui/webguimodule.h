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

#ifndef __OPENSPACE_MODULE_WEBGUI___WEBGUIMODULE___H__
#define __OPENSPACE_MODULE_WEBGUI___WEBGUIMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <ghoul/misc/process.h>
#include <memory>
#include <unordered_map>
#include <vector>

namespace openspace {

class WebGuiModule : public OpenSpaceModule {
public:
    using CallbackHandle = int;
    using EndpointCallback = std::function<void(const std::string&, bool)>;

    static constexpr const char* Name = "WebGui";

    WebGuiModule();
    int port() const;
    std::string address() const;
    CallbackHandle addEndpointChangeCallback(EndpointCallback cb);
    void removeEndpointChangeCallback(CallbackHandle);

    static documentation::Documentation Documentation();

protected:
    void internalInitialize(const ghoul::Dictionary&) override;

private:
    void startProcess();
    void stopProcess();
    void notifyEndpointListeners(const std::string& endpoint, bool exists);

    std::unique_ptr<ghoul::Process> _process;
    properties::BoolProperty _enabled;
    properties::StringProperty _entryPoint;
    properties::StringListProperty _directories;
    properties::StringListProperty _servedDirectories;
    properties::StringProperty _defaultEndpoint;

    std::unordered_map<std::string, std::string> _endpoints;

    properties::IntProperty _port;
    properties::StringProperty _address;
    properties::StringProperty _webSocketInterface;

    std::vector<std::pair<CallbackHandle, EndpointCallback>> _endpointChangeCallbacks;
    int _nextCallbackHandle = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBGUI___WEBGUIMODULE___H__
