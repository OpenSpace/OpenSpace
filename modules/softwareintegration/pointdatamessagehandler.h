/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___POINTDATAMESSAGEHANDLER___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___POINTDATAMESSAGEHANDLER___H__

#include <unordered_map>

#include <openspace/properties/propertyowner.h>

#include <modules/softwareintegration/network/softwareconnection.h>

namespace openspace {

class Renderable;

class PointDataMessageHandler {
    struct Callback {
        std::function<void()> function;
        std::vector<softwareintegration::storage::Key> waitForData = {};
        std::string description = "???"; // To help debugging. Maybe remove?
    };
    using CallbackList = std::vector<Callback>;
    using CallbackMap = std::unordered_map<std::string, CallbackList>;

public:
    void handlePointDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleFixedColorMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleColormapMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleAttributeDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleOpacityMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleFixedPointSizeMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleLinearPointSizeMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleVisibilityMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);
    void handleRemoveSGNMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection);

    void postSync();

private:
    const Renderable* getRenderable(const std::string& identifier);
    void checkRenderable(
        const std::vector<char>& message, size_t& messageOffset,
        std::shared_ptr<SoftwareConnection> connection, std::string& identifier
    );

    void subscribeToRenderableUpdates(const std::string& identifier, std::shared_ptr<SoftwareConnection> connection);

    void addCallback(
        const std::string& identifier,
        const Callback& newCallback
    );

    CallbackMap _onceNodeExistsCallbacks;
    std::mutex _onceNodeExistsCallbacksMutex;
    size_t _onceNodeExistsCallbacksRetries{0};

    void onFixedColorChange(
        properties::Property* property,
        const std::string& identifier,
        std::shared_ptr<SoftwareConnection> connection
    );
    void onOpacityChange(
        properties::Property* property,
        const std::string& identifier,
        std::shared_ptr<SoftwareConnection> connection
    );
    void onFixedPointSizeChange(
        properties::Property* property,
        const std::string& identifier,
        std::shared_ptr<SoftwareConnection> connection
    );
    void onVisibilityChange(
        properties::Property* property,
        const std::string& identifier,
        std::shared_ptr<SoftwareConnection> connection
    );
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___POINTDATAMESSAGEHANDLER___H__
