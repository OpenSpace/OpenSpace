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

#include <openspace/properties/propertyowner.h>

#include <modules/softwareintegration/network/softwareconnection.h>

namespace openspace {

class PointDataMessageHandler {
public:
    void handlePointDataMessage(const std::vector<char>& message,
        SoftwareConnection& connection);
    void handleColorMessage(const std::vector<char>& message);
    void handleOpacityMessage(const std::vector<char>& message);
    void handlePointSizeMessage(const std::vector<char>& message);
    void handleVisiblityMessage(const std::vector<char>& message);

    void preSyncUpdate();

private:
    void subscribeToRenderableUpdates(const std::string& identifier,
        SoftwareConnection& connection);

    float readFloatValue(const std::vector<char>& message, int& offset);
    glm::vec3 readColor(const std::vector<char>& message, int& offset);
    std::string readString(const std::vector<char>& message, int& offset);
    std::vector<float> readFloatData(const std::vector<char>& message,
        int nValues, int& offset);

    std::unordered_map<std::string, std::function<void()>> _onceNodeExistsCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___POINTDATAMESSAGEHANDLER___H__
