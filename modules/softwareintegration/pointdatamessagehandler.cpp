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

#include <modules/softwareintegration/pointdatamessagehandler.h>

#include <modules/softwareintegration/softwareintegrationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include <iomanip>

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegration";

    constexpr const int LargeDatasetThreshold = 5000;
} // namespace

namespace openspace {

void PointDataMessageHandler::handlePointDataMessage(const std::vector<char>& message,
                                                     SoftwareConnection& connection)
{
    int messageOffset = 0;

    // The following order of creating variables is the exact order they are received
    // in the message. If the order is not the same, the global variable
    // 'message offset' will be wrong
    const std::string identifier = readString(message, messageOffset);
    const glm::vec3 color = readColor(message, messageOffset);
    const float opacity = readFloatValue(message, messageOffset);
    const float size = readFloatValue(message, messageOffset);
    const std::string guiName = readString(message, messageOffset);

    // 9 first bytes is the length of the data
    const int lengthOffset = messageOffset + 9;
    std::string length;
    for (int i = messageOffset; i < lengthOffset; i++) {
        length.push_back(message[i]);
        messageOffset++;
    }

    const int nPoints = stoi(length);

    const std::vector<float> xCoordinates = readFloatData(message, nPoints, messageOffset);
    const std::vector<float> yCoordinates = readFloatData(message, nPoints, messageOffset);
    const std::vector<float> zCoordinates = readFloatData(message, nPoints, messageOffset);

    // Do some simple checking to make sure the data was loaded correctly
    // @TODO make this check more clever to avoid trying to read all data
    // if something goes wrong
    bool equalSize = (xCoordinates.size() == yCoordinates.size()) &&
        (xCoordinates.size() == zCoordinates.size());

    if (!equalSize || (nPoints != xCoordinates.size())) {
        LERROR("Something went wrong when loading the data!");
        return;
    }

    using namespace std::string_literals;

    // Create a renderable
    ghoul::Dictionary renderable;
    renderable.setValue("Type", "RenderablePointsCloud"s);
    renderable.setValue("Color", static_cast<glm::dvec3>(color));
    renderable.setValue("Opacity", static_cast<double>(opacity));
    renderable.setValue("Size", static_cast<double>(size));

    if (nPoints > LargeDatasetThreshold) {
        // If huge number of points, use the module's temporary data storage
        const int nValuesPerPoint = 3;
        const int nValues = nPoints * nValuesPerPoint;

        std::vector<float> data;
        data.reserve(nValues);
        for (int i = 0; i < nPoints; i++) {
            float x = xCoordinates[i];
            float y = yCoordinates[i];
            float z = zCoordinates[i];
            data.insert(data.end(), { x, y, z });
        }

        // Use the renderable identifier as the data key
        const std::string key = identifier;
        auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
        module->storeData(key, std::move(data));

        renderable.setValue("DataStorageKey", key);
    }
    else {
        ghoul::Dictionary pointDataDictonary;
        for (int i = 0; i < nPoints; i++) {
            float x = xCoordinates[i];
            float y = yCoordinates[i];
            float z = zCoordinates[i];
            glm::dvec3 point{ x, y, z };

            const std::string key = fmt::format("[{}]", i + 1);

            // Avoid passing nan values through dictionary
            if (glm::any(glm::isnan(point))) {
                point = glm::dvec3(0.0);
                // @TODO Keep track of invalid indices?
            }

            pointDataDictonary.setValue<glm::dvec3>(key, std::move(point));
        }
        renderable.setValue("Data", pointDataDictonary);
    }

    ghoul::Dictionary gui;
    gui.setValue("Name", guiName);
    gui.setValue("Path", "/Software Integration"s);

    ghoul::Dictionary node;
    node.setValue("Identifier", identifier);
    node.setValue("Renderable", renderable);
    node.setValue("GUI", gui);

    openspace::global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + ghoul::formatLua(node) + ")",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '" + identifier + "')"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    // We have to wait until the renderable exists before we can subscribe to
    // changes in its properties
    auto callback = [this, identifier, &connection]() {
        subscribeToRenderableUpdates(identifier, connection);
    };
    _onceNodeExistsCallbacks.emplace(identifier, callback);
}

void PointDataMessageHandler::handleColorMessage(const std::vector<char>& message) {
    int messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    const glm::vec3 color = readColor(message, messageOffset);

    // Get color of renderable
    const Renderable* r = renderable(identifier);
    if (!r) {
        return;
    }

    properties::Property* colorProperty = r->property("Color");
    std::any propertyAny = colorProperty->get();
    glm::vec3 propertyColor = std::any_cast<glm::vec3>(propertyAny);

    // Update color of renderable
    if (propertyColor != color) {
        colorProperty->set(color);
    }
}

void PointDataMessageHandler::handleOpacityMessage(const std::vector<char>& message) {
    int messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    const float opacity = readFloatValue(message, messageOffset);

    // Get opacity of renderable
    const Renderable* r = renderable(identifier);
    if (!r) {
        return;
    }

    properties::Property* opacityProperty = r->property("Opacity");
    auto propertyAny = opacityProperty->get();
    float propertyOpacity = std::any_cast<float>(propertyAny);

    // Update opacity of renderable
    if (propertyOpacity != opacity) {
        opacityProperty->set(opacity);
    }
}

void PointDataMessageHandler::handlePointSizeMessage(const std::vector<char>& message) {
    int messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    float size = readFloatValue(message, messageOffset);

    // Get size of renderable
    const Renderable* r = renderable(identifier);
    if (!r) {
        return;
    }

    properties::Property* sizeProperty = r->property("Size");
    auto propertyAny = sizeProperty->get();
    float propertySize = std::any_cast<float>(propertyAny);

    // Update size of renderable
    if (propertySize != size) {
        sizeProperty->set(size);
    }
}

void PointDataMessageHandler::handleVisiblityMessage(const std::vector<char>& message) {
    int messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    std::string visibility;
    visibility.push_back(message[messageOffset]);

    // Toggle visibility of renderable
    const Renderable* r = renderable(identifier);
    if (!r) {
        return;
    }

    bool boolValue = (visibility == "F") ? false : true;
    properties::Property* visibilityProperty = r->property("ToggleVisibility");
    visibilityProperty->set(boolValue);
}

void PointDataMessageHandler::preSyncUpdate() {
    if (_onceNodeExistsCallbacks.empty()) {
        return;
    }

    // Check if the scene graph node has been created. If so, call the corresponding
    // callback function to set up any subscriptions
    auto it = _onceNodeExistsCallbacks.begin();
    while (it != _onceNodeExistsCallbacks.end()) {
        const std::string& identifier = it->first;
        const std::function<void()>& callback = it->second;
        const SceneGraphNode* sgn =
            global::renderEngine->scene()->sceneGraphNode(identifier);

        if (sgn) {
            callback();
            it = _onceNodeExistsCallbacks.erase(it);
            continue;
        }
        it++;
    }
}

std::string formatUpdateMessage(std::string_view messageType,
                                std::string_view identifier, std::string_view value)
{
    const int lengthOfIdentifier = static_cast<int>(identifier.length());
    const int lengthOfValue = static_cast<int>(value.length());
    std::string subject = fmt::format(
        "{}{}{}{}", lengthOfIdentifier, identifier, lengthOfValue, value
    );

    // Format length of subject to always be 4 digits
    std::ostringstream os;
    os << std::setfill('0') << std::setw(4) << subject.length();
    const std::string lengthOfSubject = os.str();

    return fmt::format("{}{}{}", messageType, lengthOfSubject, subject);
}

void PointDataMessageHandler::subscribeToRenderableUpdates(const std::string& identifier,
                                                          SoftwareConnection& connection)
{
    const Renderable* aRenderable = renderable(identifier);
    if (!aRenderable) {
        LERROR(fmt::format("Renderable with identifier '{}' doesn't exist", identifier));
        return;
    }

    if (!connection.isConnected()) {
        LERROR(fmt::format(
            "Could not subscribe to updates for renderable '{}' due to lost connection",
            identifier
        ));
        return;
    }

    properties::Property* colorProperty = aRenderable->property("Color");
    properties::Property* opacityProperty = aRenderable->property("Opacity");
    properties::Property* sizeProperty = aRenderable->property("Size");
    properties::Property* visibilityProperty = aRenderable->property("ToggleVisibility");

    // Update color of renderable
    auto updateColor = [colorProperty, identifier, &connection]() {
        const std::string value = colorProperty->getStringValue();
        const std::string message = formatUpdateMessage("UPCO", identifier, value);
        connection.sendMessage(message);
    };
    if (colorProperty) {
        colorProperty->onChange(updateColor);
    }

    // Update opacity of renderable
    auto updateOpacity = [opacityProperty, identifier, &connection]() {
        const std::string value = opacityProperty->getStringValue();
        const std::string message = formatUpdateMessage("UPOP", identifier, value);
        connection.sendMessage(message);
    };
    if (opacityProperty) {
        opacityProperty->onChange(updateOpacity);
    }

    // Update size of renderable
    auto updateSize = [sizeProperty, identifier, &connection]() {
        const std::string value = sizeProperty->getStringValue();
        const std::string message = formatUpdateMessage("UPSI", identifier, value);
        connection.sendMessage(message);
    };
    if (sizeProperty) {
        sizeProperty->onChange(updateSize);
    }

    // Toggle visibility of renderable
    auto toggleVisibility = [visibilityProperty, identifier, &connection]() {
        const int lengthOfIdentifier = static_cast<int>(identifier.length());

        bool isVisible = visibilityProperty->getStringValue() == "true";
        std::string_view visibilityFlag = isVisible ? "T" : "F";

        const std::string subject = fmt::format(
            "{}{}{}", lengthOfIdentifier, identifier, visibilityFlag
        );
        // We don't need a lengthOfValue here because it will always be 1 character

        // @TODO (emmbr 2021-02-02) make sure this message has the same format as the
        // others, so the 'formatUpdateMessage(..)' function can be used here

        // Format length of subject to always be 4 digits
        std::ostringstream os;
        os << std::setfill('0') << std::setw(4) << subject.length();
        const std::string lengthOfSubject = os.str();

        const std::string_view messageType = "TOVI";
        const std::string message = fmt::format(
            "{}{}{}", messageType, lengthOfSubject, subject
        );
        connection.sendMessage(message);
    };
    if (visibilityProperty) {
        visibilityProperty->onChange(toggleVisibility);
    }
}

float PointDataMessageHandler::readFloatValue(const std::vector<char>& message,
                                              int& offset)
{
    std::string length;
    length.push_back(message[offset]);
    offset++;

    int lengthOfValue = stoi(length);
    std::string value;
    int counter = 0;
    while (counter != lengthOfValue) {
        value.push_back(message[offset]);
        offset++;
        counter++;
    }
    return std::stof(value);
}

glm::vec3 PointDataMessageHandler::readColor(const std::vector<char>& message,
                                             int& offset)
{
    std::string lengthOfColor; // Not used for now, but sent in message
    lengthOfColor.push_back(message[offset]);
    offset++;
    lengthOfColor.push_back(message[offset]);
    offset++;

    // Color is recieved in a string-format of (redValue, greenValue, blueValue)
    // Therefore, we have to iterate through the message and ignore characters
    // "( , )" and separate the values in the string
    std::string red;
    while (message[offset] != ',') {
        if (message[offset] == '(') {
            offset++;
        }
        else {
            red.push_back(message[offset]);
            offset++;
        }
    }
    offset++;

    std::string green;
    while (message[offset] != ',') {
        green.push_back(message[offset]);
        offset++;
    }
    offset++;

    std::string blue;
    while (message[offset] != ')') {
        blue.push_back(message[offset]);
        offset++;
    }
    offset++;

    // Convert red, green, blue strings to floats
    float r = std::stof(red);
    float g = std::stof(green);
    float b = std::stof(blue);

    return glm::vec3(r, g, b);
}

std::string PointDataMessageHandler::readString(const std::vector<char>& message,
                                                int& offset)
{
    std::string length;
    length.push_back(message[offset]);
    offset++;
    length.push_back(message[offset]);
    offset++;

    int lengthOfValue = stoi(length);
    std::string value;
    int counter = 0;
    while (counter != lengthOfValue) {
        value.push_back(message[offset]);
        offset++;
        counter++;
    }

    return value;
}

std::vector<float> PointDataMessageHandler::readFloatData(const std::vector<char>& message,
                                                         int nValues, int& offset)
{
    std::vector<float> data;

    for (int counter = 0; counter < nValues; ++counter) {
        std::string value;
        while (message[offset] != ',') {
            value.push_back(message[offset]);
            offset++;
        }

        try {
            float dataValue = stof(value);
            data.push_back(dataValue);
        }
        catch (const std::invalid_argument& ia) {
            LERROR(fmt::format(
                "Error reading value {}. Invalid argument: {} ", counter + 1, ia.what()
            ));
            return std::vector<float>();
        }

        offset++;
    }

    return data;
}

} // namespace openspace
