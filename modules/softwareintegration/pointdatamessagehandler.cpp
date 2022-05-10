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
#include <modules/softwareintegration/simp.h>
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
    constexpr const char* _loggerCat = "PDatMessHand";
} // namespace

namespace openspace {

void PointDataMessageHandler::handlePointDataMessage(const std::vector<char>& message,
                                                     SoftwareConnection& connection,
                                                     std::list<std::string>& sceneGraphNodes)
{
    size_t messageOffset = 0;

    std::string identifier;
    glm::vec4 color;
    float opacity;
    float size;
    std::string guiName;
    int nPoints;
    int dimensionality;
    std::vector<float> points;

    try {
        // The following order of creating variables is the exact order they are received
        // in the message. If the order is not the same, the global variable
        // 'message offset' will be wrong
        identifier = readString(message, messageOffset);
        sceneGraphNodes.push_back(identifier);
        color = readColor(message, messageOffset);
        opacity = readFloatValue(message, messageOffset);
        size = readFloatValue(message, messageOffset);
        guiName = readString(message, messageOffset);
        nPoints = readIntValue(message, messageOffset);
        dimensionality = readIntValue(message, messageOffset);
        points.reserve(nPoints * dimensionality);
        points = readPointData(message, messageOffset, nPoints, dimensionality);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading point data message: {}", err.message));
        return;
    }

    using namespace std::string_literals;

    // Create a renderable
    ghoul::Dictionary renderable;
    renderable.setValue("Type", "RenderablePointsCloud"s);
    renderable.setValue("Color", static_cast<glm::dvec3>(glm::vec3{color.r, color.g, color.b}));
    renderable.setValue("Opacity", static_cast<double>(opacity));
    renderable.setValue("Size", static_cast<double>(size));
    renderable.setValue("Identifier", identifier);

    // Use the renderable identifier as the data key
    const std::string key = identifier + "-DataPoints";
    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    module->storeData(key, std::move(points));

    renderable.setValue("DataStorageKey", key);

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
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')"
        "openspace.setPropertyValueSingle('Modules.CefWebGui.Reload', nil)", // Reload WebGUI so that SoftwareIntegration GUI appears
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
    size_t messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    const glm::vec3 color = readColor(message, messageOffset);

    // Get renderable
    auto r = getRenderable(identifier);
    if (!r) return;

    // Get color of renderable
    properties::Property* colorProperty = r->property("Color");
    std::any propertyAny = colorProperty->get();
    glm::vec3 propertyColor = std::any_cast<glm::vec3>(propertyAny);

    if (propertyColor != glm::vec3(0, 0, 0)) {
        LWARNING(fmt::format("propertyColor '{}'", propertyColor));
    }

    // Update color of renderable
    if (propertyColor != color) {
        std::string colorScript = fmt::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Color', {});",
            identifier, ghoul::to_string(color)
        );

        std::string disableColorMapScript = fmt::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColorMapEnabled', {});",
            identifier, false
        );

        openspace::global::scriptEngine->queueScript(
            colorScript,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        openspace::global::scriptEngine->queueScript(
            disableColorMapScript,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void PointDataMessageHandler::handleColorMapMessage(const std::vector<char>& message) {
    size_t messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    const std::vector<float> colorMap = readColorMap(message, messageOffset);

     // Use the renderable identifier as the data key
     const std::string key = identifier + "-ColorMap";
     auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
     module->storeData(key, std::move(colorMap));

     // Get renderable
     auto r = getRenderable(identifier);
     if (!r) return;

     std::string script = fmt::format(
         "openspace.setPropertyValueSingle('Scene.{}.Renderable.LoadNewColorMap', {});",
         identifier, true
     );
     openspace::global::scriptEngine->queueScript(
         script,
         scripting::ScriptEngine::RemoteScripting::Yes
     );
}

void PointDataMessageHandler::handleOpacityMessage(const std::vector<char>& message) {
    size_t messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    const float opacity = readFloatValue(message, messageOffset);

    // Get renderable
    auto r = getRenderable(identifier);
    if (!r) return;

    // Get opacity of renderable
    properties::Property* opacityProperty = r->property("Opacity");
    auto propertyAny = opacityProperty->get();
    float propertyOpacity = std::any_cast<float>(propertyAny);

    // Update opacity of renderable
    if (propertyOpacity != opacity) {
        std::string script = fmt::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Opacity', {});",
            identifier, ghoul::to_string(opacity)
        );
        openspace::global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void PointDataMessageHandler::handlePointSizeMessage(const std::vector<char>& message) {
    size_t messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    float size = readFloatValue(message, messageOffset);

    // Get renderable
    auto r = getRenderable(identifier);
    if (!r) return;

    // Get size of renderable
    properties::Property* sizeProperty = r->property("Size");
    auto propertyAny = sizeProperty->get();
    float propertySize = std::any_cast<float>(propertyAny);

    // Update size of renderable
    if (propertySize != size) {
        // TODO: Add to script when the "send back to glue" stuff is done
        // "openspace.setPropertyValueSingle('Scene.{}.Renderable.Size', {}, 1);",
        std::string script = fmt::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Size', {});",
            identifier, ghoul::to_string(size)
        );
        openspace::global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void PointDataMessageHandler::handleVisiblityMessage(const std::vector<char>& message) {
    size_t messageOffset = 0;
    const std::string identifier = readString(message, messageOffset);
    std::string visibilityMessage;
    visibilityMessage.push_back(message[messageOffset]);

    // Get renderable
    auto r = getRenderable(identifier);
    if (!r) return;

    // Toggle visibility of renderable
    const std::string visability = visibilityMessage == "T" ? "true" : "false";
    std::string script = fmt::format(
        "openspace.setPropertyValueSingle('Scene.{}.Renderable.Enabled', {});",
        identifier, visability
    );
    openspace::global::scriptEngine->queueScript(
        script,
        scripting::ScriptEngine::RemoteScripting::Yes
    );
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

const Renderable* PointDataMessageHandler::getRenderable(const std::string& identifier) const {
    const Renderable* r = renderable(identifier);
    if (!r) {
        LERROR(fmt::format("No renderable with identifier '{}' was found", identifier));
        return nullptr;
    };
    return r;
}

void PointDataMessageHandler::subscribeToRenderableUpdates(
    const std::string& identifier,
    SoftwareConnection& connection
) {
    // Get renderable
    auto aRenderable = getRenderable(identifier);
    if (!aRenderable) return;

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
        const std::string message = simp::formatUpdateMessage(simp::MessageType::Color, identifier, value);
        connection.sendMessage(message);
    };
    if (colorProperty) {
        colorProperty->onChange(updateColor);
    }

    // Update opacity of renderable
    auto updateOpacity = [opacityProperty, identifier, &connection]() {
        const std::string value = opacityProperty->getStringValue();
        const std::string message = simp::formatUpdateMessage(simp::MessageType::Opacity, identifier, value);
        connection.sendMessage(message);
    };
    if (opacityProperty) {
        opacityProperty->onChange(updateOpacity);
    }

    // Update size of renderable
    auto updateSize = [sizeProperty, identifier, &connection]() {
        const std::string value = sizeProperty->getStringValue();
        const std::string message = simp::formatUpdateMessage(simp::MessageType::Size, identifier, value);
        connection.sendMessage(message);
    };
    if (sizeProperty) {
        sizeProperty->onChange(updateSize);
    }

    // Toggle visibility of renderable
    auto toggleVisibility = [visibilityProperty, identifier, &connection]() {
        bool isVisible = visibilityProperty->getStringValue() == "true";
        std::string_view visibilityFlag = isVisible ? "T" : "F";

        const std::string message = simp::formatUpdateMessage(simp::MessageType::Visibility, identifier, visibilityFlag);
        connection.sendMessage(message);
    };
    if (visibilityProperty) {
        visibilityProperty->onChange(toggleVisibility);
    }
}

int PointDataMessageHandler::readIntValue(const std::vector<char>& message, size_t& offset) {
    std::string string_value;
    int value;
    bool isHex = false;

    while (!simp::isEndOfCurrentValue(message, offset)) {
        char c = message[offset];
        if (c == 'x' || c == 'X') isHex = true;
        string_value.push_back(c);
        offset++;
    }

    try {
        value = std::stoi(string_value, nullptr, isHex ? 16 : 10);
    }
    catch(std::exception &err) {
        throw simp::SimpError(
            simp::ErrorCode::Generic,
            fmt::format("Error when trying to parse the integer {}: {}", string_value, err.what())
        );
    }

    ++offset;
    return value;
}

float PointDataMessageHandler::readFloatValue(const std::vector<char>& message, size_t& offset) {
    std::string string_value;
    float value;

    while (!simp::isEndOfCurrentValue(message, offset)) {
        string_value.push_back(message[offset]);
        offset++;
    }

    try {
        // long l;

        // l = std::strtol(string_value.data(), (char**)NULL, 16);
        // value = (float)l;
        value = std::stof(string_value);

        // if ((*s == '0') && ((*s == 'X') || (*s == 'x'))) {
        //     unsigned long ul = strtoul(d, NULL, 16);
        //     return  (float) ul;
        // }
        // double d = atof(s, NULL);
        // return (float) d;
    }
    catch(std::exception &err) {
        throw simp::SimpError(
            simp::ErrorCode::Generic,
            fmt::format("Error when trying to parse the float {}: {}", string_value, err.what())
        );
    }

    ++offset;
    return value;
}

std::vector<float> PointDataMessageHandler::readColorMap(const std::vector<char>& message,
                                                         size_t& offset) {
    std::vector<float> colorMap;
    while (message[offset] != simp::SEP) {
        glm::vec4 color = readSingleColor(message, offset);
        
        // ColorMap should be stored in a sequential vector 
        // of floats for syncing between nodes and when 
        // loaded to as a texture in the shader.
        colorMap.push_back(color[0]);
        colorMap.push_back(color[1]);
        colorMap.push_back(color[2]);
        colorMap.push_back(color[3]);
    }
    
    offset++;
    return colorMap;
}

glm::vec4 PointDataMessageHandler::readSingleColor(const std::vector<char>& message, size_t& offset) {
    if (message[offset] != '[') {
        throw simp::SimpError(
            simp::ErrorCode::Generic,
            fmt::format("Expected to read '[', got {} in 'readColor'", message[offset])
        );
    }
    ++offset;

    float r = readFloatValue(message, offset);
    float g = readFloatValue(message, offset);
    float b = readFloatValue(message, offset);
    float a = readFloatValue(message, offset);

    if (message[offset] != ']') {
        throw simp::SimpError(
            simp::ErrorCode::Generic,
            fmt::format("Expected to read ']', got {} in 'readColor'", message[offset])
        );
    }
    ++offset;

    return { r, g, b, a };
}

glm::vec4 PointDataMessageHandler::readColor(const std::vector<char>& message, size_t& offset) {
    glm::vec4 color = readSingleColor(message, offset);
 
    ++offset;
    return color;
}

std::string PointDataMessageHandler::readString(const std::vector<char>& message, size_t& offset) {
    std::string value;
    while (!simp::isEndOfCurrentValue(message, offset)) {
        value.push_back(message[offset]);
        ++offset;
    }

    ++offset;
    return value;
}

std::vector<float> PointDataMessageHandler::readPointData(
    const std::vector<char>& message,
    size_t& offset,
    int nPoints,
    int dimensionality
) {
    std::vector<float> result;
    result.reserve(nPoints * dimensionality);

    while (!simp::isEndOfCurrentValue(message, offset)) {
        if (message[offset] != '[') {
            throw simp::SimpError(
                simp::ErrorCode::Generic,
                fmt::format("Expected to read '[', got {} in 'readPointData'", message[offset])
            );
        }
        ++offset;

        for (int i = 0; i < dimensionality; ++i) {
            result.push_back(readFloatValue(message, offset));
        }

        if (message[offset] != ']') {
            throw simp::SimpError(
                simp::ErrorCode::Generic,
                fmt::format("Expected to read ']', got {} in 'readPointData'", message[offset])
            );
        }
        ++offset;
    }

    ++offset;
    return result;
}

} // namespace openspace
