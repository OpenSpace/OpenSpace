/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/softwareintegration/messagehandler.h>
#include <modules/softwareintegration/softwareintegrationmodule.h>
#include <modules/softwareintegration/utils.h>

#include <openspace/navigation/navigationhandler.h>
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

namespace openspace::softwareintegration::network {

// Anonymous namespace
namespace {

CallbackMap callbacks{};
std::mutex callbacksMutex{};
size_t callbacksRetries{0};

const Renderable* getRenderable(const std::string& identifier) {
    return renderable(identifier);
}

void checkRenderable(
    const std::vector<char>& message, size_t& messageOffset,
    std::shared_ptr<SoftwareConnection> connection, std::string& identifier
) {
    std::string guiName;

    try {
        // The following order of creating variables is the exact order they are received
        // in the message. If the order is not the same, the global variable
        // 'message offset' will be wrong
        identifier = simp::readString(message, messageOffset);
        guiName = simp::readString(message, messageOffset);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading identifier and guiName from message: {}", err.message));
        return;
    }

    connection->addSceneGraphNode(identifier);

    auto r = renderable(identifier);
    bool hasCallbacks = false;
    {
        std::lock_guard guard(callbacksMutex);
        hasCallbacks = callbacks.count(identifier) > 0;
    }
    if (!r && !hasCallbacks) {
        LDEBUG(fmt::format("No renderable with identifier '{}' was found. Creating it.", identifier));

        // Create a renderable, since it didn't exist
        using namespace std::string_literals;
        ghoul::Dictionary renderablePointsCloud;
        renderablePointsCloud.setValue("Type", "RenderablePointsCloud"s);
        renderablePointsCloud.setValue("Identifier", identifier);
        renderablePointsCloud.setValue("Name", guiName);

        ghoul::Dictionary gui;
        gui.setValue("Name", guiName);
        gui.setValue("Path", "/Software Integration"s);

        ghoul::Dictionary node;
        node.setValue("Identifier", identifier);
        node.setValue("Renderable", renderablePointsCloud);
        node.setValue("GUI", gui);

        global::scriptEngine->queueScript(
            "openspace.addSceneGraphNode(" + ghoul::formatLua(node) + ")"
            "openspace.setPropertyValueSingle('Modules.CefWebGui.Reload', nil)", // Reload WebGUI so that SoftwareIntegration GUI appears
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void addCallback(
    const std::string& identifier,
    const Callback& newCallback
) {
    std::lock_guard guard(callbacksMutex);
    auto it = callbacks.find(identifier);
    if (it == callbacks.end()) {
        CallbackList newCallbackList{ newCallback };
        callbacks.emplace(identifier, newCallbackList);
    }
    else {
        it->second.push_back(newCallback);
    }
}

void onFixedColorChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        connection->removePropertySubscription(property->identifier(), identifier);
        return;
    }

    // auto propertySubscription = connection->getPropertySubscription(identifier, property->identifier());
    // if (!propertySubscription) return;
    // if (!propertySubscription->shouldSendMessage) {
    //     propertySubscription->shouldSendMessage = true;
    //     return;
    // }

    glm::vec4 color = std::any_cast<glm::vec4>(property->get());
    
    const std::string message = simp::formatColorMessage(identifier, color);
    connection->sendMessage(message);
}

void onOpacityChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        connection->removePropertySubscription(property->identifier(), identifier);
        return;
    }

    // auto propertySubscription = connection->getPropertySubscription(identifier, property->identifier());
    // if (!propertySubscription) return;
    // if (!propertySubscription->shouldSendMessage) {
    //     propertySubscription->shouldSendMessage = true;
    //     return;
    // }

    float value = std::any_cast<float>(property->get());
    std::string hex_value = simp::floatToHex(value);

    const std::string message = simp::formatUpdateMessage(simp::MessageType::Opacity, identifier, hex_value);
    connection->sendMessage(message);
}

void onFixedPointSizeChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        connection->removePropertySubscription(property->identifier(), identifier);
        return;
    }

    // auto propertySubscription = connection->getPropertySubscription(identifier, property->identifier());
    // if (!propertySubscription) return;
    // if (!propertySubscription->shouldSendMessage) {
    //     propertySubscription->shouldSendMessage = true;
    //     return;
    // }

    float value = std::any_cast<float>(property->get());
    std::string hex_value = simp::floatToHex(value);

    const std::string message = simp::formatUpdateMessage(simp::MessageType::FixedSize, identifier, hex_value);
    connection->sendMessage(message);
}

void onVisibilityChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        connection->removePropertySubscription(property->identifier(), identifier);
        return;
    }

    // auto propertySubscription = connection->getPropertySubscription(identifier, property->identifier());
    // if (!propertySubscription) return;
    // if (!propertySubscription->shouldSendMessage) {
    //     propertySubscription->shouldSendMessage = true;
    //     return;
    // }

    bool isVisible = std::any_cast<bool>(property->get());
    std::string_view visibilityFlag = isVisible ? "T" : "F";

    const std::string message = simp::formatUpdateMessage(simp::MessageType::Visibility, identifier, visibilityFlag);
    connection->sendMessage(message);
}

void handlePointDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    size_t nPoints;
    size_t dimensionality;
    std::vector<float> points;

    try {
        // The following order of creating variables is the exact order they are received
        // in the message. If the order is not the same, the global variable
        // 'message offset' will be wrong
        nPoints = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        dimensionality = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        simp::readPointData(message, messageOffset, nPoints, dimensionality, points);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading point data message: {}", err.message));
        return;
    }

    // Use the renderable identifier as the data key
    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    module->storeData(identifier, storage::Key::DataPoints, std::move(points));

    auto reanchorCallback = [identifier] {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
            "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '" + identifier + "')"
            "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { reanchorCallback, { storage::Key::DataPoints }, "reanchorCallback" });
}

void handleVelocityDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    LWARNING(fmt::format("handleVelocityDataMessage()"));
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    std::string velocityDistanceUnitString;
    std::string velocityTimeUnitString;
    simp::VelocityNaNRenderMode velocityNaNMode;
    size_t nVelocities;
    size_t dimensionality;
    std::vector<float> velocities;

    try {
        // The following order of creating variables is the exact order they are received
        // in the message. If the order is not the same, the global variable
        // 'message offset' will be wrong
        velocityDistanceUnitString = simp::readString(message, messageOffset);
        velocityTimeUnitString = simp::readString(message, messageOffset);
        std::string velocityNaNModeStr = simp::readString(message, messageOffset);
        velocityNaNMode = simp::getVelocityNaNRenderMode(velocityNaNModeStr);
        if (velocityNaNMode == simp::VelocityNaNRenderMode::Unknown) {
            throw simp::SimpError(
                fmt::format(
                    "'{}' is not recognized as a velocity NaN render mode", 
                    velocityNaNModeStr
                )
            );
        }
        nVelocities = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        dimensionality = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        simp::readPointData(message, messageOffset, nVelocities, dimensionality, velocities);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading point data message: {}", err.message));
        return;
    }

    // Set units first to make sure they're available when converting during data loading
    auto velocityUnitsCallback = [identifier, velocityDistanceUnitString, velocityTimeUnitString, connection] {
        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.VelocityDistanceUnit', \"{}\");",
                identifier, velocityDistanceUnitString
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.VelocityTimeUnit', \"{}\");",
                identifier, velocityTimeUnitString
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { velocityUnitsCallback, {}, "velocityUnitsCallback" });

    // Use the renderable identifier as the data key
    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    module->storeData(identifier, storage::Key::VelocityData, std::move(velocities));

    auto velocityNaNModeCallback = [identifier, velocityNaNMode, connection] {
        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.VelocityNaNMode', {});",
                identifier, static_cast<int>(velocityNaNMode)
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { velocityNaNModeCallback, {}, "velocityNaNModeCallback" });

    // TODO: Fix! Executes too soon!
    auto enableMotionCallback = [identifier] {
        // Set large time steps for the GUI (so you for example 
        // can see the movement of stars at 5000 years/second)
        // Values set in seconds: Real time, 5k years, 
        // 10k year, 50k year, 100k year, 500k year, 1M year
        std::string largeTimeSteps = "{ 1.0, 157680000000.0, 315360000000.0,"
                                     " 1576800000000.0, 3153600000000.0,"
                                     " 15768000000000.0, 3153600000000.0 }";
        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.time.setDeltaTimeSteps({});",
                largeTimeSteps
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        
        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.MotionEnabled', {});",
                identifier, "true"
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(
        identifier, 
        { 
            enableMotionCallback, 
            { storage::Key::VelocityData }, 
            "Enable motion mode, wait for VelocityData" 
        }
    );
}

void handleFixedColorMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    glm::vec4 color;
    try {
        color = simp::readColor(message, messageOffset);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading fixed color message: {}", err.message));
        return;
    }
    
    // Create weak_ptr, safer than shared_ptr for lambdas
    std::weak_ptr<SoftwareConnection> connWeakPtr{ connection };

    auto setFixedColorCallback = [identifier, color, connWeakPtr] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find renderable {} while setting color", 
                identifier
            ));
            return;
        }

        // Get color of renderable
        properties::Property* colorProperty = r->property("Color");
        
        if (!colorProperty || connWeakPtr.expired()) return;
        // auto conn = connWeakPtr.lock()

        // auto propertySub = connection->getPropertySubscription(identifier, colorProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Update color of renderable
        glm::vec4 currentColor = std::any_cast<glm::vec4>(colorProperty->get());
        if (currentColor != color) {
            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.Color', {});",
                    identifier, ghoul::to_string(color)
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }

        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapEnabled', {});",
                identifier, "false"
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { setFixedColorCallback, {}, "setFixedColorCallback" });

    // Create and set onChange for color
    auto onChangeColorCallback = [identifier, connWeakPtr] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find renderable {} to set onFixedColorChange", 
                identifier
            ));
            return;
        }

        // Get color of renderable
        properties::Property* colorProperty = r->property("Color");

        if (!colorProperty || connWeakPtr.expired()) return;
        auto updateColor = [colorProperty, identifier, connWeakPtr] {
            if (!colorProperty || connWeakPtr.expired()) return;
            onFixedColorChange(colorProperty, identifier, connWeakPtr.lock());
        };
        auto conn = connWeakPtr.lock();
        conn->addPropertySubscription(colorProperty->identifier(), identifier, updateColor);
    };
    addCallback(identifier, { onChangeColorCallback, {}, "onChangeColorCallback" });
}

void handleColormapMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    float min;
    float max;
    simp::ColormapNaNRenderMode colormapNaNMode;
    glm::vec4 colormapNaNColor;
    size_t nColors;
    std::vector<float> colormap;
    try {
        min = simp::readFloatValue(message, messageOffset);
        max = simp::readFloatValue(message, messageOffset);
        std::string colormapNaNModeString = simp::readString(message, messageOffset);
        colormapNaNMode = simp::getColormapNaNRenderMode(colormapNaNModeString);
        switch (colormapNaNMode) {
            case simp::ColormapNaNRenderMode::FixedColor:
                colormapNaNColor = simp::readColor(message, messageOffset);
                break;
            case simp::ColormapNaNRenderMode::Hide: // Nothing to read
                break;
            default: // simp::ColormapNaNRenderMode::Unknown
                throw simp::SimpError(fmt::format(
                    "OpenSpace doesn't support the colormap NaN render mode '{}'.",
                    colormapNaNModeString
                ));
                break;
        }
        nColors = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        simp::readColormap(message, messageOffset, nColors, colormap);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading colormap message: {}", err.message));
        return;
    }

    // Use the renderable identifier as the data key
    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    module->storeData(identifier, storage::Key::Colormap, std::move(colormap));

    auto colormapLimitsCallback = [identifier, min, max, connection] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) return;

        properties::Property* colormapMinProperty = r->property("ColormapMin");
        // auto minPropertySub = connection->getPropertySubscription(identifier, colormapMinProperty->identifier());
        // if (minPropertySub) {
        //     minPropertySub->shouldSendMessage = false;
        // }

        float colormapMin = std::any_cast<float>(colormapMinProperty->get());
        if (min != colormapMin) {
            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapMin', {});",
                    identifier, ghoul::to_string(min)
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }

        properties::Property* colormapMaxProperty = r->property("ColormapMax");
        // auto maxPropertySub = connection->getPropertySubscription(identifier, colormapMaxProperty->identifier());
        // if (maxPropertySub) {
        //     maxPropertySub->shouldSendMessage = false;
        // }
        float colormapMax = std::any_cast<float>(colormapMaxProperty->get());
        if (max != colormapMax) {
            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapMax', {});",
                    identifier, ghoul::to_string(max)
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    };
    addCallback(identifier, { colormapLimitsCallback, {}, "colormapLimitsCallback" });

    auto colormapNaNModeCallback = [identifier, colormapNaNMode, colormapNaNColor, connection] {
        if (colormapNaNMode == simp::ColormapNaNRenderMode::FixedColor) {
            // Get renderable
            auto r = getRenderable(identifier);
            if (!r) return;

            // Get colormapNaNColor of renderable
            properties::Property* colormapNaNColorProperty = r->property("ColormapNaNColor");
            glm::vec4 propertyColormapNaNColor = std::any_cast<glm::vec4>(colormapNaNColorProperty->get());

            // Update colormapNaNColor of renderable
            if (propertyColormapNaNColor != colormapNaNColor) {
                global::scriptEngine->queueScript(
                    fmt::format(
                        "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapNaNColor', {});",
                        identifier, ghoul::to_string(colormapNaNColor)
                    ),
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
        }

        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapNaNMode', {});",
                identifier, static_cast<int>(colormapNaNMode)
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { colormapNaNModeCallback, {}, "colormapNaNModeCallback" });

    auto enableColormapCallback = [identifier] {
        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapEnabled', {});",
                identifier, "true"
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    // Callback 
    std::vector<storage::Key> dataToWaitFor{ storage::Key::Colormap, storage::Key::ColormapAttrData };
    addCallback(identifier, { enableColormapCallback, std::move(dataToWaitFor), "enableColormapCallback"  });
}

void handleAttributeDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    std::string usedFor;
    size_t nValues;
    std::vector<float> attributeData;
    try {
        usedFor = simp::readString(message, messageOffset);
        nValues = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        attributeData.reserve(nValues);
        for (size_t i = 0; i < nValues; ++i)
            attributeData.push_back(simp::readFloatValue(message, messageOffset));
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading message with scalars for colormap: {}", err.message));
        return;
    }

    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    storage::Key key = storage::Key::Unknown;

    if (storage::hasStorageKey(usedFor)) {
        key = storage::getStorageKey(usedFor);
    }
    else {
        LERROR(fmt::format(
            "The received attribute data had the \"usedFor\" value {}, which is unrecognized.",
            usedFor
        ));
        return;
    }

    module->storeData(identifier, key, std::move(attributeData));

    std::string callbackDescription = "handleAttributeDataMessage, key=" + storage::getStorageKeyString(key);
    switch (key) {
        case storage::Key::ColormapAttrData : {
            auto callback = [identifier] {
                global::scriptEngine->queueScript(
                    fmt::format(
                        "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapEnabled', {});",
                        identifier, "true"
                    ),
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            };
            addCallback(identifier, { callback, { key, storage::Key::Colormap }, callbackDescription });
            break;
        }
        case storage::Key::LinearSizeAttrData: {
            auto callback = [identifier] {
                global::scriptEngine->queueScript(
                    fmt::format(
                        "openspace.setPropertyValueSingle('Scene.{}.Renderable.LinearSizeEnabled', {});",
                        identifier, "true"
                    ),
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            };
            addCallback(identifier, { callback, { key }, callbackDescription });
            break;
        }
        default:
            break;
    }
}

void handleOpacityMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    float opacity;
    try {
        opacity = simp::readFloatValue(message, messageOffset);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading opacity message: {}", err.message));
        return;
    }

    // Create weak_ptr, safer than shared_ptr for lambdas
    std::weak_ptr<SoftwareConnection> connWeakPtr{ connection };

    auto setOpacityCallback = [identifier, opacity, connWeakPtr] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find the renderable {} while trying to set opacity", 
                identifier
            ));
            return;
        }

        // Get opacity of renderable
        properties::Property* opacityProperty = r->property("Opacity");
        
        if (!opacityProperty || connWeakPtr.expired()) return;
        // auto conn = connWeakPtr.lock()
        
        // auto propertySub = connection->getPropertySubscription(identifier, opacityProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Update opacity of renderable
        float currentOpacity = std::any_cast<float>(opacityProperty->get());
        if (currentOpacity != opacity) {
            std::string script = fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.Opacity', {});",
                identifier, ghoul::to_string(opacity)
            );
            global::scriptEngine->queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    };
    addCallback(identifier, { setOpacityCallback, {}, "setOpacityCallback" });

    // Create and set onChange for opacity
    auto onChangeOpacityCallback = [identifier, connWeakPtr] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find the renderable {} while trying to set opacity", 
                identifier
            ));
            return;
        }

        // Get opacity of renderable
        properties::Property* opacityProperty = r->property("Opacity");
        
        if (!opacityProperty || connWeakPtr.expired()) return;
        auto updateOpacity = [opacityProperty, identifier, connWeakPtr] {
            if (!opacityProperty || connWeakPtr.expired()) return;
            onOpacityChange(opacityProperty, identifier, connWeakPtr.lock());
        };
        auto conn = connWeakPtr.lock();
        conn->addPropertySubscription(opacityProperty->identifier(), identifier, updateOpacity);
    };
    addCallback(identifier, { onChangeOpacityCallback, {}, "onChangeOpacityCallback" });
}

void handleFixedPointSizeMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    float size;
    try {
        size = simp::readFloatValue(message, messageOffset);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading fixed point size message: {}", err.message));
        return;
    }

    // Create weak_ptr, safer than shared_ptr for lambdas
    std::weak_ptr<SoftwareConnection> connWeakPtr{ connection };

    auto setFixedPointSizeCallback = [identifier, size, connWeakPtr] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find renderable {} while setting point size", 
                identifier
            ));
            return;
        }

        // Get size of renderable
        properties::Property* sizeProperty = r->property("Size");

        if (!sizeProperty || connWeakPtr.expired()) return;
        // auto conn = connWeakPtr.lock()

        // auto propertySub = connection->getPropertySubscription(identifier, sizeProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Update size of renderable
        float currentSize = std::any_cast<float>(sizeProperty->get());
        if (currentSize != size) {
            // TODO: Add interpolation to script, but do not send back
            // updates to external software until the interpolation is done
            // Use this: "openspace.setPropertyValueSingle('Scene.{}.Renderable.Size', {}, 1);",
            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.Size', {});",
                    identifier, ghoul::to_string(size)
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }

        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.LinearSizeEnabled', {});",
                identifier, "false"
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { setFixedPointSizeCallback, {}, "setFixedPointSizeCallback" });
    
    // Create and set onChange for point size
    auto onChangeSizeCallback = [identifier, connWeakPtr] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find renderable {} to set onFixedPointSizeChange", 
                identifier
            ));
            return;
        }

        // Get size of renderable
        properties::Property* sizeProperty = r->property("Size");

        if (!sizeProperty || connWeakPtr.expired()) return;
        auto updateSize = [sizeProperty, identifier, connWeakPtr] {
            if (!sizeProperty || connWeakPtr.expired()) return;
            onFixedPointSizeChange(sizeProperty, identifier, connWeakPtr.lock());
        };
        auto conn = connWeakPtr.lock();
        conn->addPropertySubscription(sizeProperty->identifier(), identifier, updateSize);
    };
    addCallback(identifier, { onChangeSizeCallback, {}, "onChangeSizeCallback" });
}

void handleLinearPointSizeMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    float size;
    float min;
    float max;
    try {
        size = simp::readFloatValue(message, messageOffset);
        min = simp::readFloatValue(message, messageOffset);
        max = simp::readFloatValue(message, messageOffset);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading linear point size message: {}", err.message));
        return;
    }

    auto linearSizeCallback = [identifier, size, min, max] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) return;

        // Get size from renderable
        properties::Property* sizeProperty = r->property("Size");
        auto propertyAny = sizeProperty->get();
        float propertySize = std::any_cast<float>(propertyAny);

        // Update size of renderable
        if (propertySize != size) {
            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.Size', {});",
                    identifier, ghoul::to_string(size)
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }

        properties::Property* linearSizeMinProperty = r->property("LinearSizeMin");
        float linearSizeMin = std::any_cast<float>(linearSizeMinProperty->get());
        if (min != linearSizeMin) {
            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.LinearSizeMin', {});",
                    identifier, ghoul::to_string(min)
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }

        properties::Property* linearSizeMaxProperty = r->property("LinearSizeMax");
        float linearSizeMax = std::any_cast<float>(linearSizeMaxProperty->get());
        if (max != linearSizeMax) {
            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.LinearSizeMax', {});",
                    identifier, ghoul::to_string(max)
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    };
    addCallback(identifier, { linearSizeCallback, {}, "linearSizeCallback" });

    auto enableLinearSizeCallback = [identifier] {
        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.LinearSizeEnabled', {});",
                identifier, "true"
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(
        identifier,
        {
            enableLinearSizeCallback, 
            { storage::Key::LinearSizeAttrData },
            "enableLinearSizeCallback"
        }
    );
}

void handleVisibilityMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    std::string visibilityMessage;
    try {
        visibilityMessage = simp::readString(message, messageOffset);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading visibility message: {}", err.message));
        return;
    }

    // Create weak_ptr, safer than shared_ptr for lambdas
    std::weak_ptr<SoftwareConnection> connWeakPtr{ connection };

    const bool visibility = visibilityMessage == "T";

    auto setVisibilityCallback = [identifier, visibility, connWeakPtr] {        
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find renderable {} while setting visibility", 
                identifier
            ));
            return;
        }

        // Get visibility of renderable
        properties::Property* visibilityProperty = r->property("Enabled");
        
        if (!visibilityProperty || connWeakPtr.expired()) return;
        // auto conn = connWeakPtr.lock()

        // Get visibility from renderable
        // properties::Property* enabledProperty = r->property("Enabled");
        // auto propertySub = connection->getPropertySubscription(identifier, enabledProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Toggle visibility of renderable
        bool currentVisibility = std::any_cast<bool>(visibilityProperty->get());
        if (currentVisibility != visibility) {
            const std::string vis = visibility ? "true" : "false";
            std::string script = fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.Enabled', {});",
                identifier, vis
            );
            global::scriptEngine->queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    };
    addCallback(identifier, { setVisibilityCallback, {}, "setVisibilityCallback" });

    // Create and set onChange for visibility
    auto onChangeVisibilityCallback = [identifier, connWeakPtr] {
        // Get renderable
        auto r = getRenderable(identifier);
        if (!r) {
            LWARNING(fmt::format(
                "Couldn't find renderable {} to set onVisibilityChange", 
                identifier
            ));
            return;
        }

        // Get visibility of renderable
        properties::Property* visibilityProperty = r->property("Enabled");
        
        if (!visibilityProperty || connWeakPtr.expired()) return;
        auto toggleVisibility = [visibilityProperty, identifier, connWeakPtr] {
            if (!visibilityProperty || connWeakPtr.expired()) return;
            onVisibilityChange(visibilityProperty, identifier, connWeakPtr.lock());    
        };
        auto conn = connWeakPtr.lock();
        conn->addPropertySubscription(visibilityProperty->identifier(), identifier, toggleVisibility);
    };
    addCallback(identifier, { onChangeVisibilityCallback, {}, "onChangeVisibilityCallback" });
}

void handleRemoveSGNMessage(const std::vector<char>& message,std::shared_ptr<SoftwareConnection> connection) {
	size_t messageOffset = 0;
    std::string identifier;

    try {
        identifier = simp::readString(message, messageOffset);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading message: {}", err.message));
        return;
    }
    
    const std::string currentAnchor =
	global::navigationHandler->orbitalNavigator().anchorNode()->identifier();

	if (currentAnchor == identifier) {
		// If the deleted node is the current anchor, first change focus to the Sun
		global::scriptEngine->queueScript(
			"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Sun')"
			"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')",
			scripting::ScriptEngine::RemoteScripting::Yes
		);
	}
	global::scriptEngine->queueScript(
		"openspace.removeSceneGraphNode('" + identifier + "');",
		scripting::ScriptEngine::RemoteScripting::Yes
	);

    connection->removeSceneGraphNode(identifier);

	LDEBUG(fmt::format("Scene graph node '{}' removed.", identifier));
}

} // namespace

void handleMessage(IncomingMessage& incomingMessage) {
	if(incomingMessage.connection.expired()) {
		LDEBUG(fmt::format("Trying to handle message from disconnected peer. Aborting."));
		return;
	}

	auto connectionPtr = incomingMessage.connection.lock();

	const simp::MessageType messageType = incomingMessage.type;
	std::vector<char>& message = incomingMessage.content;

	switch (messageType) {
		case simp::MessageType::Connection: {
			LDEBUG(fmt::format("Message recieved... Connection: {}", connectionPtr->id()));
			size_t offset = 0;
			const std::string software = simp::readString(message, offset);

			// Send back message to software to complete handshake
			connectionPtr->sendMessage(simp::formatConnectionMessage(software));
			LINFO(fmt::format("OpenSpace has connected with {} through socket", software));
			break;
		}
		case simp::MessageType::PointData: {
			LDEBUG("Message recieved.. Point data");
			handlePointDataMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::VelocityData: {
			LDEBUG("Message recieved... Velocity data");
			handleVelocityDataMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::RemoveSceneGraphNode: {
			LDEBUG(fmt::format("Message recieved.. Remove SGN"));
			handleRemoveSGNMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Color: {
			LDEBUG(fmt::format("Message recieved.. New color"));
			handleFixedColorMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Colormap: {
			LDEBUG(fmt::format("Message recieved.. New colormap"));
			handleColormapMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::AttributeData: {
			LDEBUG(fmt::format("Message recieved.. New attribute data"));
			handleAttributeDataMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Opacity: {
			LDEBUG(fmt::format("Message recieved.. New Opacity"));
			handleOpacityMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::FixedSize: {
			LDEBUG(fmt::format("Message recieved.. New size"));
			handleFixedPointSizeMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::LinearSize: {
			LDEBUG(fmt::format("Message recieved.. New linear size"));
			handleLinearPointSizeMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Visibility: {
			LDEBUG(fmt::format("Message recieved.. New visibility"));
			handleVisibilityMessage(message, connectionPtr);
			break;
		}
		default: {
			LERROR(fmt::format(
				"Unsupported message type: {}", incomingMessage.rawMessageType
			));
			break;
		}
	}
}

void postSyncCallbacks() {
    std::lock_guard guard(callbacksMutex);
    // Check if the scene graph node has been created.
    // If so, call the corresponding callback functions to set up any subscriptions
    auto callbackMapIt = callbacks.begin();
    while (callbackMapIt != callbacks.end()) {
        auto& [identifier, callbackList] = *callbackMapIt;
        
        try {
            const SceneGraphNode* sgn = global::renderEngine->scene()->sceneGraphNode(identifier);
            if (!sgn) throw std::exception{};

            auto r = getRenderable(identifier);
            if (!r) throw std::exception{};

            auto softwareIntegrationModule = global::moduleEngine->module<SoftwareIntegrationModule>();

            auto callbacksIt = callbackList.begin();
            while (callbacksIt != callbackList.end()) {
                auto& [callback, waitForData, description] = *callbacksIt;

                try {
                    for (auto& waitFor : waitForData) {
                        if (!softwareIntegrationModule->dataLoaded(identifier, waitFor)) {
                            throw std::exception{};
                        }
                    }

                    callback();
                    callbacksIt = callbackList.erase(callbacksIt);
                }
                catch (std::exception&) {
                    ++callbacksIt;
                }
            }

            if (callbackList.empty()) {
                callbackMapIt = callbacks.erase(callbackMapIt);
                callbacksRetries = 0;
            } else {
                callbackMapIt++;
            }
        }
        catch(std::exception &err) {
            ++callbacksRetries;
            ghoul_assert(callbacksRetries < 10, "Too many callback retries");
            LDEBUG(fmt::format("Error when trying to run callback: {}", err.what()));
            break;
        }
    }
}

} // namespace openspace::softwareintegration::messagehandler
