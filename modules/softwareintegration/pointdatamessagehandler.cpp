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

namespace openspace {

using namespace softwareintegration;

void PointDataMessageHandler::handlePointDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
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

void PointDataMessageHandler::handleVelocityDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    LWARNING(fmt::format("PointDataMessageHandler::handleVelocityDataMessage()"));
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    size_t nVelocities;
    size_t dimensionality;
    std::vector<float> velocities;

    try {
        // The following order of creating variables is the exact order they are received
        // in the message. If the order is not the same, the global variable
        // 'message offset' will be wrong
        nVelocities = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        dimensionality = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        simp::readPointData(message, messageOffset, nVelocities, dimensionality, velocities);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading point data message: {}", err.message));
        return;
    }

    // Use the renderable identifier as the data key
    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    module->storeData(identifier, storage::Key::VelocityData, std::move(velocities));
    auto callback = [identifier] {
        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.MotionEnabled', {});",
                identifier, "true"
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { callback, { storage::Key::VelocityData }, "Enable motion mode, wait for VelocityData" });
}

void PointDataMessageHandler::handleFixedColorMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
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

    auto callback = [this, identifier, color, connection] {
        // Get renderable
        auto r = getRenderable(identifier);

        // Get color of renderable
        properties::Property* colorProperty = r->property("Color");
        glm::vec4 propertyColor = std::any_cast<glm::vec4>(colorProperty->get());

        // auto propertySub = connection->getPropertySubscription(identifier, colorProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Update color of renderable
        if (propertyColor != color) {
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
    addCallback(identifier, { callback, {}, "handleFixedColorMessage" });
}

void PointDataMessageHandler::handleColormapMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
    size_t messageOffset = 0;
    std::string identifier;

    checkRenderable(message, messageOffset, connection, identifier);

    float min;
    float max;
    simp::CmapNaNMode cmapNaNMode;
    glm::vec4 cmapNaNColor;
    size_t nColors;
    std::vector<float> colorMap;
    try {
        min = simp::readFloatValue(message, messageOffset);
        max = simp::readFloatValue(message, messageOffset);
        cmapNaNMode = simp::getCmapNaNMode(simp::readString(message, messageOffset));
        switch (cmapNaNMode) {
            case simp::CmapNaNMode::Color:
                cmapNaNColor = simp::readColor(message, messageOffset);
                break;
            case simp::CmapNaNMode::Hide: // Nothing to read
                break;
            default: // simp::CmapNaNMode::Unknown
                // TODO:  Throw SimpError
                break;
        }
        nColors = static_cast<size_t>(simp::readIntValue(message, messageOffset));
        simp::readColormap(message, messageOffset, nColors, colorMap);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format("Error when reading color map message: {}", err.message));
        return;
    }

    // Use the renderable identifier as the data key
    auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
    module->storeData(identifier, storage::Key::Colormap, std::move(colorMap));

    auto colormapLimitsCallback = [this, identifier, min, max, connection] {
        // Get renderable
        auto r = getRenderable(identifier);

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

    auto cmapNaNModeCallback = [this, identifier, cmapNaNMode, cmapNaNColor, connection] {

        if (cmapNaNMode == simp::CmapNaNMode::Color) {
            // Get renderable
            auto r = getRenderable(identifier);

            // Get cmapNaNColor of renderable
            properties::Property* cmapNaNColorProperty = r->property("CmapNaNColor");
            glm::vec4 propertyCmapNaNColor = std::any_cast<glm::vec4>(cmapNaNColorProperty->get());

            // Update cmapNaNColor of renderable
            if (propertyCmapNaNColor != cmapNaNColor) {
                global::scriptEngine->queueScript(
                    fmt::format(
                        "openspace.setPropertyValueSingle('Scene.{}.Renderable.CmapNaNColor', {});",
                        identifier, ghoul::to_string(cmapNaNColor)
                    ),
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
        }

        global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.CmapNaNMode', {});",
                identifier, static_cast<int>(cmapNaNMode)
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { cmapNaNModeCallback, {}, "cmapNaNModeCallback" });

    auto enableColormapCallback = [this, identifier] {
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

void PointDataMessageHandler::handleAttributeDataMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
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
        LERROR(fmt::format("Error when reading message with scalars for color map: {}", err.message));
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
            auto callback = [this, identifier] {
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
            auto callback = [this, identifier] {
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

void PointDataMessageHandler::handleOpacityMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
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

    auto callback = [this, identifier, opacity, connection] {
        // Get renderable
        auto r = getRenderable(identifier);

        // Get opacity from renderable
        properties::Property* opacityProperty = r->property("Opacity");
        auto propertyAny = opacityProperty->get();
        float propertyOpacity = std::any_cast<float>(propertyAny);

        // auto propertySub = connection->getPropertySubscription(identifier, opacityProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Update opacity of renderable
        if (propertyOpacity != opacity) {
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
    addCallback(identifier, { callback, {}, "handleOpacityMessage" });
}

void PointDataMessageHandler::handleFixedPointSizeMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
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

    auto callback = [this, identifier, size, connection] {
        // Get renderable
        auto r = getRenderable(identifier);

        // Get size from renderable
        properties::Property* sizeProperty = r->property("Size");
        auto propertyAny = sizeProperty->get();
        float propertySize = std::any_cast<float>(propertyAny);

        // auto propertySub = connection->getPropertySubscription(identifier, sizeProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Update size of renderable
        if (propertySize != size) {
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
    addCallback(identifier, { callback, {}, "handleFixedPointSizeMessage" });
}

void PointDataMessageHandler::handleLinearPointSizeMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
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

    auto linearSizeCallback = [this, identifier, size, min, max] {
        // Get renderable
        auto r = getRenderable(identifier);

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

    auto enableLinearSizeCallback = [this, identifier] {
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

void PointDataMessageHandler::handleVisibilityMessage(const std::vector<char>& message, std::shared_ptr<SoftwareConnection> connection) {
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

    auto callback = [this, identifier, visibilityMessage, connection] {
        // Get renderable
        // auto r = getRenderable(identifier);

        // Get visibility from renderable
        // properties::Property* enabledProperty = r->property("Enabled");
        // auto propertySub = connection->getPropertySubscription(identifier, enabledProperty->identifier());
        // if (propertySub) {
        //     propertySub->shouldSendMessage = false;
        // }

        // Toggle visibility from renderable
        const std::string visability = visibilityMessage == "T" ? "true" : "false";
        std::string script = fmt::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Enabled', {});",
            identifier, visability
        );
        global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };
    addCallback(identifier, { callback, {}, "handleVisibilityMessage" });
}

void PointDataMessageHandler::handleRemoveSGNMessage(const std::vector<char>& message,std::shared_ptr<SoftwareConnection> connection) {
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

void PointDataMessageHandler::postSync() {
    std::lock_guard guard(_onceNodeExistsCallbacksMutex);
    // Check if the scene graph node has been created.
    // If so, call the corresponding callback functions to set up any subscriptions
    auto callbackMapIt = _onceNodeExistsCallbacks.begin();
    while (callbackMapIt != _onceNodeExistsCallbacks.end()) {
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
                        if (softwareIntegrationModule->isSyncDataDirty(identifier, waitFor)) {
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
                callbackMapIt = _onceNodeExistsCallbacks.erase(callbackMapIt);
                _onceNodeExistsCallbacksRetries = 0;
            } else {
                callbackMapIt++;
            }
        }
        catch(std::exception &err) {
            ++_onceNodeExistsCallbacksRetries;
            ghoul_assert(_onceNodeExistsCallbacksRetries < 10, "Too many callback retries");
            LDEBUG(fmt::format("Error when trying to run callback: {}", err.what()));
            break;
        }
    }
}

const Renderable* PointDataMessageHandler::getRenderable(const std::string& identifier) {
    return renderable(identifier);
}

void PointDataMessageHandler::checkRenderable(
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

    const Renderable* r = renderable(identifier);
    bool hasCallbacks = false;
    {
        std::lock_guard guard(_onceNodeExistsCallbacksMutex);
        hasCallbacks = _onceNodeExistsCallbacks.count(identifier) > 0;
    }
    if (!r && !hasCallbacks) {
        LDEBUG(fmt::format("No renderable with identifier '{}' was found. Creating it.", identifier));

        // Create a renderable, since it didn't exist
        using namespace std::string_literals;
        ghoul::Dictionary renderablePointsCloud;
        renderablePointsCloud.setValue("Type", "RenderablePointsCloud"s);
        renderablePointsCloud.setValue("Identifier", identifier);

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
        auto subscriptionCallback = [this, identifier, connection] {
            subscribeToRenderableUpdates(identifier, connection);
        };
        addCallback(identifier, { subscriptionCallback, {}, "subscriptionCallback" });
    }
    else {
        subscribeToRenderableUpdates(identifier, connection);
    }
}

void PointDataMessageHandler::subscribeToRenderableUpdates(
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    // Get renderable
    auto aRenderable = getRenderable(identifier);
    if (!aRenderable) return;

    if (!connection->isConnected()) {
        LERROR(fmt::format(
            "Could not subscribe to updates for renderable '{}' due to lost connection",
            identifier
        ));
        return;
    }

    // Update color of renderable
    properties::Property* colorProperty = aRenderable->property("Color");
    if (colorProperty) {
        auto updateColor = [this, colorProperty, identifier, connection]() {
            onFixedColorChange(colorProperty, identifier, connection);
        };
        connection->addPropertySubscription(colorProperty->identifier(), identifier, updateColor);
    }

    // Update opacity of renderable
    properties::Property* opacityProperty = aRenderable->property("Opacity");
    if (opacityProperty) {
        auto updateOpacity = [this, opacityProperty, identifier, connection]() {
            onOpacityChange(opacityProperty, identifier, connection);
        };
        connection->addPropertySubscription(opacityProperty->identifier(), identifier, updateOpacity);
    }

    // Update size of renderable
    properties::Property* sizeProperty = aRenderable->property("Size");
    if (sizeProperty) {
        auto updateSize = [this, sizeProperty, identifier, connection] {
            onFixedPointSizeChange(sizeProperty, identifier, connection);
        };
        connection->addPropertySubscription(sizeProperty->identifier(), identifier, updateSize);
    }

    // Toggle visibility of renderable
    properties::Property* visibilityProperty = aRenderable->property("Enabled");
    if (visibilityProperty) {
        auto toggleVisibility = [this, visibilityProperty, identifier, connection] {
            onVisibilityChange(visibilityProperty, identifier, connection);    
        };
        connection->addPropertySubscription(visibilityProperty->identifier(), identifier, toggleVisibility);
    }
}

void PointDataMessageHandler::addCallback(
    const std::string& identifier,
    const Callback& newCallback
) {
    std::lock_guard guard(_onceNodeExistsCallbacksMutex);
    auto it = _onceNodeExistsCallbacks.find(identifier);
    if (it == _onceNodeExistsCallbacks.end()) {
        CallbackList newCallbackList{ newCallback };
        _onceNodeExistsCallbacks.emplace(identifier, newCallbackList);
    }
    else {
        it->second.push_back(newCallback);
    }
}

void PointDataMessageHandler::onFixedColorChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        SoftwareConnection::PointDataMessageHandlerFriends::removePropertySubscription(
            connection, property->identifier(), identifier
        );
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

void PointDataMessageHandler::onOpacityChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        SoftwareConnection::PointDataMessageHandlerFriends::removePropertySubscription(
            connection, property->identifier(), identifier
        );
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

void PointDataMessageHandler::onFixedPointSizeChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        SoftwareConnection::PointDataMessageHandlerFriends::removePropertySubscription(
            connection, property->identifier(), identifier
        );
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

void PointDataMessageHandler::onVisibilityChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    if (!connection->isConnected()) {
        SoftwareConnection::PointDataMessageHandlerFriends::removePropertySubscription(
            connection, property->identifier(), identifier
        );
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

} // namespace openspace
