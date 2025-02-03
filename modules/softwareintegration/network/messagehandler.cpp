/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/softwareintegration/network/messagehandler.h>

#include <modules/softwareintegration/softwareintegrationmodule.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include <iomanip>

namespace {

constexpr const char* _loggerCat = "SoftwareIntegration MessageHandler: ";

} // namespace

namespace openspace::softwareintegration::messagehandler {

// Anonymous namespace
namespace {

CallbackMap callbacks{};
std::mutex callbacksMutex{};
size_t callbacksRetries{0};

void checkRenderable(
    const std::vector<std::byte>& message, size_t& messageOffset,
    std::shared_ptr<SoftwareConnection> connection, std::string& identifier
) {
    std::string guiName;

    try {
        // The following order of creating variables is the exact order they are received
        // in the message. If the order is not the same, the global variable
        // 'message offset' will be wrong
        simp::readValue(message, messageOffset, identifier);
        simp::readValue(message, messageOffset, guiName);
    }
    catch (const simp::SimpError& err) {
        LERROR(std::format("Error when reading identifier and guiName from message: {}", err.message));
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
        LDEBUG(std::format("No renderable with identifier '{}' was found. Creating it.", identifier));

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
            "openspace.setPropertyValueSingle('Modules.CefWebGui.Reload', nil)" // Reload WebGUI so that SoftwareIntegration GUI appears
        );

        auto reanchorCallback = [identifier] {
            global::scriptEngine->queueScript(
                "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
                "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '" + identifier + "')"
                "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')"
            );
        };
        addCallback(identifier, { reanchorCallback, { storage::Key::DataPoints }, "reanchorCallback" });

        softwareintegration::setDefaultDeltaTimeSteps();
    }
}

void onFixedColorChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    glm::vec4 color = std::any_cast<glm::vec4>(property->get());
    std::lock_guard guard(connection->outgoingMessagesMutex());

    {
        std::vector<std::byte> red;
        simp::toByteBuffer(red, 0, color.r);   
        connection->addToMessageQueue(identifier, simp::DataKey::Red, { red, 1 });
    }

    {
        std::vector<std::byte> green;
        simp::toByteBuffer(green, 0, color.g);        
        connection->addToMessageQueue(identifier, simp::DataKey::Green, { green, 1 });
    }

    {
        std::vector<std::byte> blue;
        simp::toByteBuffer(blue, 0, color.b);   
        connection->addToMessageQueue(identifier, simp::DataKey::Blue, { blue, 1 });
    }

    {
        std::vector<std::byte> alpha;
        simp::toByteBuffer(alpha, 0, color.a);   
        connection->addToMessageQueue(identifier, simp::DataKey::Alpha, { alpha, 1 });
    }
}

void onFixedPointSizeChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    float pointSizeValue = std::any_cast<float>(property->get());
    std::lock_guard guard(connection->outgoingMessagesMutex());

    std::vector<std::byte> size{};
    simp::toByteBuffer(size, 0, pointSizeValue);
    connection->addToMessageQueue(identifier, simp::DataKey::FixedSize, { size, 1 });
}

void onVisibilityChange(
    properties::Property* property,
    const std::string& identifier,
    std::shared_ptr<SoftwareConnection> connection
) {
    bool isVisible = std::any_cast<bool>(property->get());
    std::lock_guard guard(connection->outgoingMessagesMutex());

    std::vector<std::byte> visibility;
    simp::toByteBuffer(visibility, 0, isVisible);
    connection->addToMessageQueue(identifier, simp::DataKey::Visibility, { visibility, 1 });
}

void checkAddOnChangeCallback(
    const std::string& identifier,
    const std::string& propertyName,
    std::shared_ptr<SoftwareConnection> connection,
    const std::function<
        void(
            properties::Property* property,
            const std::string& identifier,
            std::shared_ptr<SoftwareConnection> connection
        )
    >& onChange
) {
    if (connection->hasPropertySubscription(identifier, propertyName)) {
        connection->setShouldNotSendData(identifier, propertyName);
    }

    // Shouldn't add another property subscription
    if (connection->hasPropertySubscription(identifier, propertyName)) return;

    // Weak pointer better for lambdas
    std::weak_ptr<SoftwareConnection> connWeakPtr{ connection };

    // Create and set onChange for property
    auto onChangeCallback = [identifier, connWeakPtr, propertyName, onChange] {
        // Get renderable
        auto r = renderable(identifier);
        if (!r) return;

        // Get property
        auto property = r->property(propertyName);
        if (!property || connWeakPtr.expired()) return;

        auto update = [propertyName, identifier, connWeakPtr, onChange] {
            if (connWeakPtr.expired()) return;
            auto connection = connWeakPtr.lock();

            // Get renderable
            auto r = renderable(identifier);
            if (!r) return;

            // Get property
            auto property = r->property(propertyName);
            if (!property) return;

            if (connWeakPtr.lock()->hasPropertySubscription(identifier, propertyName)) return;

            if (!connection->isConnected()) {
                connection->removePropertySubscription(identifier, propertyName);
                return;
            }

            if (!connection->shouldSendData(identifier, propertyName)) {
                return;
            }

            onChange(property, identifier, connection);
        };
        auto conn = connWeakPtr.lock();
        conn->addPropertySubscription(propertyName, identifier, update);
    };
    addCallback(
        identifier,
        {
            onChangeCallback,
            {},
            std::format("onChangeCallback on property {}", propertyName),
        }
    );
}

bool handleSingleFloatValue(
    const std::vector<std::byte>& message,
    size_t& offset,
    const std::string& identifier,
    const simp::DataKey& dataKey,
    const std::string& propertyName,
    std::shared_ptr<SoftwareConnection> connection = nullptr,
    const std::function<
        void(
            properties::Property* property,
            const std::string& identifier,
            std::shared_ptr<SoftwareConnection> connection
        )
    >& onChangeCallback = nullptr
) {
    float newValue;
    try {
        simp::readValue(message, offset, newValue);
    }
    catch (const simp::SimpError& err) {
        LERROR(std::format(
            "Error when parsing float in {} message: {}",
            simp::getStringFromDataKey(dataKey), err.message
        ));
        return false;
    }

    auto setValueCallback = [identifier, newValue, propertyName] {
        // Get renderable
        auto r = renderable(identifier);
        if (!r) return;

        // Get property of renderable
        auto property = r->property(propertyName);
        if (!property) return;

        // Update property of renderable
        auto currentValue = std::any_cast<float>(property->get());
        if (abs(newValue - currentValue) > std::numeric_limits<float>::epsilon()) {
            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.{}', {});",
                    identifier, propertyName, ghoul::to_string(newValue)
                )
            );
        }
    };
    addCallback(
        identifier,
        {
            setValueCallback,
            {},
            std::format("Callback for {} on property {}", simp::getStringFromDataKey(dataKey), propertyName),
        }
    );

    if (onChangeCallback && connection) {
        checkAddOnChangeCallback(identifier, propertyName, connection, onChangeCallback);
    }

    return true;
}

bool handleColorValue(
    const std::string& identifier,
    const glm::vec4& _newColor,
    const std::string& propertyName,
    std::shared_ptr<SoftwareConnection> connection = nullptr,
    const std::function<
        void(
            properties::Property* property,
            const std::string& identifier,
            std::shared_ptr<SoftwareConnection> connection
        )
    >& onChangeCallback = nullptr
) {
    auto setColorCallback = [identifier, _newColor, propertyName] {
        // Get renderable
        auto r = renderable(identifier);
        if (!r) return;

        // Get color of renderable
        auto property = r->property(propertyName);
        if (!property) return;

        auto currentColor = std::any_cast<glm::vec4>(property->get());
        
        // Update new color channel values
        auto newColor = _newColor;
        for (glm::vec4::length_type i = 0; i < glm::vec4::length(); ++i) {
            if (newColor[i] < 0) {
                newColor[i] = currentColor[i];
            }
        }

        // Update color of renderable
        if (glm::any(glm::epsilonNotEqual(newColor, currentColor, std::numeric_limits<float>::epsilon()))) {
            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.{}', {});",
                    identifier, propertyName, ghoul::to_string(newColor)
                )
            );
        }
    };
    addCallback(
        identifier, 
        {
            setColorCallback, 
            {}, 
            std::format("Callback on property {}", propertyName), 
        }
    );

    if (onChangeCallback && connection) {
        checkAddOnChangeCallback(identifier, propertyName, connection, onChangeCallback);
    }

    return true;
}

bool handleDateValue(
    const std::string& identifier,
    const glm::ivec3& _newDate,
    const std::string& propertyName,
    std::shared_ptr<SoftwareConnection> connection = nullptr,
    const std::function<
        void(
            properties::Property* property,
            const std::string& identifier,
            std::shared_ptr<SoftwareConnection> connection
        )
    >& onChangeCallback = nullptr
) {
    auto setDateCallback = [identifier, _newDate, propertyName] {
        // Get renderable
        auto r = renderable(identifier);
        if (!r) return;

        // Get date value of renderable
        auto property = r->property(propertyName);
        if (!property) return;
        
        auto currentDate = std::any_cast<glm::ivec3>(property->get());

        // Update new date values
        auto newDate = _newDate;
        for (glm::ivec3::length_type i = 0; i < glm::ivec3::length(); ++i) {
            // Keep the parts of currentDate that won't be updated
            if (newDate[i] < 0) {
                newDate[i] = currentDate[i];
            }
        }

        // Update date of renderable
        if (glm::any(glm::notEqual(newDate, currentDate))) {
            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.{}', {});",
                    identifier, propertyName, ghoul::to_string(newDate)
                )
            );
        }
    };
    addCallback(
        identifier, 
        {
            setDateCallback, 
            {}, 
            std::format("Callback on property {}", propertyName), 
        }
    );

    if (onChangeCallback && connection) {
        checkAddOnChangeCallback(identifier, propertyName, connection, onChangeCallback);
    }

    return true;
}

bool handleBoolValue(
    const std::vector<std::byte>& message,
    size_t& offset,
    const std::string& identifier,
    const simp::DataKey& dataKey,
    const std::string& propertyName,
    const std::vector<storage::Key>& _waitFor,
    std::shared_ptr<SoftwareConnection> connection = nullptr,
    const std::function<
        void(
            properties::Property* property,
            const std::string& identifier,
            std::shared_ptr<SoftwareConnection> connection
        )
    >& onChangeCallback = nullptr
) {
    bool newValue;
    try {
        simp::readValue(message, offset, newValue);
    }
    catch (const simp::SimpError& err) {
        LERROR(std::format(
            "Error when parsing bool in DATA.{} message: {}",
            simp::getStringFromDataKey(dataKey), err.message
        ));
        return false;
    }

    auto setEnabledCallback = [identifier, propertyName, newValue] {
        // Get renderable
        auto r = renderable(identifier);
        if (!r) return;

        // Get property
        auto property = r->property(propertyName);
        if (!property) return;

        // Update bool property of renderable
        auto currentValue = std::any_cast<bool>(property->get());
        if (newValue != currentValue) {
            std::string newValueString = newValue ? "true" : "false";
            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.{}', {});",
                    identifier, propertyName, newValueString
                )
            );
        }
    };
    std::vector<storage::Key> waitFor{};
    if (newValue) {
        waitFor = _waitFor;
    }
    addCallback(
        identifier,
        {
            setEnabledCallback,
            waitFor,
            std::format("Callback on property {}", propertyName),
        }
    );

    if (onChangeCallback && connection) {
        checkAddOnChangeCallback(identifier, propertyName, connection, onChangeCallback);
    }

    return true;
}

bool handleStringValue(
    const std::vector<std::byte>& message,
    size_t& offset,
    const std::string& identifier,
    const simp::DataKey& dataKey,
    const std::string& propertyName,
    std::shared_ptr<SoftwareConnection> connection = nullptr,
    const std::function<
        void(
            properties::Property* property,
            const std::string& identifier,
            std::shared_ptr<SoftwareConnection> connection
        )
    >& onChangeCallback = nullptr
) {
    std::string newStringValue;
    try {
        simp::readValue(message, offset, newStringValue);
    }
    catch (const simp::SimpError& err) {
        LERROR(std::format(
            "Error when parsing string in DATA.{} message: {}",
            simp::getStringFromDataKey(dataKey), err.message
        ));
        return false;
    }

    auto setStringCallback = [identifier, propertyName, newStringValue] {
        // Get renderable
        auto r = renderable(identifier);
        if (!r) return;

        // Get property
        auto property = r->property(propertyName);
        if (!property) return;

        // Update string property of renderable
        auto currentStringValue = property->stringValue();
        if (newStringValue != currentStringValue) {
            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.{}', \"{}\");",
                    identifier, propertyName, newStringValue
                )
            );
        }
    };
    addCallback(
        identifier,
        {
            setStringCallback,
            {},
            std::format("Callback for {} on property {}", simp::getStringFromDataKey(dataKey), propertyName),
        }
    );

    if (onChangeCallback && connection) {
        checkAddOnChangeCallback(identifier, propertyName, connection, onChangeCallback);
    }

    return true;
}

void handleDataMessage(const std::vector<std::byte>& message, std::shared_ptr<SoftwareConnection> connection) {    
    size_t offset = 0;
    std::string identifier;

    checkRenderable(message, offset, connection, identifier);

    glm::vec4 newColor{ -1.0 };
    bool hasNewColor = false;
    glm::vec4 newColormapNanColor{ -1.0 };
    bool hasNewNanColor = false;
    glm::ivec3 newVelocityDateRecorded{ -1 };
    bool hasNewVelocityDateRecorded = false;

    while (offset < message.size()) {
        std::string dataKeyStr;
        simp::DataKey dataKey;
        try {
            simp::readValue(message, offset, dataKeyStr);
            dataKey = simp::getDataKey(dataKeyStr);
        }
        catch (const simp::SimpError& err) {
            LERROR(std::format("Error when reading data message: {}", err.message));
            return;
        }
        LDEBUG(std::format(
            "Handling '{}':",
            simp::getStringFromDataKey(dataKey)
        ));
        // Handle multi-valued data key
        if (dataKey == simp::DataKey::X
            || dataKey == simp::DataKey::Y
            || dataKey == simp::DataKey::Z
            || dataKey == simp::DataKey::U
            || dataKey == simp::DataKey::V
            || dataKey == simp::DataKey::W
            || dataKey == simp::DataKey::ColormapReds
            || dataKey == simp::DataKey::ColormapGreens
            || dataKey == simp::DataKey::ColormapBlues
            || dataKey == simp::DataKey::ColormapAlphas
            || dataKey == simp::DataKey::ColormapAttributeData
            || dataKey == simp::DataKey::LinearSizeAttributeData
        ) {
            // Add values to syncable storage
            std::vector<std::byte> dataBuffer;
            int32_t nValues = -1;
            try {
                simp::readValue(message, offset, nValues);
                if (nValues < 0) {
                    throw simp::SimpError(std::format(
                        "Number of values should be >0. Got {}",
                        nValues
                    ));
                }
                size_t nBytesToCopy = static_cast<int64_t>(nValues) * 4; // Should be positive int
                dataBuffer.resize(nBytesToCopy);
                std::memcpy(
                    dataBuffer.data(),
                    message.data() + offset,
                    nBytesToCopy
                );
                offset += nBytesToCopy;
            }
            catch (const simp::SimpError& err) {
                if (nValues != -1) {
                    LERROR(std::format(
                        "Error when reading {} values in {} message: {}",
                        nValues, dataKeyStr, err.message
                    ));
                }
                else {
                    LERROR(std::format(
                        "Error when parsing number of values in {} message: {}",
                        dataKeyStr, err.message
                    ));
                }
                break;
            }

            // Get module
            auto module = global::moduleEngine->module<SoftwareIntegrationModule>();
            if (!module) continue;
            module->storeData(identifier, dataKey, std::move(dataBuffer));
            continue;
        }

        // Handle single-valued data key

        if (dataKey == simp::DataKey::PointUnit) {
            if (!handleStringValue(message, offset, identifier, dataKey, "PointUnit")) break;
        }
        // Handle fixed color
        else if (dataKey == simp::DataKey::Red) {
            if (!simp::readColorChannel(message, offset, dataKey, newColor, 0)) break;
            hasNewColor = true;
        }
        else if (dataKey == simp::DataKey::Green) {
            if (!simp::readColorChannel(message, offset, dataKey, newColor, 1)) break;
            hasNewColor = true;
        }
        else if (dataKey == simp::DataKey::Blue) {
            if (!simp::readColorChannel(message, offset, dataKey, newColor, 2)) break;
            hasNewColor = true;
        }
        else if (dataKey == simp::DataKey::Alpha) {
            if (!simp::readColorChannel(message, offset, dataKey, newColor, 3)) break;
            hasNewColor = true;
        }
        // Handle color mode
        else if (dataKey == simp::DataKey::ColormapEnabled) {
            if (!handleBoolValue(
                message,
                offset,
                identifier,
                dataKey,
                "ColormapEnabled",
                { storage::Key::Colormap, storage::Key::ColormapAttrData }
            )) {
                break;
            }
        }
        // Handle colormap min
        else if (dataKey == simp::DataKey::ColormapMin) {
            if (!handleSingleFloatValue(message, offset, identifier, dataKey, "ColormapMin")) break;
        }
        // Handle colormap max
        else if (dataKey == simp::DataKey::ColormapMax) {
            if (!handleSingleFloatValue(message, offset, identifier, dataKey, "ColormapMax")) break;
        }
        // Handle colormap NaN color
        else if (dataKey == simp::DataKey::ColormapNanR) {
            if (!simp::readColorChannel(message, offset, dataKey, newColormapNanColor, 0)) break;
            hasNewNanColor = true;
        }
        else if (dataKey == simp::DataKey::ColormapNanG) {
            if (!simp::readColorChannel(message, offset, dataKey, newColormapNanColor, 1)) break;
            hasNewNanColor = true;
        }
        else if (dataKey == simp::DataKey::ColormapNanB) {
            if (!simp::readColorChannel(message, offset, dataKey, newColormapNanColor, 2)) break;
            hasNewNanColor = true;
        }
        else if (dataKey == simp::DataKey::ColormapNanA) {
            if (!simp::readColorChannel(message, offset, dataKey, newColormapNanColor, 3)) break;
            hasNewNanColor = true;
        }
        // Handle colormap NaN mode
        else if (dataKey == simp::DataKey::ColormapNanMode) {
            if (!handleEnumValue<simp::ColormapNanRenderMode>(message, offset, identifier, dataKey, "ColormapNanMode")) break;
        }
        else if (dataKey == simp::DataKey::FixedSize) {
            if (!handleSingleFloatValue(message, offset, identifier, dataKey, "Size", connection, &onFixedPointSizeChange)) break;
        }
        else if (dataKey == simp::DataKey::LinearSizeEnabled) {
            if (!handleBoolValue(
                message,
                offset,
                identifier,
                dataKey,
                "LinearSizeEnabled",
                { storage::Key::LinearSizeAttrData }
            )) {
                break;
            }
        }
        else if (dataKey == simp::DataKey::LinearSizeMin) {
            if (!handleSingleFloatValue(message, offset, identifier, dataKey, "LinearSizeMin")) break;
        }
        else if (dataKey == simp::DataKey::LinearSizeMax) {
            if (!handleSingleFloatValue(message, offset, identifier, dataKey, "LinearSizeMax")) break;
        }
        else if (dataKey == simp::DataKey::Visibility) {
            if (!handleBoolValue(
                message,
                offset,
                identifier,
                dataKey,
                "Enabled",
                {},
                connection,
                &onVisibilityChange
            )) {
                break;
            }
        }
        else if (dataKey == simp::DataKey::VelocityDistanceUnit) {
            if (!handleStringValue(message, offset, identifier, dataKey, "VelocityDistanceUnit")) break;
        }
        else if (dataKey == simp::DataKey::VelocityTimeUnit) {
            if (!handleStringValue(message, offset, identifier, dataKey, "VelocityTimeUnit")) break;
        }
        else if (dataKey == simp::DataKey::VelocityYearRecorded) {
            if (!simp::readDateValue(message, offset, dataKey, newVelocityDateRecorded, 0)) break;
            hasNewVelocityDateRecorded = true;
        }
        else if (dataKey == simp::DataKey::VelocityMonthRecorded) {
            if (!simp::readDateValue(message, offset, dataKey, newVelocityDateRecorded, 1)) break;
            hasNewVelocityDateRecorded = true;
        }
        else if (dataKey == simp::DataKey::VelocityDayRecorded) {
            if (!simp::readDateValue(message, offset, dataKey, newVelocityDateRecorded, 2)) break;
            hasNewVelocityDateRecorded = true;
        }
        else if (dataKey == simp::DataKey::VelocityNanMode) {
            if (!handleEnumValue<simp::VelocityNanRenderMode>(message, offset, identifier, dataKey, "VelocityNanMode")) break;
        }
        else if (dataKey == simp::DataKey::VelocityEnabled) {
            if (!handleBoolValue(
                message,
                offset,
                identifier,
                dataKey,
                "MotionEnabled",
                { storage::Key::VelocityData }
            )) {
                break;
            }
        }
    }

    if (hasNewColor) {
        handleColorValue(identifier, newColor, "Color", connection, &onFixedColorChange);
    }
    if (hasNewNanColor) {
        handleColorValue(identifier, newColormapNanColor, "ColormapNanColor");
    }
    if (hasNewVelocityDateRecorded) {
        handleDateValue(identifier, newVelocityDateRecorded, "VelocityDateRecorded");
    }
}

void handleRemoveSGNMessage(const std::vector<std::byte>& message, std::shared_ptr<SoftwareConnection> connection) {
	size_t messageOffset = 0;

    std::string identifier;
    try {
        simp::readValue(message, messageOffset, identifier);
    }
    catch (const simp::SimpError& err) {
        LERROR(std::format("Error when reading message: {}", err.message));
        return;
    }
    
    const std::string currentAnchor = global::navigationHandler->orbitalNavigator().anchorNode()->identifier();

	if (currentAnchor == identifier) {
		// If the deleted node is the current anchor, first change focus to the Sun
		global::scriptEngine->queueScript(
			"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Sun')"
			"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')"
		);
	}
	global::scriptEngine->queueScript(
		"openspace.removeSceneGraphNode('" + identifier + "');"
	);

    connection->removeSceneGraphNode(identifier);

	LDEBUG(std::format("Scene graph node '{}' removed.", identifier));
}

} // namespace

void addCallback(const std::string& identifier, const Callback& newCallback) {
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

void handleMessage(IncomingMessage& incomingMessage) {
	if(incomingMessage.connection.expired()) {
		LDEBUG(std::format("Trying to handle message from disconnected peer. Aborting."));
		return;
	}

	auto connectionPtr = incomingMessage.connection.lock();

    const simp::MessageType messageType = incomingMessage.type;
	std::vector<std::byte>& message = incomingMessage.content;

	switch (messageType) {
		case simp::MessageType::Connection: {
			LDEBUG(std::format("Message recieved... Connection: {}", connectionPtr->id()));
			if (connectionPtr->handshakeHasBeenMade()) {
                LERROR(std::format("Connection {} is already connected. Can't connect again.", connectionPtr->id()));
                return;
            }
            size_t offset = 0;
            std::string software;
            try {
                simp::readValue(message, offset, software);
            }
            catch (const simp::SimpError& err) {
                LERROR(std::format(
                    "Error when parsing software name in {} message: {}",
                    simp::getStringFromMessageType(simp::MessageType::Connection), err.message
                ));
                break;
            }

            std::string sendBack = software + simp::DELIM;

            // Send back message to software to complete handshake            
            std::vector<std::byte> subject;
            simp::toByteBuffer(subject, 0, sendBack);

			connectionPtr->sendMessage(messageType, subject);
			LINFO(std::format("OpenSpace has connected with {} through socket", software));
            connectionPtr->setHandshakeHasBeenMade();
            break;
		}
		case simp::MessageType::Data: {
			LINFO(std::format("Data message recieved on connection {}.", connectionPtr->id()));
			handleDataMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::RemoveSceneGraphNode: {
			LINFO(std::format("Remove Scene Graph Node message recieved on connection {}.", connectionPtr->id()));
			handleRemoveSGNMessage(message, connectionPtr);
			break;
		}
		default: {
			LERROR(std::format(
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
            LDEBUG(std::format("Callbacks for {}:", identifier));
            const SceneGraphNode* sgn = global::renderEngine->scene()->sceneGraphNode(identifier);
            if (!sgn) throw std::exception{};

            auto r = renderable(identifier);
            if (!r) throw std::exception{};

            auto softwareIntegrationModule = global::moduleEngine->module<SoftwareIntegrationModule>();

            auto callbacksIt = callbackList.begin();
            while (callbacksIt != callbackList.end()) {
                auto& [callback, waitForData, description] = *callbacksIt;

                try {
                    for (auto& waitFor : waitForData) {
                        if (!softwareIntegrationModule->dataLoaded(identifier, waitFor)) {
                            LINFO(std::format(
                                "Callback '{}' NOT executed. Waiting for '{}':",
                                description, storage::getStorageKeyString(waitFor)
                            ));
                            throw std::exception{};
                        }
                    }

                    callback();
                    callbacksIt = callbackList.erase(callbacksIt);
                    LDEBUG(std::format("Callback '{}' executed", description));
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
            LDEBUG(std::format("Error when trying to run callback: {}", err.what()));
            break;
        }
    }
}

} // namespace openspace::softwareintegration::messagehandler
