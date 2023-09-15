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

#define _USE_MATH_DEFINES

#include <modules/sonification/include/cosmicsonification.h>

#include <modules/cosmiclife/rendering/renderablecosmicpoints.h>
#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/query/query.h>
#include <math.h>

namespace {
    constexpr std::string_view _loggerCat = "CosmicSonification";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CosmicSonificationInfo =
    {
       "CosmicSonification",
       "Cosmic Sonification",
       "Sonification of the cosmic view of life labelsData"
    };

    constexpr openspace::properties::Property::PropertyInfo DistancePrecisionInfo = {
        "DistancePrecision",
        "Distance Precision",
        "The precision used for distances, given in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo AnglePrecisionInfo = {
        "AnglePrecision",
        "Angle Precision",
        "The precision used for angles, given in radians"
    };

    constexpr openspace::properties::Property::PropertyInfo AmplitudeModeInfo = {
        "AmplitudeMode",
        "Amplitude Mode",
        "The mode for what the amplitude is dependent on in the sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo FocusTypeInfo = {
        "FocusType",
        "Focus Type",
        "The type of focus for the sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo FilterInfo = {
        "Filter",
        "Filter",
        "The type of relationship that the sonification focuses on for the items"
    };

    constexpr openspace::properties::Property::PropertyInfo LockClosestItemInfo = {
        "LockClosestItem",
        "Lock focused item",
        "Locks the sonification to the current closest item in the scene"
    };

    constexpr openspace::properties::Property::PropertyInfo InvertPolarityInfo = {
        "InvertPolarity",
        "Invert polarity",
        "Decides what polarity is used for the sonification, true is inverted and false "
        "is normal (default is false)"
    };
} // namespace
#include "cosmicsonification_lua.inl"

namespace openspace {

CosmicSonification::CosmicSonification(const std::string& ip, int port)
    : SonificationBase(CosmicSonificationInfo, ip, port)
    , _distancePrecision(DistancePrecisionInfo, 0.1, 0.01, 1.0e3)
    , _anglePrecision(AnglePrecisionInfo, 0.05, 0.0, M_PI/2.0)
    , _amplitudeModeProperty(
        AmplitudeModeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _focusTypeProperty(
        FocusTypeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _filterProperty(
        FilterInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _lockClosestItem(LockClosestItemInfo, false)
    , _invertPolarity(InvertPolarityInfo, false)
{
    addProperty(_distancePrecision);
    _distancePrecision.setExponent(2);
    addProperty(_anglePrecision);

    // Add options to the amplitude mode drop down menu
    _amplitudeModeProperty.addOptions({
        { 0, "Always Full" },
        { 1, "Distance" },
        { 2, "Data" }
    });
    _amplitudeModeProperty.onChange([this]() { guiChangeAmpMode(); });
    addProperty(_amplitudeModeProperty);

    // Add options to the fopcus type drop down menu
    _focusTypeProperty.addOptions({
        { 0, "Panning" },
        { 1, "Distance" }
    });
    _focusTypeProperty.onChange([this]() { guiChangeFocusType(); });
    addProperty(_focusTypeProperty);

    // Add options to the filter drop down menu
    _filterProperty.addOptions({
        { 0, "Focus" },
        { 1, "All" },
        { 2, "Taxonomy" },
        { 3, "Habitat" },
        { 4, "Domesticated" }
    });
    _filterProperty.onChange([this]() { guiChangeFilter(); });
    addProperty(_filterProperty);

    addProperty(_lockClosestItem);
    _lockClosestItem.onChange([this]() { guiChangeLock(); });

    addProperty(_invertPolarity);
    _invertPolarity.onChange([this]() { guiChangePolarity(); });
}

void CosmicSonification::guiChangeAmpMode() {
    _amplitudeMode = static_cast<AmplitudeMode>(_amplitudeModeProperty.value());

    std::string label = "/AmpMode";
    std::vector<OscDataType> data(1);
    data[0] = static_cast<int>(_amplitudeMode);

    _connection->send(label, data);
}

void CosmicSonification::guiChangeFocusType() {
    _focusType = static_cast<FocusType>(_focusTypeProperty.value());

    std::string label = "/FocusType";
    std::vector<OscDataType> data(1);
    data[0] = static_cast<int>(_focusType);

    _connection->send(label, data);
}

void CosmicSonification::guiChangeFilter() {
    _filter = static_cast<Filter>(_filterProperty.value());

    std::string label = "/BirdFilter";
    std::vector<OscDataType> data(1);
    data[0] = static_cast<int>(_filter);

    _connection->send(label, data);
}

void CosmicSonification::guiChangeLock() {
    std::string label = "/BirdLock";
    std::vector<OscDataType> data(1);
    data[0] = static_cast<int>(_lockClosestItem);

    _connection->send(label, data);
}

void CosmicSonification::guiChangePolarity() {
    std::string label = "/InvertPolarity";
    std::vector<OscDataType> data(1);
    data[0] = static_cast<int>(_invertPolarity);

    _connection->send(label, data);
}

CosmicSonification::~CosmicSonification() {

}

void CosmicSonification::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    // Get the current focus
    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        return;
    }

    // Update node data
    for (NodeData& nodeData : _nodes) {
        std::string id = nodeData.identifier;
        if (nodeData.identifier == "Focus") {
            id = focusNode->identifier();
        }

        double distance = SonificationBase::calculateDistanceTo(
            camera,
            id,
            DistanceUnit::Meter
        );
        double angleH = SonificationBase::calculateAngleTo(camera, id);
        double angleV = SonificationBase::calculateElevationAngleTo(camera, id);

        if (abs(distance) < std::numeric_limits<double>::epsilon()) {
            continue;
        }

        // Check if this data is new, otherwise don't send it
        bool shouldSendData = false;
        if (std::abs(nodeData.distance - distance) > _distancePrecision ||
            std::abs(nodeData.horizontalAngle - angleH) > _anglePrecision ||
            std::abs(nodeData.verticalAngle - angleV) > _anglePrecision)
        {
            // Update the saved data for the node
            nodeData.distance = distance;
            nodeData.horizontalAngle = angleH;
            nodeData.verticalAngle = angleV;
            shouldSendData = true;
        }

        if (shouldSendData) {
            std::string label = "/" + nodeData.identifier;
            std::vector<OscDataType> oscData;

            if (nodeData.identifier == "Focus") {
                // Also send the name of the focus node
                oscData.push_back(id);
            }

            // Distance
            oscData.push_back(nodeData.distance);

            // Horizontal angle
            oscData.push_back(nodeData.horizontalAngle);

            // Vertical angle
            oscData.push_back(nodeData.verticalAngle);

            oscData.shrink_to_fit();
            _connection->send(label, oscData);
        }
    }

    // Update labels data
    for (Labels& label : _labels) {
        // Find the node
        SceneGraphNode* node = sceneGraphNode(label.identifier);
        if (!node) {
            LWARNING(fmt::format("Could not find node {}", label.identifier));
            continue;
        }
        glm::dmat4 transform = node->modelTransform();
        const glm::dvec3 parentWorldPos = node->parent()->worldPosition();
        const glm::dmat3 parentWorldRot = node->parent()->worldRotationMatrix();
        const glm::dvec3 parentWorldScale = node->parent()->worldScale();

        // Find the RenderableCosmicPoints
        Renderable* renderable = node->renderable();

        if (!renderable) {
            LWARNING(fmt::format("Could not find renderable for node {}", label.identifier));
            continue;
        }

        if (renderable->typeAsString() != "RenderableCosmicPoints") {
            LWARNING(fmt::format(
                "Not supported RenderableType {} detected", renderable->typeAsString()
            ));
            continue;
        }

        RenderableCosmicPoints* cosmicRenderable =
            reinterpret_cast<RenderableCosmicPoints*>(renderable);

        // Get labels if not set already
        if (!label.isInitialized) {
            label.labels = cosmicRenderable->labels();
            label.unit = cosmicRenderable->unit();
            label.data.resize(label.labels->entries.size());

            label.isInitialized = true;
        }

        // Update distances to all labels
        for (int i = 0; i < label.labels->entries.size(); ++i) {
            const glm::dvec3 labelPos =
                static_cast<glm::dvec3>(label.labels->entries[i].position);

            glm::dvec3 labelWorldPos =
                parentWorldPos + parentWorldRot * (parentWorldScale * labelPos);

            labelWorldPos *= toMeter(label.unit);
            glm::dvec3 transformedPos = transform * glm::vec4(labelWorldPos, 1.0);

            double distance = SonificationBase::calculateDistanceTo(
                camera,
                transformedPos,
                DistanceUnit::Meter
            );
            double angleH = SonificationBase::calculateAngleTo(
                camera,
                transformedPos
            );
            double angleV = SonificationBase::calculateElevationAngleTo(
                camera,
                transformedPos
            );

            if (abs(distance) < std::numeric_limits<double>::epsilon()) {
                continue;
            }

            // Check if this data is new, otherwise don't send it
            bool shouldSendData = false;
            if (std::abs(label.data[i].distance - distance) > _distancePrecision ||
                std::abs(label.data[i].horizontalAngle - angleH) > _anglePrecision ||
                std::abs(label.data[i].verticalAngle - angleV) > _anglePrecision)
            {
                // Update the saved data for the planet
                label.data[i].distance = distance;
                label.data[i].horizontalAngle = angleH;
                label.data[i].verticalAngle = angleV;
                shouldSendData = true;
            }

            if (shouldSendData) {
                std::string osclabel = "/" + label.identifier;
                std::vector<OscDataType> oscData;

                // Label name
                oscData.push_back(label.labels->entries[i].text);

                // Distance
                oscData.push_back(label.data[i].distance);

                // Horizontal angle
                oscData.push_back(label.data[i].horizontalAngle);

                // Vertical angle
                oscData.push_back(label.data[i].verticalAngle);

                oscData.shrink_to_fit();
                _connection->send(osclabel, oscData);
            }
        }
    }
}

void CosmicSonification::stop() {

}

void CosmicSonification::addNode(const std::string& nodeId) {
    if (std::find(_nodes.begin(), _nodes.end(), nodeId) != _nodes.end()) {
        LERROR(fmt::format("Identifier {} already exist in the internal nodes list", nodeId));
        return;
    }

    _nodes.push_back(nodeId);
}

void CosmicSonification::addLabelNode(const std::string& labelId) {
    if (std::find(_labels.begin(), _labels.end(), labelId) != _labels.end()) {
        LERROR(fmt::format("Identifier {} already exist in the internal labels list", labelId));
        return;
    }

    _labels.push_back(labelId);
}

scripting::LuaLibrary CosmicSonification::luaLibrary() {
    return {
        "cosmicSonification",
        {
            codegen::lua::AddNodes,
            codegen::lua::AddLabels
        }
    };
}

} // openspace namespace
