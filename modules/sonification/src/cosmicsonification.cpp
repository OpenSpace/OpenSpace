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

    constexpr openspace::properties::Property::PropertyInfo BirdFilterInfo = {
        "BirdFilter",
        "Bird Filter",
        "The type of relationship that the sonification focuses on for the birds"
    };

    constexpr openspace::properties::Property::PropertyInfo LockClosestBirdInfo = {
        "LockClosestBird",
        "Lock Closest Bird",
        "Locks the sonification to the current closest bird in the scene"
    };
} // namespace
#include "cosmicsonification_lua.inl"

namespace openspace {

CosmicSonification::CosmicSonification(const std::string& ip, int port)
    : SonificationBase(CosmicSonificationInfo, ip, port)
    , _distancePrecision(DistancePrecisionInfo, 0.1, 0.01, 1.0e3)
    , _anglePrecision(AnglePrecisionInfo, 0.05, 0.0, M_PI/2.0)
    , _filter(
        BirdFilterInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _lockClosestBird(LockClosestBirdInfo, false)
{
    addProperty(_distancePrecision);
    _distancePrecision.setExponent(2);
    addProperty(_anglePrecision);

    // Add options to the bird filter drop down menu
    _filter.addOptions({
        { 0, "None" },
        { 1, "Taxonomy" },
        { 2, "Habitat" },
        { 3, "Domesticated" }
    });
    _filter.onChange([this]() { guiChangeFilter(); });
    addProperty(_filter);

    addProperty(_lockClosestBird);
    _lockClosestBird.onChange([this]() { guiChangeLock(); });
}

void CosmicSonification::guiChangeFilter() {
    _birdFilter = static_cast<BirdFilter>(_filter.value());

    std::string label = "/BirdFilter";
    std::vector<OscDataType> data(1);
    data[0] = static_cast<int>(_filter);

    _connection->send(label, data);
}

void CosmicSonification::guiChangeLock() {
    std::string label = "/BirdLock";
    std::vector<OscDataType> data(1);
    data[0] = static_cast<int>(_lockClosestBird);

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
            glm::vec3 scaledPos(label.labels->entries[i].position);
            scaledPos *= toMeter(label.unit);
            glm::vec3 transformedPos = transform * glm::vec4(scaledPos, 1.0);

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
