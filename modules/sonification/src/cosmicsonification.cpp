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

namespace {
    constexpr std::string_view _loggerCat = "CosmicSonification";
    constexpr double AnglePrecision = 0.05;
    constexpr double DistancePrecision = 0.1;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CosmicSonificationInfo =
    {
       "CosmicSonification",
       "Cosmic Sonification",
       "Sonification of the cosmic view of life labelsData"
    };
} // namespace
#include "cosmicsonification_lua.inl"

namespace openspace {

CosmicSonification::CosmicSonification(const std::string& ip, int port)
    : SonificationBase(CosmicSonificationInfo, ip, port)
{
    _enabled = true;
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
    for (auto& [identifier, nodeData] : _nodes) {
        std::string id = identifier;
        if (identifier == "Focus") {
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
        if (std::abs(nodeData[DistanceIndex] - distance) > DistancePrecision ||
            std::abs(nodeData[HAngleIndex] - angleH) > AnglePrecision ||
            std::abs(nodeData[VAngleIndex] - angleV) > AnglePrecision)
        {
            // Update the saved data for the planet
            nodeData[DistanceIndex] = distance;
            nodeData[HAngleIndex] = angleH;
            nodeData[VAngleIndex] = angleV;
            shouldSendData = true;
        }

        if (shouldSendData) {
            std::string label = "/" + identifier;
            std::vector<OscDataType> oscData;

            if (identifier == "Focus") {
                // Also send the name of the focus node
                oscData.push_back(id);
            }

            // Distance
            oscData.push_back(nodeData[DistanceIndex]);

            // Horizontal angle
            oscData.push_back(nodeData[HAngleIndex]);

            // Vertical angle
            oscData.push_back(nodeData[VAngleIndex]);

            oscData.shrink_to_fit();
            _connection->send(label, oscData);
        }
    }

    // Update labels data
    for (auto& [identifier, labelsData] : _labels) {
        // Find the node
        SceneGraphNode* node = sceneGraphNode(identifier);
        if (!node) {
            LWARNING(fmt::format("Could not find node {}", identifier));
            continue;
        }

        // Find the RenderableCosmicPoints
        Renderable* renderable = node->renderable();

        if (!renderable) {
            LWARNING(fmt::format("Could not find renderable for node {}", identifier));
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
        if (!labelsData.isInitialized) {
            labelsData.labels = cosmicRenderable->labels();
            labelsData.unit = cosmicRenderable->unit();
            labelsData.data.resize(
                labelsData.labels->entries.size(),
                std::vector<double>(NumDataItems)
            );

            labelsData.isInitialized = true;
        }

        // Update distances to all labels
        for (int i = 0; i < labelsData.labels->entries.size(); ++i) {
            glm::vec3 scaledPos(labelsData.labels->entries[i].position);
            scaledPos *= toMeter(labelsData.unit);

            double distance = SonificationBase::calculateDistanceTo(
                camera,
                scaledPos,
                DistanceUnit::Meter
            );
            double angleH = SonificationBase::calculateAngleTo(
                camera,
                scaledPos
            );
            double angleV = SonificationBase::calculateElevationAngleTo(
                camera,
                scaledPos
            );

            if (abs(distance) < std::numeric_limits<double>::epsilon()) {
                continue;
            }

            // Check if this data is new, otherwise don't send it
            bool shouldSendData = false;
            if (std::abs(labelsData.data[i][DistanceIndex] - distance) > DistancePrecision ||
                std::abs(labelsData.data[i][HAngleIndex] - angleH) > AnglePrecision ||
                std::abs(labelsData.data[i][VAngleIndex] - angleV) > AnglePrecision)
            {
                // Update the saved data for the planet
                labelsData.data[i][DistanceIndex] = distance;
                labelsData.data[i][HAngleIndex] = angleH;
                labelsData.data[i][VAngleIndex] = angleV;
                shouldSendData = true;
            }

            if (shouldSendData) {
                std::string label = "/" + identifier;
                std::vector<OscDataType> oscData;

                // Label name
                oscData.push_back(labelsData.labels->entries[i].text);

                // Distance
                oscData.push_back(labelsData.data[i][DistanceIndex]);

                // Horizontal angle
                oscData.push_back(labelsData.data[i][HAngleIndex]);

                // Vertical angle
                oscData.push_back(labelsData.data[i][VAngleIndex]);

                oscData.shrink_to_fit();
                _connection->send(label, oscData);
            }
        }
    }
}

void CosmicSonification::stop() {

}

void CosmicSonification::addNode(const std::string& nodeId) {
    if (_nodes.find(nodeId) != _nodes.end()) {
        LERROR(fmt::format("Identifier {} already exist in the internal nodes list", nodeId));
        return;
    }

    _nodes[nodeId] = std::vector<double>(NumDataItems);
}

void CosmicSonification::addLabelNode(const std::string& labelId) {
    if (_labels.find(labelId) != _labels.end()) {
        LERROR(fmt::format("Identifier {} already exist in the internal labels list", labelId));
        return;
    }

    _labels[labelId] = LabelsData();
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
