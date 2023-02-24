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

#include <modules/sonification/include/cosmicsonification.h>

#include <modules/cosmiclife/rendering/renderablecosmicpoints.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/query/query.h>

namespace {
    constexpr std::string_view _loggerCat = "CosmicSonification";
    constexpr int DistanceIndex = 0;
    constexpr int AngleIndex = 1;
    constexpr int NumDataItems = 2;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        CosmicSonificationInfo =
    {
       "CosmicViewSonification",
       "Cosmic View Sonification",
       "Sonification of the cosmic view of life data"
    };

} // namespace

namespace openspace {

CosmicSonification::CosmicSonification(const std::string& ip, int port)
    : SonificationBase(CosmicSonificationInfo, ip, port)
{
    _anglePrecision = 0.05;
    _distancePrecision = 0.1;

    // List all nodes to subscribe to
    _nodeData["Focus"] = std::vector<double>(NumDataItems);
    _nodeData["Cercopithecoidea_volume_center"] = std::vector<double>(NumDataItems);
    _nodeData["Hominoidea_volume_center"] = std::vector<double>(NumDataItems);
    _nodeData["Platyrrhini_volume_center"] = std::vector<double>(NumDataItems);
    _nodeData["Strepsirrhini_volume_center"] = std::vector<double>(NumDataItems);

    // List all labels to subscribe to
    _labelsData["birds_lineage_29"] = LabelsData();
}

CosmicSonification::~CosmicSonification() {

}

void CosmicSonification::update(const Camera* camera) {
    // Get the current focus
    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        return;
    }

    // Update node data
    for (auto& [identifier, data] : _nodeData) {
        std::string id = identifier;
        if (identifier == "Focus") {
            id = focusNode->identifier();
        }

        double distance = SonificationBase::calculateDistanceTo(
            camera,
            id,
            DistanceUnit::Meter
        );
        double angle = SonificationBase::calculateAngleTo(camera, id);

        if (abs(distance) < std::numeric_limits<double>::epsilon()) {
            continue;
        }

        // Check if this data is new, otherwise don't send it
        bool shouldSendData = false;
        if (std::abs(data[DistanceIndex] - distance) > _distancePrecision ||
            std::abs(data[AngleIndex] - angle) > _anglePrecision)
        {
            // Update the saved data for the planet
            data[DistanceIndex] = distance;
            data[AngleIndex] = angle;
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
            oscData.push_back(data[DistanceIndex]);

            // Angle
            oscData.push_back(data[AngleIndex]);

            oscData.shrink_to_fit();
            _connection->send(label, oscData);
        }
    }

    // Update labels data
    for (auto& [identifier, data] : _labelsData) {
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
        if (!data.isInitialized) {
            data.labels = cosmicRenderable->labels();
            data.prevValues.resize(
                data.labels->entries.size(),
                std::vector<double>(NumDataItems)
            );

            data.isInitialized = true;
        }

        // Update distances to all labels
        for (int i = 0; i < data.labels->entries.size(); ++i) {
            double distance = SonificationBase::calculateDistanceTo(
                camera,
                data.labels->entries[i].position,
                DistanceUnit::Meter
            );
            double angle = SonificationBase::calculateAngleTo(
                camera,
                data.labels->entries[i].position
            );

            if (abs(distance) < std::numeric_limits<double>::epsilon()) {
                continue;
            }

            // Check if this data is new, otherwise don't send it
            bool shouldSendData = false;
            if (std::abs(data.prevValues[i][DistanceIndex] - distance) > _distancePrecision ||
                std::abs(data.prevValues[i][AngleIndex] - angle) > _anglePrecision)
            {
                // Update the saved data for the planet
                data.prevValues[i][DistanceIndex] = distance;
                data.prevValues[i][AngleIndex] = angle;
                shouldSendData = true;
            }

            if (shouldSendData) {
                std::string label = "/" + identifier;
                std::vector<OscDataType> oscData;

                // Label name
                oscData.push_back(data.labels->entries[i].text);

                // Distance
                oscData.push_back(data.prevValues[i][DistanceIndex]);

                // Angle
                oscData.push_back(data.prevValues[i][AngleIndex]);

                oscData.shrink_to_fit();
                _connection->send(label, oscData);
            }
        }
    }
}

} // openspace namespace
