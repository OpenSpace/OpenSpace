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

#include "modules/sonification/sonificationmodule.h"
#include "openspace/camera/camera.h"
#include "openspace/engine/globals.h"
#include "openspace/navigation/navigationhandler.h"
#include "openspace/navigation/orbitalnavigator.h"
#include "openspace/rendering/renderengine.h"

namespace {
    constexpr std::string_view _loggerCat = "CosmicSonification";
    constexpr double DistancePrecision = 1000.0;
    constexpr double MeterToKilometer = 1000.0;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo CosmicSonificationInfo = {
       "CosmicViewSonification",
       "Cosmic View Sonification",
       "Settings for the cosmic view sonificaiton"
    };

} // namespace

namespace openspace {

CosmicSonification::CosmicSonification()
    : PropertyOwner(CosmicSonificationInfo)
{
    _isRunning = true;
}

CosmicSonification::~CosmicSonification() {
    _isRunning = false;
    if (_thread.joinable()) {
        _thread.join();
    }
}

void CosmicSonification::start() {
    _thread = std::thread([this]() { threadMain(std::ref(_isRunning)); });
}

void CosmicSonification::threadMain(std::atomic<bool>& isRunning) {
    Scene* scene = nullptr;
    Camera* camera = nullptr;
    glm::dvec3 cameraPosition;
    const SceneGraphNode* focusNode = nullptr;

    while (isRunning) {
        // Get the scene
        scene = global::renderEngine->scene();
        if (!scene || scene->root()->children().size() < 1) {
            // Scene is empty or could not be found
            continue;
        }

        // Get the camera
        camera = scene->camera();
        if (!camera) {
            // Could not ´find camera
            continue;
        }
        cameraPosition = camera->positionVec3();

        // Get the current focus
        focusNode = global::navigationHandler->orbitalNavigator().anchorNode();

        subscripbeFocusDistance(scene, cameraPosition, focusNode);
    }
}

void CosmicSonification::subscripbeFocusDistance(const Scene* scene,
                                                 const glm::dvec3 cameraPosition,
                                                 const SceneGraphNode* focusNode)
{
    // Calculate distance to current focus
    glm::dvec3 cameraToNode = focusNode->worldPosition() - cameraPosition;
    double distance = glm::length(cameraToNode);

    // Convert to km
    distance /= MeterToKilometer;

    // Don't send if the data is the same
    if (abs(distance - _prevDistance) < DistancePrecision) {
        return;
    }
    _prevDistance = distance;

    // Create the data structore for the message
    std::string label = "/cosmicFocus";
    std::vector<SonificationModule::OscDataEntry> data(1);
    data[0].type = SonificationModule::OscDataType::Double;
    data[0].doubleValue = distance;

    // Send the message via Osc
    SonificationModule::ref().sendOscMessage(label, data);
}

} // openspace namespace
