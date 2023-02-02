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
#include <openspace/query/query.h>
#include "glm/gtx/projection.hpp"
#include "glm/gtx/vector_angle.hpp"

namespace {
    constexpr std::string_view _loggerCat = "CosmicSonification";
    constexpr double DistancePrecision = 0.1;
    constexpr double AnglePrecision = 0.05;
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

        //subscripbeFocusDistance(scene, cameraPosition, focusNode);
        subscripbeNodeDistance(scene, camera, "Strepsirrhini_volume_center");
        subscripbeNodeDistance(scene, camera, "Platyrrhini_volume_center");
        subscripbeNodeDistance(scene, camera, "Catarrhini_volume_center");
        subscripbeNodeDistance(scene, camera, "Hominoidea_volume_center");
    }
}

void CosmicSonification::subscripbeFocusDistance(const Scene* scene,
                                                 const glm::dvec3 cameraPosition,
                                                 const SceneGraphNode* focusNode)
{
    // Calculate distance to current focus
    glm::dvec3 cameraToNode = focusNode->worldPosition() - cameraPosition;
    double distance = glm::length(cameraToNode);

    // Don't send if the data is the same
    if (abs(distance - _prevFocusDistance) < DistancePrecision) {
        return;
    }
    _prevFocusDistance = distance;

    // Create the data structore for the message
    std::string label = "/cosmicFocus";
    std::vector<SonificationModule::OscDataEntry> data(2);
    data[0].type = SonificationModule::OscDataType::Double;
    data[0].doubleValue = distance;

    data[1].type = SonificationModule::OscDataType::String;
    data[1].stringValue = focusNode->identifier();

    // Send the message via Osc
    SonificationModule::ref().sendOscMessage(label, data);
}

void CosmicSonification::subscripbeNodeDistance(const Scene* scene, const Camera* camera,
                                                const std::string& identifier)
{
    // Find node
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return;
    }

    // Find entry in subscription list
    auto findIdentifier = [identifier](std::pair<std::string, std::vector<double>> p) {
        return p.first == identifier;
    };
    bool isNew = false;
    auto it = std::find_if(_nodeSubscriptions.begin(), _nodeSubscriptions.end(), findIdentifier);
    if (it == _nodeSubscriptions.end()) {
        _nodeSubscriptions.push_back({ identifier, {0.0, 0.0} });
        it = _nodeSubscriptions.end() - 1;
        isNew = true;
    }

    // Calculate distance to current focus
    glm::dvec3 cameraToNode = node->worldPosition() - camera->positionVec3();
    double distance = glm::length(cameraToNode);

    // Don't send if the data is the same
    bool distanceDirty = false;
    if (abs(distance - (* it).second[0]) > DistancePrecision) {
        distanceDirty = true;
    }

    //Calculate angle from camera to the planet in the camera plane
    //Project v down to the camera plane, Pplane(v)
    //Pn(v) is v projected on the normal n of the plane
    //Pplane(v) = v - Pn(v)
    glm::dvec3 cameraToProjectedNode = cameraToNode -
        glm::proj(cameraToNode, camera->lookUpVectorWorldSpace());

    double angle = glm::orientedAngle(glm::normalize(camera->viewDirectionWorldSpace()),
        glm::normalize(cameraToProjectedNode),
        camera->lookUpVectorWorldSpace());

    bool angleDirty = false;
    if (abs(angle - (*it).second[1]) > AnglePrecision) {
        angleDirty = true;
    }

    if (isNew) {
        distanceDirty = true;
        angleDirty = true;
    }

    if (!distanceDirty && !angleDirty) {
        return;
    }

    if (distanceDirty) {
        (*it).second[0] = distance;
    }
    if (angleDirty) {
        (*it).second[1] = angle;
    }

    // Create the data structore for the message
    std::string label = "/" + identifier;
    std::vector<SonificationModule::OscDataEntry> data(2);
    data[0].type = SonificationModule::OscDataType::Double;
    data[0].doubleValue = distance;

    data[1].type = SonificationModule::OscDataType::Double;
    data[1].doubleValue = angle;

    // Send the message via Osc
    SonificationModule::ref().sendOscMessage(label, data);
}

} // openspace namespace
