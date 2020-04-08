/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/autonavigation/atnodenavigator.h>

#include <modules/autonavigation/waypoint.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "AtNodeNavigator";

    constexpr const openspace::properties::Property::PropertyInfo OrbitSpeedFactorInfo = {
        "OrbitSpeedFactor",
        "Orbit Speed Factor",
        "Controls the speed of the orbiting around an anchor."
    };
} // namespace

namespace openspace::autonavigation {

AtNodeNavigator::AtNodeNavigator()
    : properties::PropertyOwner({ "AtNodeNavigator" })
    , _orbitSpeedFactor(OrbitSpeedFactorInfo, 0.5, 0.0, 20.0)
{
    addProperty(_orbitSpeedFactor);
    _behavior = Behavior::None;
}

AtNodeNavigator::~AtNodeNavigator() {} // NOLINT

void AtNodeNavigator::setNode(std::string identifier) {
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' to navigate in relation to.", identifier
        ));
        return;
    }
    _node = node;
}

void AtNodeNavigator::setBehavior(Behavior behavior) {
    _behavior = behavior;
}

void AtNodeNavigator::updateCamera(double deltaTime) {
    switch (_behavior)
    {
    case Behavior::Orbit:
        orbitNode(deltaTime);
        break;
    case Behavior::None:
        // Do nothing
        break;
    default:
        LERROR("Behavior type not implemented.");
        break;
    }
}

Camera* AtNodeNavigator::camera() const {
    return global::navigationHandler.camera();
}

// Move along the right vector for the camera, while looking at the center of the node
void AtNodeNavigator::orbitNode(double deltaTime) {
    ghoul_assert(_node != nullptr, "Node to orbit must be set!")
    glm::dvec3 prevPosition = camera()->positionVec3();
    glm::dquat prevRotation = camera()->rotationQuaternion();

    glm::dvec3 nodeCenter = _node->worldPosition();
    double distanceToNode = glm::distance(prevPosition, nodeCenter) - _node->boundingSphere();
    double orbitSpeed = distanceToNode * 0.1 * _orbitSpeedFactor; 

    const glm::dvec3 up = camera()->lookUpVectorWorldSpace();
    glm::dvec3 forward = prevRotation * glm::dvec3(0.0, 0.0, -1.0);
    glm::dvec3 right = glm::normalize(glm::cross(forward, up));

    glm::dvec3 newPosition = prevPosition + orbitSpeed * deltaTime * right;

    glm::dmat4 lookAtMat = glm::lookAt(newPosition, nodeCenter, up);
    glm::dquat newRotation = glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));

    camera()->setPositionVec3(newPosition);
    camera()->setRotation(newRotation);
}

} // namespace openspace::autonavigation
