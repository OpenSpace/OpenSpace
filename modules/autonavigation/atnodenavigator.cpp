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

#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/waypoint.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/rotate_vector.hpp>

namespace {
    constexpr const char* _loggerCat = "AtNodeNavigator";

    const double Epsilon = 1E-7;

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

const AtNodeNavigator::Behavior AtNodeNavigator::behavior() const {
    return _behavior;
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
    return global::navigationHandler->camera();
}

const SceneGraphNode* AtNodeNavigator::anchor() const {
    return global::navigationHandler->anchorNode();
}

// Move along the right vector for the camera, while looking at the center of the node
void AtNodeNavigator::orbitNode(double deltaTime) {
    ghoul_assert(anchor() != nullptr, "Node to orbit must be set!")
    const glm::dvec3 prevPosition = camera()->positionVec3();
    const glm::dquat prevRotation = camera()->rotationQuaternion();

    const glm::dvec3 up = camera()->lookUpVectorWorldSpace();
    const double speedFactor = 0.1 * _orbitSpeedFactor;

    glm::dvec3 nodeCenter = anchor()->worldPosition();
    double orbitRadius = glm::distance(prevPosition, nodeCenter);
    double distanceToSurface = orbitRadius - anchor()->boundingSphere();
    double orbitSpeed = distanceToSurface * speedFactor;

    // compute a new position along the orbit
    const glm::dquat lookAtNodeRotation = helpers::lookAtQuaternion(
        prevPosition,
        nodeCenter,
        up
    );
    const glm::dvec3 targetForward = lookAtNodeRotation * glm::dvec3(0.0, 0.0, -1.0);
    const glm::dvec3 rightOrbitTangent = glm::normalize(glm::cross(targetForward, up));

    glm::dvec3 newPosition = prevPosition + orbitSpeed * deltaTime * rightOrbitTangent;

    // adjust for numerical error
    const glm::dvec3 nodeToNewPos = newPosition - nodeCenter;
    const double nodeToPrevPosDistance = glm::distance(prevPosition, nodeCenter);
    const double distanceDiff = glm::length(nodeToNewPos) - nodeToPrevPosDistance;
    newPosition -= distanceDiff * glm::normalize(nodeToNewPos);

    // rotate with the orbit, but keep the relative orientation with regards to the anchor
    const glm::dquat localRotation = glm::inverse(lookAtNodeRotation) * prevRotation;
    const glm::dquat newLookAtRotation =
        helpers::lookAtQuaternion(newPosition, nodeCenter, up);

    const glm::dquat newRotation = newLookAtRotation * localRotation;

    camera()->setPositionVec3(newPosition);
    camera()->setRotation(newRotation);
}

} // namespace openspace::autonavigation
