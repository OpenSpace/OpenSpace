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

#include <modules/autonavigation/instruction.h>

#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PathInstruction";

    constexpr const char* KeyDuration = "Duration";
    constexpr const char* KeyStopAtTarget = "StopAtTarget";
    constexpr const char* KeyStopDetails = "StopDetails";
    constexpr const char* KeyBehavior = "Behavior";

    constexpr const char* KeyTarget = "Target";
    constexpr const char* KeyPosition = "Position";
    constexpr const char* KeyHeight = "Height";

    constexpr const char* KeyNavigationState = "NavigationState";
} // namespace

namespace openspace::autonavigation {

Instruction::Instruction(const ghoul::Dictionary& dictionary) {
    if (dictionary.hasValue<double>(KeyDuration)) {
        duration = dictionary.value<double>(KeyDuration);
    }

    // TODO: include info about pauses/stops
    if (dictionary.hasValue<bool>(KeyStopAtTarget)) {
        stopAtTarget = dictionary.value<bool>(KeyStopAtTarget);
    }

    if (dictionary.hasValue<ghoul::Dictionary>(KeyStopDetails)) {
        ghoul::Dictionary stopDictionary = 
            dictionary.value<ghoul::Dictionary>(KeyStopDetails);

        if (stopDictionary.hasValue<double>(KeyDuration)) {
            stopDuration = stopDictionary.value<double>(KeyDuration);
        }

        if (stopDictionary.hasValue<std::string>(KeyBehavior)) {
            stopBehavior = stopDictionary.value<std::string>(KeyBehavior);
        }
    }
}

Instruction::~Instruction() {}

TargetNodeInstruction::TargetNodeInstruction(const ghoul::Dictionary& dictionary) 
    : Instruction(dictionary)
{
    if (!dictionary.hasValue<std::string>(KeyTarget)) {
        throw ghoul::RuntimeError(
            "A camera path instruction requires a target node, to go to or use as reference frame."
        );
    }

    nodeIdentifier = dictionary.value<std::string>(KeyTarget);

    if (!sceneGraphNode(nodeIdentifier)) {
        throw ghoul::RuntimeError(fmt::format("Could not find target node '{}'", nodeIdentifier));
    }

    if (dictionary.hasValue<glm::dvec3>(KeyPosition)) {
        position = dictionary.value<glm::dvec3>(KeyPosition);
    }

    if (dictionary.hasValue<double>(KeyHeight)) {
        height = dictionary.value<double>(KeyHeight);
    }
}

std::vector<Waypoint> TargetNodeInstruction::getWaypoints() const {
    const SceneGraphNode* targetNode = sceneGraphNode(nodeIdentifier);
    if (!targetNode) {
        LERROR(fmt::format("Could not find target node '{}'", nodeIdentifier));
        return std::vector<Waypoint>();
    }

    glm::dvec3 targetPos;
    if (position.has_value()) {
        // note that the anchor and reference frame is our targetnode. 
        // The position in instruction is given is relative coordinates.
        targetPos = targetNode->worldPosition() +
            targetNode->worldRotationMatrix() * position.value();
    }
    else {
        // TODO: Instead of this case, allow the curve to set its final position

        //glm::dvec3 nodePos = targetNode->worldPosition();
        //glm::dvec3 nodeToPrev = lastWayPoint().position() - nodePos;
        //// TODO: compute position in a more clever way

        //const double radius = WaypointNodeDetails::findValidBoundingSphere(targetNode, _minAllowedBoundingSphere);
        //const double defaultHeight = 2 * radius;

        //bool hasHeight = props->height.has_value();
        //double height = hasHeight ? props->height.value() : defaultHeight;

        //// move target position out from surface, along vector to camera
        //targetPos = nodePos + glm::normalize(nodeToPrev) * (radius + height);


        // OBS! TEMPORARY!! TODO: fix so that a camera pose is optional in Waypoint
        const double radius = WaypointNodeDetails::findValidBoundingSphere(targetNode);
        targetPos = targetNode->worldPosition() + 3 * radius * glm::dvec3(1.0, 0.0, 0.0);
    }

    Camera* camera = global::navigationHandler.camera();

    glm::dmat4 lookAtMat = glm::lookAt(
        targetPos,
        targetNode->worldPosition(),
        camera->lookUpVectorWorldSpace()
    );

    glm::dquat targetRot = glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));

    Waypoint wp{ targetPos, targetRot, nodeIdentifier }; 
    return std::vector<Waypoint>({ wp });
}

NavigationStateInstruction::NavigationStateInstruction(
    const ghoul::Dictionary& dictionary): Instruction(dictionary)
{
    if (dictionary.hasValue<ghoul::Dictionary>(KeyNavigationState)) {
        auto navStateDict = dictionary.value<ghoul::Dictionary>(KeyNavigationState);

        openspace::documentation::testSpecificationAndThrow(
            NavigationState::Documentation(),
            navStateDict,
            "NavigationState"
        );

        navigationState = NavigationState(navStateDict);
    }
}

std::vector<Waypoint> NavigationStateInstruction::getWaypoints() const {
    Waypoint wp{ navigationState };
    return std::vector<Waypoint>({ wp });
}

} // namespace openspace::autonavigation
