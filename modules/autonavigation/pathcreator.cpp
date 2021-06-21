/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/autonavigation/pathcreator.h>

#include <modules/autonavigation/autonavigationmodule.h>
#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/waypoint.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PathCreator";
    constexpr const float Epsilon = 1e-5f;

    // TODO: where should this documentation be?
    struct [[codegen::Dictionary(PathInstruction)]] Parameters {
        enum class Type {
            Node,
            NavigationState
        };
        Type type;

        // The desired duration traversing the specified path segment should take
        std::optional<float> duration;

        // (Node): The target node of the camera path. Not optional for 'Node' instructions
        std::optional<std::string> target;

        // (Node): An optional position in relation to the target node, in model 
        // coordinates (meters)
        std::optional<glm::dvec3> position;

        // (Node): An optional height in relation to the target node, in meters
        std::optional<double> height;

        // (Node): If true, the up direction of the node is taken into account when 
        // computing the wayopoint for this instruction
        std::optional<bool> useTargetUpDirection;

        // (NavigationState): A navigation state that will be the target
        // of this path segment
        std::optional<ghoul::Dictionary> navigationState 
            [[codegen::reference("core_navigation_state")]];

        // A navigation state that determines the start state for the camera path
        std::optional<ghoul::Dictionary> startState
            [[codegen::reference("core_navigation_state")]];
    };
#include "pathinstruction_codegen.cpp"
} // namespace

namespace openspace::pathnavigation {

using NavigationState = interaction::NavigationHandler::NavigationState;

Path PathCreator::createPath(const ghoul::Dictionary& dictionary, Path::CurveType curveType) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::optional<float> duration = p.duration;

    bool hasStart = p.startState.has_value();
    Waypoint startPoint = hasStart ? Waypoint(p.startState.value()) : waypointFromCamera();

    // TODO: also handle curve type here
    
    std::vector<Waypoint> waypoints;
    switch (p.type) {
        case Parameters::Type::NavigationState: {
            if (!p.navigationState.has_value()) {
                throw ghoul::RuntimeError("A navigation state is required");
            }

            const NavigationState navigationState = NavigationState(p.navigationState.value());
            waypoints = { Waypoint(navigationState) };
            break;
        }
        case Parameters::Type::Node: {
            if (!p.target.has_value()) {
                throw ghoul::RuntimeError("A target node is required");
            }

            const std::string nodeIdentifier = p.target.value();
            const SceneGraphNode* targetNode = sceneGraphNode(nodeIdentifier);

            if (!targetNode) {
                throw ghoul::RuntimeError(fmt::format(
                    "Could not find target node '{}'", nodeIdentifier
                ));
            }

            NodeInfo info {
                nodeIdentifier,
                p.position,
                p.height,
                p.useTargetUpDirection.value_or(false)
            };

            waypoints = { computeDefaultWaypoint(info, startPoint) };
            break;
        }
        default: {
            LERROR(fmt::format("Uknown instruciton type: {}", p.type));
            throw ghoul::MissingCaseException();
        }
    }

    // TODO: allow for an instruction to represent a list of waypoints
    Waypoint waypointToAdd = waypoints[0];

    return Path(startPoint, waypointToAdd, curveType, duration);
}

Waypoint PathCreator::waypointFromCamera() {
    Camera* camera = global::navigationHandler->camera();
    const glm::dvec3 pos = camera->positionVec3();
    const glm::dquat rot = camera->rotationQuaternion();
    const std::string node = global::navigationHandler->anchorNode()->identifier();
    return Waypoint{ pos, rot, node };
}

Waypoint PathCreator::computeDefaultWaypoint(const NodeInfo& info, 
                                             const Waypoint& startPoint) 
{
    const SceneGraphNode* targetNode = sceneGraphNode(info.identifier);
    if (!targetNode) {
        LERROR(fmt::format("Could not find target node '{}'", info.identifier));
        return Waypoint();
    }

    glm::dvec3 targetPos;
    if (info.position.has_value()) {
        // Note that the anchor and reference frame is our targetnode.
        // The position in instruction is given is relative coordinates
        targetPos = targetNode->worldPosition() +
            targetNode->worldRotationMatrix() * info.position.value();
    }
    else {
        const glm::dvec3 nodePos = targetNode->worldPosition();
        const glm::dvec3 sunPos = glm::dvec3(0.0, 0.0, 0.0);
        const SceneGraphNode* closeNode = findNodeNearTarget(targetNode);

        glm::dvec3 stepDirection;
        if (closeNode) {
            // If the node is close to another node in the scene, make sure that the
            // position is set to minimize risk of collision
            stepDirection = glm::normalize(nodePos - closeNode->worldPosition());
        }
        else if (glm::length(sunPos - nodePos) < Epsilon) {
            // Special case for when the target is the Sun. Assumption: want an overview of
            // the solar system, and not stay in the orbital plane
            stepDirection = glm::dvec3(0.0, 0.0, 1.0);
        }
        else {
            // Go to a point that is being lit up by the sun, slightly offsetted from sun
            // direction
            const glm::dvec3 prevPos = startPoint.position();
            const glm::dvec3 targetToPrev = prevPos - nodePos;
            const glm::dvec3 targetToSun = sunPos - nodePos;

            constexpr const float defaultPositionOffsetAngle = -30.f; // degrees
            constexpr const float angle = glm::radians(defaultPositionOffsetAngle);
            const glm::dvec3 axis = glm::normalize(glm::cross(targetToPrev, targetToSun));
            const glm::dquat offsetRotation = angleAxis(static_cast<double>(angle), axis);

            stepDirection = glm::normalize(offsetRotation * targetToSun);
        }

        const double radius = Waypoint::findValidBoundingSphere(targetNode);
        const double defaultHeight = 2.0 * radius;
        const double height = info.height.value_or(defaultHeight);

        targetPos = nodePos + stepDirection * (radius + height);
    }

    glm::dvec3 up = global::navigationHandler->camera()->lookUpVectorWorldSpace();
    if (info.useTargetUpDirection) {
        // @TODO (emmbr 2020-11-17) For now, this is hardcoded to look good for Earth, 
        // which is where it matters the most. A better solution would be to make each 
        // sgn aware of its own 'up' and query 
        up = targetNode->worldRotationMatrix() * glm::dvec3(0.0, 0.0, 1.0);
    }

    const glm::dvec3 lookAtPos = targetNode->worldPosition();
    const glm::dquat targetRot = helpers::lookAtQuaternion(targetPos, lookAtPos, up);

    return Waypoint(targetPos, targetRot, info.identifier);
}

SceneGraphNode* PathCreator::findNodeNearTarget(const SceneGraphNode* node) {
    const std::vector<SceneGraphNode*>& relevantNodes =
        global::moduleEngine->module<AutoNavigationModule>()->relevantNodes();

    for (SceneGraphNode* n : relevantNodes) {
        if (n->identifier() == node->identifier()) {
            continue;
        }

        constexpr const float proximityRadiusFactor = 3.f;

        const float bs = static_cast<float>(n->boundingSphere());
        const float proximityRadius = proximityRadiusFactor * bs;
        const glm::dvec3 posInModelCoords = 
            glm::inverse(n->modelTransform()) * glm::dvec4(node->worldPosition(), 1.0);

        bool isClose = helpers::isPointInsideSphere(
            posInModelCoords,
            glm::dvec3(0.0, 0.0, 0.0),
            proximityRadius
        );

        if (isClose) {
            return n;
        }
    }

    return nullptr;
}

} // namespace openspace::pathnavigation
