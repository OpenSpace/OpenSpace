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

#include <modules/autonavigation/helperfunctions.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PathInstruction";

    struct [[codegen::Dictionary(Instruction)]] Parameters {
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
        // of this path segment.
        std::optional<ghoul::Dictionary> navigationState 
            [[codegen::reference("core_navigation_state")]];
    };
#include "instruction_codegen.cpp"
} // namespace

namespace openspace::autonavigation {

documentation::Documentation Instruction::Documentation() {
    return codegen::doc<Parameters>("autonavigation_pathinstruction");
}

Instruction::Instruction(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    duration = p.duration;

    switch (p.type) {
        case Parameters::Type::NavigationState: {
            type = Type::NavigationState;

            if (!p.navigationState.has_value()) {
                throw ghoul::RuntimeError("A navigation state is required");
            }

            using NavigationState = interaction::NavigationHandler::NavigationState;
            navigationState = NavigationState(p.navigationState.value());
            _waypoints = { Waypoint(navigationState) };
            break;
        }
        case Parameters::Type::Node: {
            type = Type::Node;

            if (!p.target.has_value()) {
                throw ghoul::RuntimeError("A target node is required");
            }

            nodeIdentifier = p.target.value();
            const SceneGraphNode* targetNode = sceneGraphNode(nodeIdentifier);

            if (!targetNode) {
                throw ghoul::RuntimeError(fmt::format(
                    "Could not find target node '{}'", nodeIdentifier
                ));
            }

            position = p.position;
            height = p.height;
            useTargetUpDirection = p.useTargetUpDirection.value_or(false);

            if (position.has_value()) {
                // Note that the anchor and reference frame is our targetnode.
                // The position in instruction is given is relative coordinates.
                glm::dvec3 targetPos = targetNode->worldPosition() +
                    targetNode->worldRotationMatrix() * position.value();

                Camera* camera = global::navigationHandler->camera();
                glm::dvec3 up = camera->lookUpVectorWorldSpace();

                if (useTargetUpDirection) {
                    // @TODO (emmbr 2020-11-17) For now, this is hardcoded to look good for Earth, 
                    // which is where it matters the most. A better solution would be to make each 
                    // sgn aware of its own 'up' and query 
                    up = targetNode->worldRotationMatrix() * glm::dvec3(0.0, 0.0, 1.0);
                }

                const glm::dvec3 lookAtPos = targetNode->worldPosition();
                const glm::dquat targetRot = helpers::lookAtQuaternion(targetPos, lookAtPos, up);

                Waypoint wp{ targetPos, targetRot, nodeIdentifier };
                _waypoints = { wp };
            }
            break;
        }
        default: {
            LERROR(fmt::format("Uknown instruciton type: {}", p.type));
            throw ghoul::MissingCaseException();
            break;
        }
    }
}

std::vector<Waypoint> Instruction::waypoints() const {
    return _waypoints;
}

} // namespace openspace::autonavigation
