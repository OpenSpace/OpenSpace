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

#include <modules/autonavigation/waypoint.h>

#include <modules/autonavigation/autonavigationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "Waypoint";
} // namespace

namespace openspace::pathnavigation {

Waypoint::Waypoint(const glm::dvec3& pos, const glm::dquat& rot, const std::string& ref)
    : nodeIdentifier(ref)
{
    pose.position = pos;
    pose.rotation = rot;

    const SceneGraphNode* node = sceneGraphNode(nodeIdentifier);
    if (!node) {
        LERROR(fmt::format("Could not find node '{}'", nodeIdentifier));
        return;
    }
    validBoundingSphere = findValidBoundingSphere(node);
}

Waypoint::Waypoint(const NavigationState& ns) {
    // OBS! The following code to get the position and rotation is exactly the same as in
    // NavigationHandler::applyNavigationState. Should probably be made into a function.
    // TODO: make that function
    const SceneGraphNode* referenceFrame = sceneGraphNode(ns.referenceFrame);
    const SceneGraphNode* anchorNode = sceneGraphNode(ns.anchor); 

    if (!anchorNode) {
        LERROR(fmt::format("Could not find node '{}' to target", ns.anchor));
        return;
    }

    nodeIdentifier = ns.anchor;
    validBoundingSphere = findValidBoundingSphere(anchorNode);

    const glm::dvec3 anchorWorldPosition = anchorNode->worldPosition();
    const glm::dmat3 referenceFrameTransform = referenceFrame->worldRotationMatrix();

    pose.position = anchorWorldPosition +
        glm::dvec3(referenceFrameTransform * glm::dvec4(ns.position, 1.0));

    glm::dvec3 up = ns.up.has_value() ?
        glm::normalize(referenceFrameTransform * ns.up.value()) :
        glm::dvec3(0.0, 1.0, 0.0);

    // Construct vectors of a "neutral" view, i.e. when the aim is centered in view.
    glm::dvec3 neutralView =
        glm::normalize(anchorWorldPosition - pose.position);

    glm::dquat neutralCameraRotation = glm::inverse(glm::quat_cast(glm::lookAt(
        glm::dvec3(0.0),
        neutralView,
        up
    )));

    glm::dquat pitchRotation = glm::angleAxis(ns.pitch, glm::dvec3(1.f, 0.f, 0.f));
    glm::dquat yawRotation = glm::angleAxis(ns.yaw, glm::dvec3(0.f, -1.f, 0.f));

    pose.rotation = neutralCameraRotation * yawRotation * pitchRotation;
}

glm::dvec3 Waypoint::position() const {
    return pose.position;
}

glm::dquat Waypoint::rotation() const {
    return pose.rotation;
}

SceneGraphNode* Waypoint::node() const {
    return sceneGraphNode(nodeIdentifier);
}

double Waypoint::findValidBoundingSphere(const SceneGraphNode* node) {
    double bs = static_cast<double>(node->boundingSphere());
    const double minValidBoundingSphere =
        global::moduleEngine->module<AutoNavigationModule>()->minValidBoundingSphere();

    if (bs < minValidBoundingSphere) {
        // If the bs of the target is too small, try to find a good value in a child node.
        // Only check the closest children, to avoid deep traversal in the scene graph.
        // Also, the possibility to find a bounding sphere represents the visual size of
        // the target well is higher for these nodes.
        for (SceneGraphNode* child : node->children()) {
            bs = static_cast<double>(child->boundingSphere());
            if (bs > minValidBoundingSphere) {
                LWARNING(fmt::format(
                    "The scene graph node '{}' has no, or a very small, bounding sphere. "
                    "Using bounding sphere of child node '{}' in computations.",
                    node->identifier(),
                    child->identifier()
                ));

                return bs;
            }
        }

        LWARNING(fmt::format(
            "The scene graph node '{}' has no, or a very small, bounding sphere. Using "
            "minimal value. This might lead to unexpected results.",
            node->identifier())
        );

        bs = minValidBoundingSphere;
    }

    return bs;
}

} // namespace openspace::pathnavigation
