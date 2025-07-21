/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___WAYPOINT___H__
#define __OPENSPACE_CORE___WAYPOINT___H__

#include <openspace/camera/camerapose.h>
#include <ghoul/glm.h>
#include <optional>
#include <string>

namespace openspace { class SceneGraphNode; }

namespace openspace::interaction {

struct NavigationState;

class Waypoint {
public:
    Waypoint() = default;
    Waypoint(const glm::dvec3& pos, const glm::dquat& rot, std::string ref);
    explicit Waypoint(const NavigationState& ns);

    CameraPose pose() const;
    glm::dvec3 position() const;
    glm::dquat rotation() const;
    SceneGraphNode* node() const;
    std::string nodeIdentifier() const;
    std::optional<std::string> aimIdentifier() const;
    double validBoundingSphere() const;

private:
    CameraPose _pose;
    std::string _nodeIdentifier;
    // To be able to handle nodes with faulty bounding spheres
    double _validBoundingSphere = 0.0;

    // Keep track of if there was an aim node, specified in for example the
    // navigation state used to create this waypoint. It may be required in
    // certain situations
    std::optional<std::string> _aimNodeIdentifier;
};

/**
 * Compute a waypoint from the current camera position.
 *
 * \return The computed WayPoint
 */
Waypoint waypointFromCamera();

struct NodeCameraStateSpec {
    std::string identifier;
    std::optional<glm::dvec3> position;
    std::optional<double> height;
    bool useTargetUpDirection = false;
};

// @TODO (2023-05-16, emmbr) Allow different light sources, not only the 'Sun'
/**
 * Compute a waypoint from information about a scene graph node and a previous waypoint,
 * where the camera will be facing the given node. If there is a 'Sun' node in the scene,
 * it will possibly be used to compute a position on the lit side of the object.
 *
 * \param spec Details about the node and state to create the waypoint from. Minimal
 *             information is the identifier of the node, but a position or height above
 *             the bounding sphere may also be given.
 * \param startPoint An optional previous waypoint. If not specified, the current camera
 *                   position will be used.
 * \param useLinear If `true`, the new waypoint will be computed along a straight line
 *                  from the start waypoint to the scene graph node or position.
 * \return The computed WayPoint
 */
Waypoint computeWaypointFromNodeInfo(const NodeCameraStateSpec& spec,
    const std::optional<Waypoint>& startPoint = std::nullopt, bool useLinear = false);

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___WAYPOINT___H__
