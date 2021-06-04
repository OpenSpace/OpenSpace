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

#ifndef __OPENSPACE_MODULE___WAYPOINT___H__
#define __OPENSPACE_MODULE___WAYPOINT___H__

#include <openspace/interaction/navigationhandler.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace::autonavigation {

struct CameraPose {
    glm::dvec3 position;
    glm::dquat rotation;
};

// The waypoint node is the anchor or target node.
struct WaypointNodeDetails {
    WaypointNodeDetails() = default;
    WaypointNodeDetails(const std::string& nodeIdentifier);

    static double findValidBoundingSphere(const SceneGraphNode* node);

    std::string identifier;
    double validBoundingSphere = 0.0; // to be able to handle nodes with faulty bounding spheres
};

struct Waypoint {
    using NavigationState = interaction::NavigationHandler::NavigationState;

    // TODO: create waypoints from a dictionary

    Waypoint() = default;
    Waypoint(const glm::dvec3& pos, const glm::dquat& rot, const std::string& ref);
    Waypoint(const NavigationState& ns);

    glm::dvec3 position() const;
    glm::dquat rotation() const;
    SceneGraphNode* node() const;

    CameraPose pose;
    WaypointNodeDetails nodeDetails;
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE___WAYPOINT___H__
