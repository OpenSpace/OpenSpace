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

#ifndef __OPENSPACE_MODULE_AUTONAVIGATION___PATHINSTRUCTION___H__
#define __OPENSPACE_MODULE_AUTONAVIGATION___PATHINSTRUCTION___H__

#include <modules/autonavigation/waypoint.h>
#include <optional>

namespace openspace::autonavigation {

class PathInstruction {
public:
    PathInstruction(const ghoul::Dictionary& dictionary);

    Waypoint startPoint() const;
    std::vector<Waypoint> waypoints() const;
    std::optional<double> duration() const;

    static documentation::Documentation Documentation();

private:
    struct NodeInfo {
        std::string identifier;
        std::optional<glm::dvec3> position;
        std::optional<double> height;
        bool useTargetUpDirection;
    };

    Waypoint waypointFromCamera() const;
    Waypoint computeDefaultWaypoint(const NodeInfo& info) const;
    SceneGraphNode* findNodeNearTarget(const SceneGraphNode* node) const;

    Waypoint _startPoint;
    std::vector<Waypoint> _waypoints;
    std::optional<double> _duration;
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___PATHINSTRUCTION___H__
