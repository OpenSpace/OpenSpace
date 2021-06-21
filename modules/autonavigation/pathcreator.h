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

#ifndef __OPENSPACE_MODULE_AUTONAVIGATION___PATHCREATOR___H__
#define __OPENSPACE_MODULE_AUTONAVIGATION___PATHCREATOR___H__

#include <modules/autonavigation/path.h>

namespace openspace::pathnavigation {

struct Waypoint;

class PathCreator {
public:
    // Create a path from a dictionary containing the instruction
    static Path createPath(const ghoul::Dictionary& dictionary, 
        Path::CurveType curveType);

private:
    struct NodeInfo {
        std::string identifier;
        std::optional<glm::dvec3> position;
        std::optional<double> height;
        bool useTargetUpDirection;
    };

    static Waypoint waypointFromCamera();
    static Waypoint computeDefaultWaypoint(const NodeInfo& info,
        const Waypoint& startPoint);

    // Test if the node lies within a given proximity radius of any relevant node 
    // in the scene
    static SceneGraphNode* findNodeNearTarget(const SceneGraphNode* node);
};

} // namespace openspace::pathnavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___PATHCREATOR___H__
