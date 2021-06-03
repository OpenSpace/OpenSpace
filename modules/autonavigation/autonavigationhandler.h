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

#ifndef __OPENSPACE_MODULE___AUTONAVIGATIONHANDLER___H__
#define __OPENSPACE_MODULE___AUTONAVIGATIONHANDLER___H__

#include <modules/autonavigation/atnodenavigator.h>
#include <modules/autonavigation/pathsegment.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/propertyowner.h>
#include <ghoul/glm.h>

namespace openspace {
    class Camera;
} // namespace openspace

namespace openspace::autonavigation {

struct Waypoint;
struct Instruction;
struct TargetNodeInstruction;
class PathSpecification;

class AutoNavigationHandler : public properties::PropertyOwner {
public:
    AutoNavigationHandler();
    ~AutoNavigationHandler();

    // Accessors
    Camera* camera() const;
    const SceneGraphNode* anchor() const;
    const std::vector<SceneGraphNode*>& relevantNodes() const;
    int integrationResolutionPerFrame() const;
    double speedScale() const;

    const PathSegment& currentSegment() const;
    bool noCurrentPath() const;
    bool hasFinished() const;

    void updateCamera(double deltaTime);
    void createPath(PathSpecification& spec);
    void clearPath();
    void startPath();
    void continuePath();
    void abortPath();

    // TODO: remove functions for debugging
    std::vector<glm::dvec3> curvePositions(int nPerSegment);
    std::vector<glm::dquat> curveOrientations(int nPerSegment);
    std::vector<glm::dvec3> curveViewDirections(int nPerSegment);
    std::vector<glm::dvec3> controlPoints();

private:
    Waypoint wayPointFromCamera();
    Waypoint lastWayPoint();
    void removeRollRotation(CameraPose& pose, double deltaTime);
    void pauseAtTarget(int i);

    void applyStopBehaviour(double deltaTime);

    void addSegment(const Instruction& ins, int index);
    void addStopDetails(const Instruction& ins);

    SceneGraphNode* findNodeNearTarget(const SceneGraphNode* node);
    Waypoint computeDefaultWaypoint(const Instruction& ins);

    std::vector<SceneGraphNode*> findRelevantNodes();

    struct StopDetails {
        bool shouldStop;
        std::optional<double> duration;
        AtNodeNavigator::Behavior behavior;
    };

    struct Path {
        std::vector<PathSegment> segments;
        std::vector<StopDetails> stops; // 1 between every segment
    };

    Path _currentPath;

    AtNodeNavigator _atNodeNavigator; // responsible for navigation during stops
    StopDetails* _activeStop = nullptr;
    double _progressedTimeInStop = 0.0;

    bool _isPlaying = false;
    unsigned int _currentSegmentIndex = 0;

    std::vector<SceneGraphNode*> _relevantNodes;

    properties::OptionProperty _defaultCurveOption;
    properties::BoolProperty _includeRoll;
    properties::BoolProperty _stopAtTargetsPerDefault;
    properties::OptionProperty _defaultStopBehavior;

    // for testing pause behaviors.
    // TODO: remove later, if it causes problems with regular navigation
    properties::BoolProperty _applyStopBehaviorWhenIdle;

    properties::StringListProperty _relevantNodeTags;
    properties::FloatProperty _defaultPositionOffsetAngle;
    properties::BoolProperty _pickClosestTargetPosition;
    properties::IntProperty _integrationResolutionPerFrame;
    properties::FloatProperty _speedScale;
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE___AUTONAVIGATIONHANDLER___H__
