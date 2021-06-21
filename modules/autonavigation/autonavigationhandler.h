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

#ifndef __OPENSPACE_MODULE_AUTONAVIGATION___AUTONAVIGATIONHANDLER___H__
#define __OPENSPACE_MODULE_AUTONAVIGATION___AUTONAVIGATIONHANDLER___H__

#include <modules/autonavigation/path.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/propertyowner.h>
#include <ghoul/glm.h>

namespace openspace {
    class Camera;
} // namespace openspace

namespace openspace::autonavigation {

class AutoNavigationHandler : public properties::PropertyOwner {
public:
    enum StopBehavior {
        None = 0,
        Orbit
    };

    AutoNavigationHandler();
    ~AutoNavigationHandler();

    // Accessors
    Camera* camera() const;
    const SceneGraphNode* anchor() const;
    double speedScale() const;

    bool hasCurrentPath() const;
    bool hasFinished() const;

    void updateCamera(double deltaTime);
    void createPath(const ghoul::Dictionary& dictionary);
    void clearPath();
    void startPath();
    void abortPath();
    void pausePath();
    void continuePath();

    // TODO: remove functions for debugging
    std::vector<glm::dvec3> curvePositions(int nSteps) const;
    std::vector<glm::dquat> curveOrientations(int nSteps) const;
    std::vector<glm::dvec3> curveViewDirections(int nSteps) const;
    std::vector<glm::dvec3> controlPoints() const;

private:
    void removeRollRotation(CameraPose& pose, double deltaTime);
    void applyStopBehavior(double deltaTime);

    void orbitAnchorNode(double deltaTime);

    std::unique_ptr<Path> _currentPath = nullptr;
    bool _isPlaying = false;

    properties::OptionProperty _defaultCurveOption;
    properties::BoolProperty _includeRoll;
    properties::FloatProperty _speedScale;
    properties::FloatProperty _orbitSpeedFactor;

    properties::BoolProperty _applyStopBehaviorWhenIdle;
    properties::OptionProperty _stopBehavior;
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___AUTONAVIGATIONHANDLER___H__
