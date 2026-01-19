/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___DIRECTMANIPULATION___H__
#define __OPENSPACE_CORE___DIRECTMANIPULATION___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/navigation/orbitalnavigator/directmanipulation/directinputsolver.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/touch.h>
#include <ghoul/glm.h>
#include <set>

namespace openspace {
    class Camera;
    struct CameraPose;
    class SceneGraphNode;
} // namespace openspace

namespace openspace::interaction {

class DirectManipulation : public properties::PropertyOwner {
public:
    struct VelocityStates {
        glm::dvec2 orbit = glm::dvec2(0.0);
        double zoom = 0.0;
        double roll = 0.0;
        glm::dvec2 pan = glm::dvec2(0.0);
    };

    DirectManipulation();

    void updateCameraFromInput();

    static CameraPose cameraPoseFromVelocities(const VelocityStates& velocities,
        const Camera* camera, const SceneGraphNode* anchor);

private:

    /**
     * Calculates the new camera state such that it minimizes the L2 error in screenspace
     * between contact points and surface coordinates projected to clip space using LMA.
     */
    void applyDirectControl(const std::vector<TouchInputHolder>& touchPoints);

    /**
     * Traces each contact point into the scene as a ray and find the intersection points
     * on the surface of the current anchor node, if any. Saves the input id the node and
     * surface coordinates the cursor hit.
     */
    void updateNodeSurfacePoints(const std::vector<TouchInputHolder>& touchPoints);

    bool isValidDirectTouchNode() const;
    bool isWithinDirectTouchDistance() const;

    properties::BoolProperty _enabled;
    properties::BoolProperty _isActive;
    properties::BoolProperty _allowMouseInput;
    properties::FloatProperty _distanceThreshold;
    properties::StringListProperty _defaultRenderableTypes;

    // A sorted version of the list in the property
    std::set<std::string> _sortedDefaultRenderableTypes;

    std::vector<DirectInputSolver::SelectedBody> _selectedNodeSurfacePoints;
    DirectInputSolver _directInputSolver;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___DIRECTMANIPULATION___H__
