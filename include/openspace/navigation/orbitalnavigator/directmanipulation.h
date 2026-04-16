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

#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/glm.h>
#include <set>

namespace openspace {

class Camera;
struct CameraPose;
class SceneGraphNode;

class DirectManipulation : public PropertyOwner {
public:
    struct VelocityStates {
        glm::dvec2 orbit = glm::dvec2(0.0);
        double zoom = 0.0;
        double roll = 0.0;
        glm::dvec2 pan = glm::dvec2(0.0);
    };

    /**
     * Stores the selected node, the cursor ID as well as the surface coordinates the
     * cursor touched.
     */
    struct SelectedBody {
        size_t id = 0;
        const SceneGraphNode* node = nullptr;
        glm::dvec3 coordinates = glm::dvec3(0.0);
    };

    /**
     * Represents touched screenspace coordinates (normalized to [0, 1]).
     */
    struct TouchPoint {
        size_t id = 0;
        glm::dvec2 position = glm::dvec2(0.0);

        // Position on first touch down
        glm::dvec2 initialPosition = glm::dvec2(0.0);
    };

    DirectManipulation();

    void updateCameraFromInput();

    /**
     * Compute a camera pose from the velocity states resulting from a direct
     * manipulation interaction.
     *
     * \param velocities The velocity states to process, which contains the parameters
     *        for orbiting, zooming, rolling, and panning
     * \param camera The camera for which to compute the new pose. The current pose of
     *        the camera is used as the starting point for the computation
     * \param anchor The anchor node for the direct manipulation interaction, which is
     *        used as the reference point for the camera transformations. For example,
     *        the camera will orbit around this node
     * \return The resulting camera pose
     */
    static CameraPose cameraPoseFromVelocities(const VelocityStates& velocities,
        const Camera* camera, const SceneGraphNode* anchor);

private:
    /**
     * Calculates the new camera state such that it minimizes the L2 error in screenspace
     * between contact points and surface coordinates projected to clip space using LMA.
     */
    void applyDirectControl(const std::vector<TouchPoint>& touchPoints);

    /**
     * Traces each contact point into the scene as a ray and find the intersection points
     * on the surface of the current anchor node, if any. Saves the input id the node and
     * surface coordinates the cursor hit.
     */
    void updateNodeSurfacePoints(const std::vector<TouchPoint>& touchPoints);

    std::optional<glm::dvec3> computeSurfacePoint(const glm::dvec2& touchPosition,
        const SceneGraphNode* node) const;

    bool isValidDirectTouchNode() const;
    bool isWithinDirectTouchDistance() const;

    /**
     * Minimize the L2 error of touch input to 3D camera position, using the levmarq
     * (Levenberg–Marquardt) algorithm.
     */
    std::optional<VelocityStates> solveVelocitiesFromTouchPoints(
        const std::vector<TouchPoint>& touchPoints, const Camera& camera);

    BoolProperty _enabled;
    BoolProperty _isActive;
    BoolProperty _allowMouseInput;
    FloatProperty _distanceThreshold;
    StringListProperty _defaultRenderableTypes;

    // A sorted version of the list in the property
    std::set<std::string> _sortedDefaultRenderableTypes;

    std::vector<SelectedBody> _selectedNodeSurfacePoints;

    glm::dvec2 _firstMousePressPos = glm::dvec2(0.0);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___DIRECTMANIPULATION___H__
