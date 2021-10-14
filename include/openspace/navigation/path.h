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

#ifndef __OPENSPACE_CORE___PATH___H__
#define __OPENSPACE_CORE___PATH___H__

#include <openspace/navigation/pathcurve.h>
#include <openspace/navigation/waypoint.h>
#include <ghoul/misc/dictionary.h>
#include <optional>
#include <vector>

namespace openspace {
    struct CameraPose;
} // namespace openspace

namespace openspace::interaction {

class Path {
public:
    enum Type {
        AvoidCollision,
        Linear,
        ZoomOutOverview,
        AvoidCollisionWithLookAt // @TODO (2021-08-13, emmbr) This type right now leads
                                 // to rapid rotations, but is useful in specific
                                 // scenarios, e.g. close to surfaces. Later we want to
                                 // remove it, and create a curve type that looks nicely
                                 // at the targets when moving, avoids collisions and
                                 // doesn't introduce sudden large changes in rotation
    };

    Path(Waypoint start, Waypoint end, Type type,
        std::optional<double> duration = std::nullopt);

    Waypoint startPoint() const;
    Waypoint endPoint() const;

    /**
     * Return the total length of the the curve for the path, in meters
     */
    double pathLength() const;

    /**
     * Return a vector of positions corresponding to the control points of the path's
     * spline curve
     */
    std::vector<glm::dvec3> controlPoints() const;

    /**
     * Take a step along the current path, corresponding to the delta time step \p dt, and
     * return the resulting camera pose. The \p speedScale is a factor that will be
     * multiplied with the traversal speed
     */
    CameraPose traversePath(double dt, float speedScale = 1.f);

    /**
     * Return the identifer of the node that is the current appropriate anchor node, of
     * the start and end waypoint's reference node. Dtermined based on how far along the
     * path we have traveled
     */
    std::string currentAnchor() const;

    /**
     * Return wether the path has reached its end point or not
     */
    bool hasReachedEnd() const;

    /**
     * Compute the interpolated camera pose at a certain distance along the path
     */
    CameraPose interpolatedPose(double distance) const;

private:
    /**
     * Interpolate between the paths start and end rotation using the approach that
     * corresponds to the path's curve type. The interpolation parameter \p t is the
     * same as for the position interpolation, i.e. the relative traveled in distance
     * along the path, in [0, 1]
     */
    glm::dquat interpolateRotation(double t) const;

    /**
     * Compute the interpolated rotation quaternion using an eased SLERP approach
     */
    glm::dquat easedSlerpRotation(double t) const;

    /**
     * Compute the interpolated rotation quaternion using an approach that first
     * interpolates to look at the start node, and then the end node, before
     * interpolating to the end rotation
     */
    glm::dquat lookAtTargetsRotation(double t) const;

    /**
     * Evaluate the current traversal speed along the path, based on the currently
     * traveled distance. The final speed will be scaled to match the desired duration
     * for the path (which might have been specified by the user)
     */
    double speedAlongPath(double traveledDistance) const;

    Waypoint _start;
    Waypoint _end;
    Type _type;

    std::unique_ptr<PathCurve> _curve;

    double _speedFactorFromDuration = 1.0;

    // Playback variables
    double _traveledDistance = 0.0;
    double _progressedTime = 0.0; // Time since playback started
};


// Create a path of the given type based on an instruction given as a dictionary.
// See top of cpp file for documentation on keys and values for the dictionary.
// Returns the created path.
Path createPathFromDictionary(const ghoul::Dictionary& dictionary, Path::Type type);

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___PATH___H__
