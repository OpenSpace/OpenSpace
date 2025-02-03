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

#ifndef __OPENSPACE_CORE___PATH___H__
#define __OPENSPACE_CORE___PATH___H__

#include <openspace/documentation/documentation.h>
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
    enum class Type {
        AvoidCollision = 0,
        ZoomOutOverview,
        Linear,

        // @TODO (2021-08-13, emmbr) This type right now leads to rapid rotations, but is
        // useful in specific scenarios, e.g. close to surfaces. Later we want to remove
        // it, and create a curve type that looks nicely at the targets when moving,
        // avoids collisions and doesn't introduce sudden large changes in rotation
        AvoidCollisionWithLookAt
    };

    Path(Waypoint start, Waypoint end, Type type,
        std::optional<float> duration = std::nullopt);

    Waypoint startPoint() const;
    Waypoint endPoint() const;

    /**
     * Return the total length of the the curve for the path, in meters.
     */
    double pathLength() const;

    /**
     * Return the remaining distance to traverse, in meters.
     */
    double remainingDistance() const;

    /**
     * Estimate a value for the remaining time to reach the target, based on the
     * currently progressed time and the estimation for how long the path will
     * take to traverse. Note that the computation is not exact.
     *
     * \param speedScale The speed scale factor that may affect how fast the camera moves
     * \return The estimated remaining time
     */
    float estimatedRemainingTime(float speedScale) const;

    /**
     * Return a vector of positions corresponding to the control points of the path's
     * spline curve.
     */
    std::vector<glm::dvec3> controlPoints() const;

    /**
     * Take a step along the current path, corresponding to the delta time step \p dt, and
     * return the resulting camera pose. The \p speedScale is a factor that will be
     * multiplied with the traversal speed.
     */
    CameraPose traversePath(double dt, float speedScale = 1.f);

    /**
     * Function that can be used to permaturely quit a path, for example when skipping
     * to the end.
     */
    void quitPath();

    /**
     * Return the identifer of the node that is the current appropriate anchor node, of
     * the start and end waypoint's reference node. Dtermined based on how far along the
     * path we have traveled.
     */
    std::string currentAnchor() const;

    /**
     * Return wether the path has reached its end point or not.
     */
    bool hasReachedEnd() const;

    /**
     * Compute the interpolated camera pose at a certain distance along a *linear*
     * path. Note that the linear path is a special case, to avoid risks of precision
     * problems for long paths.
     */
    CameraPose linearInterpolatedPose(double distance, double displacement);

    /**
     * Compute the interpolated camera pose at a certain distance along the path.
     */
    CameraPose interpolatedPose(double distance) const;

    /**
     * Reset variables used to play back path.
     */
    void resetPlaybackVariables();

    static documentation::Documentation Documentation();

private:
    /**
     * Interpolate between the paths start and end rotation using the approach that
     * corresponds to the path's curve type. The interpolation parameter \p t is the same
     * as for the position interpolation, i.e. the relative traveled distance along the
     * path, in [0, 1].
     *
     * \param t The interpolation parameter, given as the relative traveled distance along
     *        the path, in [0, 1]
     */
    glm::dquat interpolateRotation(double t) const;

    /**
     * Compute the interpolated rotation quaternion using an eased SLERP approach.
     *
     * \param t The interpolation variable for the rotatation interpolation. Should be the
     *        relative traveled distance, in [0, 1]
     */
    glm::dquat easedSlerpRotation(double t) const;

    /**
     * Compute the interpolated rotation quaternion using a method that is customized for
     * linear paths. The camera will first interpoalte to look at the targetted node, and
     * keep doing so for most of the path. At the end, when within a certain distance from
     * the target, the rotation is interpolated so that the camera ends up in the target
     * pose at the end of the path.
     *
     * \param t The interpolation variable for the rotatation interpolation. Should be the
     *        relative traveled distance, in [0, 1]
     */
    glm::dquat linearPathRotation(double t) const;

    /**
     * Compute the interpolated rotation quaternion using an approach that first
     * interpolates to look at the start node, and then the end node, before interpolating
     * to the end rotation.
     *
     * \param t The interpolation variable for the rotatation interpolation. Should be the
     *        relative traveled distance, in [0, 1]
     */
    glm::dquat lookAtTargetsRotation(double t) const;

    /**
     * Evaluate the current traversal speed along the path, based on the currently
     * traveled distance. The final speed will be scaled to match the desired duration for
     * the path (which might have been specified by the user).
     *
     * \param traveledDistance The current distance traveled along the path, in meters
     */
    double speedAlongPath(double traveledDistance) const;

    Waypoint _start;
    Waypoint _end;
    Type _type;

    std::unique_ptr<PathCurve> _curve;

    float _speedFactorFromDuration = 1.f;
    float _expectedDuration = 0.f;

    // Playback variables
    double _traveledDistance = 0.0; // Meters
    float _progressedTime = 0.f; // Time since playback started (seconds)
    bool _shouldQuit = false;
    CameraPose _prevPose;
};

/**
 * Create a path based on an instruction given as a dictionary (see top of cpp file
 * for documentation on keys and values for the dictionary). If \p forceType is specified,
 * that type will primarily be used as the type for the created path. Secondly, the type
 * will be read from the dictionary, and lastly it will use the default from
 * PathNavigator.
 *
 * \return The created path
 */
Path createPathFromDictionary(const ghoul::Dictionary& dictionary,
    std::optional<Path::Type> forceType = std::nullopt);

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___PATH___H__
