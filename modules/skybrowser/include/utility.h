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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___UTILITY___H__
#define __OPENSPACE_MODULE_SKYBROWSER___UTILITY___H__

#include <openspace/documentation/documentation.h>
#include <openspace/util/distanceconstants.h>
#include <chrono>

namespace openspace::skybrowser {

// Constants
constexpr double ScreenSpaceZ = -2.1;
constexpr double CelestialSphereRadius = 4.0 * distanceconstants::Parsec;

/**
 * Converts from Cartesian coordinates to spherical coordinates with unit length.
 *
 * \param coords Cartesian coordinates
 * \return Spherical coordinates with unit length in degrees
 */
glm::dvec2 cartesianToSpherical(const glm::dvec3& coords);

/**
 * Converts from spherical coordinates to Cartesian coordinates with unit length.
 *
 * \param coords Spherical coordinates in degrees
 * \return Cartesian coordinates with unit length
 */
glm::dvec3 sphericalToCartesian(const glm::dvec2& coords);

/**
 * Converts from Cartesian galactic coordinates to Cartesian equatorial coordinates in
 * epoch J2000 with unit length.
 *
 * \param coords Cartesian galactic coordinates
 * \return Cartesian equatorial coordinates in the epoch of J2000 with unit length
 */
glm::dvec3 galacticToEquatorial(const glm::dvec3& coords);

/**
 * Converts from Cartesian equatorial coordinates to Cartesian galactic coordinates. The
 * galactic coordinates vector has the length of the radius of the Celestial sphere.
 *
 * \param coords Cartesian equatorial coordinates
 * \return Cartesian galactic coordinates placed on the Celestial sphere
 */
glm::dvec3 equatorialToGalactic(const glm::dvec3& coords);

// Conversion to screenspace from local camera / pixels
/**
 * Converts from local camera coordinates to screenspace coordinates. The screenspace
 * coordinates are placed on the screenspace plane which has the z-coordinate as -2.1.
 *
 * \param coords Cartesian local camera coordinates
 * \return Cartesian galactic coordinates placed on the Celestial sphere
 */
glm::dvec3 localCameraToScreenSpace3d(const glm::dvec3& coords);

/**
 * Converts from pixel coordinates to screenspace coordinates in 2D.
 *
 * \param mouseCoordinate Pixel coordinate
 * \return Cartesian ScreenSpace coordinate
 */
glm::vec2 pixelToScreenSpace2d(const glm::vec2& mouseCoordinate);

// Conversion local camera space <-> galactic / equatorial
/**
 * Converts from Cartesian equatorial coordinates in epoch J2000 to local camera space.
 *
 * \param coords Cartesian equatorial coordinates in epoch J2000
 * \return Local camera coordinates with unit length
 */
glm::dvec3 equatorialToLocalCamera(const glm::dvec3& coords);

/**
 * Converts from Cartesian galactic coordinates to local camera space with unit length.
 *
 * \param coords Cartesian galactic coordinates
 * \return Cartesian local camera coordinates with unit length
 */
glm::dvec3 galacticToLocalCamera(const glm::dvec3& coords);

/**
 * Converts from Cartesian local camera coordinates to galactic coordinates.
 *
 * \param coords Cartesian local camera coordinates
 * \return Cartesian galactic coordinates placed on the Celestial sphere
 */
glm::dvec3 localCameraToGalactic(const glm::dvec3& coords);

/**
 * Converts from local camera coordinates to Cartesian equatorial coordinates in the epoch
 * J2000.
 *
 * \param coords Cartesian local camera coordinates
 * \return Cartesian equatorial coordinates with unit length
 */
glm::dvec3 localCameraToEquatorial(const glm::dvec3& coords);

// Camera roll and direction
/**
 * Returns the angle between the up direction of the OpenSpace camera and the equatorial
 * North Pole direction.
 *
 * \return Angle in degrees between the OpenSpace camera's up direction vector and the
 *         equatorial North Pole direction.
 */
double targetRoll(const glm::dvec3& up, const glm::dvec3& forward);

/**
 * Returns the view direction of the OpenSpace camera in galactic coordinates.
 * \return View direction of the OpenSpace camera in Cartesian galactic coordinates.
 */
glm::dvec3 cameraDirectionGalactic();

/**
 * Returns the view direction of the OpenSpace camera in equatorial coordinates in epoch
 * J2000.
 *
 * \return View direction of the OpenSpace camera in Cartesian equatorial coordinates in
 *         epoch J2000.
 */
glm::dvec3 cameraDirectionEquatorial();

/**
 * Returns the window ratio r which is calculated as x / y.
 *
 * \return The window ratio x / y
 */
float windowRatio();

/**
 * Returns the vertical and horizontal field of view of the OpenSpace window.
 *
 * \return The horizontal and vertical field of view in degrees
 */
glm::dvec2 fovWindow();

/**
 * Returns true if the Cartesian equatorial coordinate is in the current view of the
 * camera.
 *
 * \param equatorial Cartesian equatorial coordinates in epoch J2000
 * \return `true` if the coordinates are in the camera's current field of view
 */
bool isCoordinateInView(const glm::dvec3& equatorial);

// Animation for target and camera
/**
 * Returns the angle between two vectors.
 *
 * \param start Cartesian vector
 * \param end Cartesian vector
 * \return Angle between two vectors in radians
 */
double angleBetweenVectors(const glm::dvec3& start, const glm::dvec3& end);

/**
 * Returns a 4x4 matrix for an incremental rotation of a vector. The matrix should be used
 * multiple times in order to animate.
 *
 * \param start Cartesian vector
 * \param end Cartesian vector
 * \param percentage Percentage of the angle between the vectors that the matrix should
 *        rotate
 * \return 4x4 matrix for incremental rotation animation of a vector
 */
glm::dmat4 incrementalAnimationMatrix(const glm::dvec3& start, const glm::dvec3& end,
    double percentage);

/**
 * Returns the size in meters that for example a plane would need to have in order to
 * display a specified field of view.
 *
 * \param fov The set field of view
 * \param worldPosition The galactic position of the plane
 * \return Field of view
 */
double sizeFromFov(double fov, const glm::dvec3& worldPosition);

template <typename T>
class Animation {
public:
    Animation(T start, T goal, double time)
        : _goal(std::move(goal))
        , _start(std::move(start))
        , _animationTime(std::chrono::milliseconds(static_cast<int>(time * 1000)))
    {}

    void start() {
        _isStarted = true;
        _startTime = std::chrono::system_clock::now();
    }

    void stop() {
        _isStarted = false;
    }

    bool isAnimating() const {
        bool timeLeft = timeSpent().count() < _animationTime.count();
        return timeLeft && _isStarted;
    }

    T newValue() const;
    glm::dmat4 rotationMatrix();

private:
    std::chrono::duration<double, std::milli> timeSpent() const {
        std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
        std::chrono::duration<double, std::milli> timeSpent = now - _startTime;
        return timeSpent;
    }

    double percentageSpent() const {
        return timeSpent().count() / _animationTime.count();
    }

    // Animation
    bool _isStarted = false;
    double _lastPercentage = 0;
    T _goal;
    T _start;
    std::chrono::milliseconds _animationTime = std::chrono::milliseconds(2000);
    std::chrono::system_clock::time_point _startTime;
};

} // namespace openspace::skybrowser

#endif // __OPENSPACE_MODULE_SKYBROWSER___UTILITY___H__
