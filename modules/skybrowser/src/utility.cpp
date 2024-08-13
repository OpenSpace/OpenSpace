/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/skybrowser/include/utility.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <ghoul/misc/easing.h>
#include <glm/gtx/vector_angle.hpp>
#include <cmath>

namespace {
    // Galactic coordinates are projected onto the celestial sphere
    // Equatorial coordinates are unit length
    // Conversion spherical <-> Cartesian

    // Conversion matrix - J2000 equatorial <-> galactic
    // https://arxiv.org/abs/1010.3773v1
    constexpr glm::dmat3 ConversionMatrix = glm::dmat3(
        -0.054875539390,  0.494109453633, -0.867666135681, // col 0
        -0.873437104725, -0.444829594298, -0.198076389622, // col 1
        -0.483834991775,  0.746982248696,  0.455983794523  // col 2
    );
} // namespace

namespace openspace::skybrowser {

// Converts from spherical coordinates in the unit of degrees to cartesian coordianates
glm::dvec3 sphericalToCartesian(const glm::dvec2& coords) {
    const glm::dvec2 coordsRadians = glm::radians(coords);
    const glm::dvec3 cartesian = glm::dvec3(
        cos(coordsRadians.x) * cos(coordsRadians.y),
        sin(coordsRadians.x) * cos(coordsRadians.y),
        sin(coordsRadians.y)
    );

    return cartesian;
}

// Converts from cartesian coordianates to spherical in the unit of degrees
glm::dvec2 cartesianToSpherical(const glm::dvec3& coords) {
    // Equatorial coordinates RA = right ascension, Dec = declination
    double ra = atan2(coords.y, coords.x);
    const double dec = atan2(
        coords.z,
        glm::sqrt((coords.x * coords.x) + (coords.y * coords.y))
    );

    ra = ra > 0.0 ? ra : ra + glm::two_pi<double>();

    const glm::dvec2 celestialCoords = glm::dvec2(ra, dec);
    return glm::degrees(celestialCoords);
}

glm::dvec3 galacticToEquatorial(const glm::dvec3& coords) {
    return glm::transpose(ConversionMatrix) * glm::normalize(coords);
}

glm::dvec3 equatorialToGalactic(const glm::dvec3& coords) {
    // On the unit sphere
    const glm::dvec3 rGalactic = ConversionMatrix * glm::normalize(coords);
    return rGalactic;
}

glm::dvec3 localCameraToScreenSpace3d(const glm::dvec3& coords) {
    // Ensure that if the coord is behind the camera,
    // the converted coordinate will be there too
    const double zCoord = coords.z > 0.0 ? -ScreenSpaceZ : ScreenSpaceZ;

    // Calculate screen space coords x and y
    const double tanX = coords.x / coords.z;
    const double tanY = coords.y / coords.z;

    const glm::dvec3 screenSpace = glm::dvec3(zCoord * tanX, zCoord * tanY, zCoord);
    return screenSpace;
}

glm::dvec3 localCameraToGalactic(const glm::dvec3& coords) {
    const glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
    const glm::dvec4 coordsVec4 = glm::dvec4(coords, 1.0) ;
    const glm::dmat4 camMat = glm::inverse(
        global::navigationHandler->camera()->combinedViewMatrix()
    );
    // Subtract camera position to get the view direction
    const glm::dvec3 galactic = glm::dvec3(camMat * coordsVec4) - camPos;

    return glm::normalize(galactic) * CelestialSphereRadius;
}

glm::dvec3 localCameraToEquatorial(const glm::dvec3& coords) {
    // Calculate the galactic coordinate of the target direction
    // projected onto the celestial sphere
    const glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
    const glm::dvec3 galactic = camPos + localCameraToGalactic(coords);
    return galacticToEquatorial(galactic);
}

glm::dvec3 equatorialToLocalCamera(const glm::dvec3& coords) {
    // Transform equatorial J2000 to galactic coord with infinite radius
    const glm::dvec3 galactic = equatorialToGalactic(coords) * CelestialSphereRadius;
    const glm::dvec3 localCamera = galacticToLocalCamera(galactic);
    return localCamera;
}

glm::dvec3 galacticToLocalCamera(const glm::dvec3& coords) {
    // Transform vector to camera's local coordinate system
    const glm::dmat4 camMat = global::navigationHandler->camera()->combinedViewMatrix();
    const glm::dvec3 viewDirectionLocal = camMat * glm::dvec4(coords, 1.0);
    return glm::normalize(viewDirectionLocal);
}

double targetRoll(const glm::dvec3& up, const glm::dvec3& forward) {
    constexpr glm::dvec3 NorthPole = glm::dvec3(0.0, 0.0, 1.0);

    const glm::dvec3 upJ2000 = galacticToEquatorial(up);
    const glm::dvec3 forwardJ2000 = galacticToEquatorial(forward);

    const glm::dvec3 crossUpNorth = glm::cross(upJ2000, NorthPole);
    const double dotNorthUp = glm::dot(NorthPole, upJ2000);
    const double dotCrossUpNorthForward = glm::dot(crossUpNorth, forwardJ2000);

    return glm::degrees(atan2(dotCrossUpNorthForward, dotNorthUp));
}

glm::dvec3 cameraDirectionEquatorial() {
    // Get the view direction of the screen in cartesian J2000 coordinates
    const glm::dvec3 camDirGalactic = cameraDirectionGalactic();
    return galacticToEquatorial(camDirGalactic);
}

glm::dvec3 cameraDirectionGalactic() {
    // Get the view direction of the screen in galactic coordinates
    Camera* camera = global::navigationHandler->camera();
    const glm::dvec3 camPos = camera->positionVec3();
    const glm::dvec3 view = camera->viewDirectionWorldSpace();
    const glm::dvec3 galCoord = camPos + CelestialSphereRadius * view;

    return galCoord;
}

float windowRatio() {
    const glm::vec2 windowRatio = global::windowDelegate->currentWindowSize();
    return windowRatio.x / windowRatio.y;
}

bool isCoordinateInView(const glm::dvec3& equatorial) {
    // Check if image coordinate is within current FOV
    const glm::dvec3 localCamera = equatorialToLocalCamera(equatorial);
    const glm::dvec3 coordsScreen = localCameraToScreenSpace3d(localCamera);
    const double r = windowRatio();

    const bool isCoordInView =
        std::abs(coordsScreen.x) < r &&
        std::abs(coordsScreen.y) < 1.f &&
        coordsScreen.z < 0.f;
    return isCoordInView;
}

// Transforms a pixel coordinate to a screen space coordinate
glm::vec2 pixelToScreenSpace2d(const glm::vec2& mouseCoordinate) {
    const glm::vec2 size = glm::vec2(global::windowDelegate->currentWindowSize());
    // Change origin to middle of the window
    glm::vec2 screenSpacePos = mouseCoordinate - (size / 2.f);
    // Ensure the upper right corner is positive on the y axis
    screenSpacePos *= glm::vec2(1.f, -1.f);
    // Transform pixel coordinates to screen space coordinates [-1,1][-ratio, ratio]
    screenSpacePos /= (0.5f * size.y);
    return screenSpacePos;
}

// The horizontal and vertical fov of the OpenSpace window
glm::dvec2 fovWindow() {
    // OpenSpace FOV
    const glm::dvec2 windowDim = glm::dvec2(global::windowDelegate->currentWindowSize());
    const double ratio = windowDim.y / windowDim.x;
    const double hFov = global::windowDelegate->getHorizFieldOfView();
    const glm::dvec2 OpenSpaceFOV = glm::dvec2(hFov, hFov * ratio);
    return OpenSpaceFOV;
}

double angleBetweenVectors(const glm::dvec3& start, const glm::dvec3& end) {
    // Find smallest angle between the two vectors
    const double cos = glm::dot(glm::normalize(start), glm::normalize(end));
    // Ensure cos is within defined interval [-1,1]
    return std::acos(std::clamp(cos, -1.0, 1.0));
}

glm::dmat4 incrementalAnimationMatrix(const glm::dvec3& start, const glm::dvec3& end,
                                      double percentage)
{
    const double smallestAngle = angleBetweenVectors(start, end);
    // Calculate rotation this frame
    const double rotationAngle = smallestAngle * percentage;

    // Create the rotation matrix
    const glm::dvec3 rotationAxis = glm::normalize(glm::cross(start, end));
    return glm::rotate(rotationAngle, rotationAxis);
}

double sizeFromFov(double fov, const glm::dvec3& worldPosition) {
    // Calculate the size with trigonometry
    //  /|
    // /_|    Adjacent is the horizontal line, opposite the vertical
    // \ |    Calculate for half the triangle first, then multiply with 2
    //  \|
    const double adjacent = glm::length(worldPosition);
    const double opposite = 2.0 * adjacent * glm::tan(glm::radians(fov * 0.5));
    return opposite;
}

template <>
double Animation<double>::newValue() const {
    if (!isAnimating()) {
        return _goal;
    }
    else {
        const double percentage = percentageSpent();
        const double diff = (_goal - _start) * ghoul::exponentialEaseOut(percentage);
        return _start + diff;
    }
}

template <>
glm::dmat4 Animation<glm::dvec3>::rotationMatrix() {
    if (!isAnimating()) {
        return glm::dmat4(1.0);
    }

    const double percentage = ghoul::sineEaseInOut(percentageSpent());
    const double increment = percentage - _lastPercentage;
    _lastPercentage = percentage;

    glm::dmat4 rotMat = skybrowser::incrementalAnimationMatrix(
        glm::normalize(_start),
        glm::normalize(_goal),
        increment
    );
    return rotMat;
}

template <>
glm::dvec3 Animation<glm::dvec3>::newValue() const {
    if (!isAnimating()) {
        return _goal;
    }
    const glm::dmat4 rotMat = skybrowser::incrementalAnimationMatrix(
        glm::normalize(_start),
        glm::normalize(_goal),
        ghoul::exponentialEaseOut(percentageSpent())
    );
    // Rotate direction
    return glm::dvec3(rotMat * glm::dvec4(_start, 1.0));;
}

} // namespace openspace::skybrowser
