/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/camera/camera.h>
#include <glm/gtx/vector_angle.hpp>
#include <cmath>

namespace openspace::skybrowser {

// Converts from spherical coordinates in the unit of degrees to cartesian coordianates
glm::dvec3 sphericalToCartesian(const glm::dvec2& coords) {
    glm::dvec2 coordsRadians = glm::radians(coords);

    glm::dvec3 cartesian = glm::dvec3(
        cos(coordsRadians.x) * cos(coordsRadians.y),
        sin(coordsRadians.x) * cos(coordsRadians.y),
        sin(coordsRadians.y)
    );

    return cartesian;
}

// Converts from cartesian coordianates to spherical in the unit of degrees
glm::dvec2 cartesianToSpherical(const glm::dvec3& coord) {
    // Equatorial coordinates RA = right ascension, Dec = declination
    double ra = atan2(coord.y, coord.x);
    double dec = atan2(coord.z, glm::sqrt((coord.x * coord.x) + (coord.y * coord.y)));

    ra = ra > 0 ? ra : ra + (2.0 * glm::pi<double>());

    glm::dvec2 celestialCoords{ ra, dec };

    return glm::degrees(celestialCoords);
}

glm::dvec3 galacticToEquatorial(const glm::dvec3& coords) {
    return glm::transpose(conversionMatrix) * glm::normalize(coords);
}
        
glm::dvec3 equatorialToGalactic(const glm::dvec3& coords) {
    // On the unit sphere
    glm::dvec3 rGalactic = conversionMatrix * glm::normalize(coords); 
    return rGalactic * CelestialSphereRadius;
}
        
glm::dvec3 localCameraToScreenSpace3d(const glm::dvec3& coords) {
    // Ensure that if the coord is behind the camera, 
    // the converted coordinate will be there too
    double zCoord = coords.z > 0 ? -ScreenSpaceZ : ScreenSpaceZ;

    // Calculate screen space coords x and y
    double tanX = coords.x / coords.z;
    double tanY = coords.y / coords.z;

    glm::dvec3 screenSpace = glm::dvec3(zCoord * tanX, zCoord * tanY, zCoord);
            
    return screenSpace;
}

glm::dvec3 localCameraToGalactic(const glm::dvec3& coords) {
    glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
    glm::dvec4 coordsVec4 = glm::dvec4(coords, 1.0) ;
    glm::dmat4 camMat = glm::inverse(
        global::navigationHandler->camera()->combinedViewMatrix()
    );
    // Subtract gamera position to get the view direction
    glm::dvec3 galactic = glm::dvec3(camMat * coordsVec4) - camPos;

    return glm::normalize(galactic) * skybrowser::CelestialSphereRadius;
}

glm::dvec3 localCameraToEquatorial(const glm::dvec3& coords) {
    // Calculate the galactic coordinate of the target direction 
    // projected onto the celestial sphere
    glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
    glm::dvec3 galactic = camPos + skybrowser::localCameraToGalactic(coords);

    return skybrowser::galacticToEquatorial(galactic);
}
    
glm::dvec3 equatorialToLocalCamera(const glm::dvec3& coords) {
    // Transform equatorial J2000 to galactic coord with infinite radius    
    glm::dvec3 galactic = equatorialToGalactic(coords);
    glm::dvec3 localCamera = galacticToLocalCamera(galactic);
    return localCamera;
}

glm::dvec3 galacticToLocalCamera(const glm::dvec3& coords) {
    // Transform vector to camera's local coordinate system
    glm::dmat4 camMat = global::navigationHandler->camera()->combinedViewMatrix();
    glm::dvec3 viewDirectionLocal = camMat * glm::dvec4(coords, 1.0);

    return glm::normalize(viewDirectionLocal);
}

double cameraRoll() {
    openspace::Camera* camera = global::navigationHandler->camera();
    glm::dvec3 upWorld = camera->lookUpVectorWorldSpace();
    glm::dvec3 forwardWorld = camera->viewDirectionWorldSpace();

    glm::dvec3 camUpJ2000 = skybrowser::galacticToEquatorial(upWorld);
    glm::dvec3 camForwardJ2000 = skybrowser::galacticToEquatorial(forwardWorld);

    glm::dvec3 crossUpNorth = glm::cross(camUpJ2000, skybrowser::NorthPole);
    double dotNorthUp = glm::dot(skybrowser::NorthPole, camUpJ2000);
    double dotCrossUpNorthForward = glm::dot(crossUpNorth, camForwardJ2000);
        
    return glm::degrees(atan2(dotCrossUpNorthForward, dotNorthUp));
}

glm::dvec3 cameraDirectionEquatorial() {
    // Get the view direction of the screen in cartesian J2000 coordinates
    return galacticToEquatorial(cameraDirectionGalactic());
}    
    
glm::dvec3 cameraDirectionGalactic() {
    // Get the view direction of the screen in galactic coordinates
    glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
    glm::dvec3 view = global::navigationHandler->camera()->viewDirectionWorldSpace();
    glm::dvec3 galCoord = camPos + (skybrowser::CelestialSphereRadius * view);

    return galCoord;
}

float windowRatio() {
    glm::vec2 windowRatio = global::windowDelegate->currentWindowSize();
    return windowRatio.x / windowRatio.y;
}

bool isCoordinateInView(const glm::dvec3& equatorial) {
    // Check if image coordinate is within current FOV
    glm::dvec3 localCamera = equatorialToLocalCamera(equatorial);
    glm::dvec3 coordsScreen = localCameraToScreenSpace3d(localCamera);
    double r = static_cast<float>(windowRatio());

    bool isCoordInView = abs(coordsScreen.x) < r && abs(coordsScreen.y) < 1.f && 
                                coordsScreen.z < 0;

    return isCoordInView;
}

// Transforms a pixel coordinate to a screen space coordinate
glm::vec2 pixelToScreenSpace2d(const glm::vec2& mouseCoordinate) {
    glm::vec2 size = glm::vec2(global::windowDelegate->currentWindowSize());
    // Change origin to middle of the window
    glm::vec2 screenSpacePos = mouseCoordinate - (size / 2.0f);
    // Ensure the upper right corner is positive on the y axis
    screenSpacePos *= glm::vec2(1.0f, -1.0f);
    // Transform pixel coordinates to screen space coordinates [-1,1][-ratio, ratio]
    screenSpacePos /= (0.5f * size.y);
    return screenSpacePos;
}

// The horizontal and vertical fov of the OpenSpace window
glm::dvec2 fovWindow() {
    // OpenSpace FOV
    glm::dvec2 windowDim = glm::dvec2(global::windowDelegate->currentWindowSize());
    double windowRatio = windowDim.y / windowDim.x;
    double hFov = global::windowDelegate->getHorizFieldOfView();
    glm::dvec2 OpenSpaceFOV = glm::dvec2(hFov, hFov * windowRatio);
    return OpenSpaceFOV;
}

double angleBetweenVectors(const glm::dvec3& start, const glm::dvec3& end) {
    // Find smallest angle between the two vectors
    double cos = glm::dot(start, end) / (glm::length(start) * glm::length(end));
    return std::acos(cos);
}

glm::dmat4 incrementalAnimationMatrix(const glm::dvec3& start, const glm::dvec3& end, 
                                      double deltaTime, double speedFactor) 
{        
    double smallestAngle = angleBetweenVectors(start, end);
    // Calculate rotation this frame
    double rotationAngle = smallestAngle * deltaTime * speedFactor;

    // Create the rotation matrix for local camera space
    glm::dvec3 rotationAxis = glm::normalize(glm::cross(start, end));
    return glm::rotate(rotationAngle, rotationAxis);
}
} // namespace openspace


    

