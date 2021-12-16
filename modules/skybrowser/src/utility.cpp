#include <modules/skybrowser/include/utility.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/camera/camera.h>
#include <glm/gtx/vector_angle.hpp>
#include <cmath> // For atan2
#define _USE_MATH_DEFINES
#include <math.h> // For M_PI


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

        ra = ra > 0 ? ra : ra + (2 * M_PI);

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
        double tan_x = coords.x / coords.z;
        double tan_y = coords.y / coords.z;

        glm::dvec3 screenSpace = glm::dvec3(zCoord * tan_x, zCoord * tan_y, zCoord);
            
        return screenSpace;
    }

    glm::dvec3 localCameraToGalactic(const glm::dvec3&  coords) {
        glm::dmat4 rotation = glm::inverse(
            global::navigationHandler->camera()->viewRotationMatrix());
        glm::dvec4 position = glm::dvec4(coords, 1.0);

        return glm::normalize(rotation * position) * skybrowser::CelestialSphereRadius;
    }

    glm::dvec3 localCameraToEquatorial(const glm::dvec3&  coords) {
        // Calculate the galactic coordinate of the target direction 
        // projected onto the celestial sphere
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        glm::dvec3 galactic = camPos + skybrowser::localCameraToGalactic(coords);

        return skybrowser::galacticToEquatorial(galactic);
    }
    
    glm::dvec3 equatorialToLocalCamera(const glm::dvec3& coords)
    {
        // Transform equatorial J2000 to galactic coord with infinite radius    
        glm::dvec3 galactic = equatorialToGalactic(coords);
        glm::dvec3 localCamera = galacticToLocalCamera(galactic);
        return localCamera;
    }

    glm::dvec3 galacticToLocalCamera(const glm::dvec3&  coords) {
        // Transform vector to camera's local coordinate system
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        glm::dmat4 camMat = global::navigationHandler->camera()->viewRotationMatrix();
        glm::dvec3 viewDirectionWorld = glm::normalize(coords - camPos);
        glm::dvec3 viewDirectionLocal = camMat * glm::dvec4(viewDirectionWorld, 1.0);

        return glm::normalize(viewDirectionLocal);
    }

    glm::dvec3 equatorialToScreenSpace3d(const glm::dvec3& coords) {
        // Transform equatorial J2000 to galactic coord with infinite radius    
        glm::dvec3 galactic = equatorialToGalactic(coords);
        glm::dvec3 localCameraSpace = galacticToLocalCamera(coords);
        // Transform galactic coord to screen space
        return localCameraToScreenSpace3d(localCameraSpace);
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

    bool isCoordinateInView(const glm::dvec3&  equatorial) {
        // Check if image coordinate is within current FOV
        glm::dvec3 coordsScreen = equatorialToScreenSpace3d(equatorial);
        double r = static_cast<float>(windowRatio());

        bool isCoordInView = abs(coordsScreen.x) < r && abs(coordsScreen.y) < 1.f && 
                                 coordsScreen.z < 0;

        // If the coordinate is not in view, rotate camera
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

    double angleBetweenVectors(const glm::dvec3&  start, const glm::dvec3&  end) {
    
        // Find smallest angle between the two vectors
        double cos = glm::dot(start, end) / (glm::length(start) * glm::length(end));
        return std::acos(cos);
    }

    glm::dmat4 incrementalAnimationMatrix(const glm::dvec3&  start,
                                    const glm::dvec3&  end, double deltaTime, 
                                    double speedFactor) {
        
        double smallestAngle = angleBetweenVectors(start, end);
        // Calculate rotation this frame
        double rotationAngle = smallestAngle * deltaTime * speedFactor;

        // Create the rotation matrix for local camera space
        glm::dvec3 rotationAxis = glm::normalize(glm::cross(start, end));
        return glm::rotate(rotationAngle, rotationAxis);
    }

}


    

