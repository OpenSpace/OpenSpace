
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/util/camera.h>
#include <modules/skybrowser/include/utility.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp> // For printing glm data
#define _USE_MATH_DEFINES
#include <cmath>



namespace openspace::skybrowser {

        glm::dvec3 sphericalToCartesian(glm::dvec2 sphericalCoords) {

            glm::dvec3 cartesian = glm::dvec3(
                cos(sphericalCoords.x * DEG_TO_RAD) * cos(sphericalCoords.y * DEG_TO_RAD),
                sin(sphericalCoords.x * DEG_TO_RAD) * cos(sphericalCoords.y * DEG_TO_RAD),
                sin(sphericalCoords.y * DEG_TO_RAD)
            );

            return cartesian;
        }

        glm::dvec2 cartesianToSpherical(glm::dvec3 cartesianCoords) {

            double ra = atan2(cartesianCoords[1], cartesianCoords[0]);
            double dec = atan2(cartesianCoords[2], glm::sqrt((cartesianCoords[0] * cartesianCoords[0]) + (cartesianCoords[1] * cartesianCoords[1])));

            ra = ra > 0 ? ra : ra + (2 * M_PI);

            return glm::dvec2(RAD_TO_DEG * ra, RAD_TO_DEG * dec);
        }

        glm::dvec3 galacticCartesianToJ2000Cartesian(glm::dvec3 rGal) {
            return glm::transpose(conversionMatrix) * rGal;
        }

        glm::dvec2 galacticCartesianToJ2000Spherical(glm::dvec3 rGal) {
            return cartesianToSpherical(galacticCartesianToJ2000Cartesian(rGal));
        }

        glm::dvec3 J2000SphericalToGalacticCartesian(glm::dvec2 coords, double distance) {
            glm::dvec3 rGalactic = conversionMatrix * sphericalToCartesian(coords); // on the unit sphere
            return distance * rGalactic;
        }
        
        glm::dvec3 J2000CartesianToGalacticCartesian(glm::dvec3 coords, double distance) {
            glm::dvec3 rGalactic = conversionMatrix * glm::normalize(coords); // on the unit sphere
            return distance * rGalactic;
        }
        
        glm::dvec3 galacticToScreenSpace(glm::dvec3 imageCoordsGalacticCartesian) {

            glm::dvec3 viewDirectionLocal = galacticCartesianToCameraLocalCartesian(imageCoordsGalacticCartesian);
            // Ensure that if the coord is behind the camera, the converted coord will be there too
            double zCoord = viewDirectionLocal.z > 0 ? -SCREENSPACE_Z : SCREENSPACE_Z;

            // Calculate screen space coords x and y
            long double tan_x = viewDirectionLocal.x / viewDirectionLocal.z;
            long double tan_y = viewDirectionLocal.y / viewDirectionLocal.z;

            glm::dvec2 angleCoordsLocal = glm::dvec2(std::atanl(tan_x), std::atanl(tan_y));
            glm::dvec3 imageCoordsScreenSpace = glm::dvec3(zCoord * static_cast<double>(std::tanl(angleCoordsLocal.x)), zCoord * static_cast<double>(std::tanl(angleCoordsLocal.y)), zCoord);
            
            return imageCoordsScreenSpace;
        }

        glm::dvec3 galacticCartesianToCameraLocalCartesian(glm::dvec3 galCoords) {
            // Transform vector to camera's local coordinate system
            glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
            glm::dvec3 camToCoordsDir = glm::normalize(galCoords - camPos);
            glm::dmat4 camMat = global::navigationHandler->camera()->viewRotationMatrix();
            glm::dvec3 viewDirectionLocal = camMat * glm::dvec4(camToCoordsDir, 1.0);
            viewDirectionLocal = glm::normalize(viewDirectionLocal);
            return viewDirectionLocal;
        }

        glm::dvec3 J2000CartesianToScreenSpace(glm::dvec3 coords) {
            // Transform equatorial J2000 to galactic coord with infinite radius    
            glm::dvec3 imageCoordsGalacticCartesian = J2000CartesianToGalacticCartesian(coords, infinity);
            // Transform galactic coord to screen space
            return galacticToScreenSpace(imageCoordsGalacticCartesian);
        }

        glm::dvec3 J2000SphericalToScreenSpace(glm::dvec2 coords) {         
            // Transform equatorial J2000 to galactic coord with infinite radius
            glm::dvec3 imageCoordsGalacticCartesian = J2000SphericalToGalacticCartesian(coords, infinity);
            // Transform galactic coord to screen space
            return galacticToScreenSpace(imageCoordsGalacticCartesian);
        }

    }
    
    

