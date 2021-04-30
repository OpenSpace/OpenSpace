
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

        glm::dvec2 galacticCartesianToJ2000(glm::dvec3 rGal) {
            glm::dvec3 J2000 = glm::transpose(conversionMatrix) * rGal;
            return cartesianToSpherical(J2000);
        }

        glm::dvec3 J2000ToGalacticCartesian(double ra, double dec, double distance) {
            glm::dvec3 rGalactic = conversionMatrix * sphericalToCartesian(glm::dvec2(ra, dec)); // on the unit sphere
            return distance * rGalactic;
        }
        
        glm::dvec2 galacticToScreenSpace(glm::dvec3 imageCoordsGalacticCartesian) {

            // Transform vector to camera's local coordinate system
            glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
            glm::dvec3 camToCoordsDir = glm::normalize(imageCoordsGalacticCartesian - camPos);
            glm::dmat4 camMat = global::navigationHandler->camera()->viewRotationMatrix();
            glm::dvec3 viewDirectionLocal = camMat * glm::dvec4(camToCoordsDir, 1.0);
            viewDirectionLocal = glm::normalize(viewDirectionLocal);

            // Calculate screen space coords x and y
            long double tan_x = viewDirectionLocal.x / viewDirectionLocal.z;
            long double tan_y = viewDirectionLocal.y / viewDirectionLocal.z;

            glm::dvec2 angleCoordsLocal = glm::dvec2(std::atanl(tan_x), std::atanl(tan_y));
            glm::dvec2 imageCoordsScreenSpace = glm::dvec2(SCREENSPACE_Z * static_cast<double>(std::tanl(angleCoordsLocal.x)), SCREENSPACE_Z * static_cast<double>(std::tanl(angleCoordsLocal.y)));
            
            return imageCoordsScreenSpace;
        }

        glm::dvec2 J2000ToScreenSpace(double ra, double dec) {
            // Transform equatorial J2000 to galactic coord with infinite radius
            constexpr double infinity = std::numeric_limits<float>::max();
            glm::dvec3 imageCoordsGalacticCartesian = J2000ToGalacticCartesian(ra, dec, infinity);
            // Transform galactic coord to screen space
            glm::dvec2 imageCoordsScreenSpace = galacticToScreenSpace(imageCoordsGalacticCartesian);
            return imageCoordsScreenSpace;
        }

    }
    
    

