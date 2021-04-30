#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/util/camera.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp> // For printing glm data
#define _USE_MATH_DEFINES
#include <math.h>


namespace openspace::skybrowser {
    const double SCREENSPACE_Z = -2.1;
    const double RAD_TO_DEG = 180.0 / M_PI;
    const double DEG_TO_RAD = M_PI / 180.0;

    // Conversion matrix from this paper: https://arxiv.org/abs/1010.3773v1
    const glm::dmat3 conversionMatrix = glm::dmat3({
               -0.054875539390,  0.494109453633, -0.867666135681, // col 0
               -0.873437104725, -0.444829594298, -0.198076389622, // col 1
               -0.483834991775,  0.746982248696,  0.455983794523 // col 2
        });

    // J2000 to galactic conversion and vice versa
    glm::dvec2 cartesianToSpherical(glm::dvec3 cartesianCoords);
    glm::dvec3 sphericalToCartesian(glm::dvec2 sphericalCoords);
    glm::dvec2 galacticCartesianToJ2000(glm::dvec3 rGal);
    glm::dvec3 J2000ToGalacticCartesian(double ra, double dec, double distance);
    glm::dvec2 J2000ToScreenSpace(double ra, double dec);
    
    glm::dvec2 galacticToScreenSpace(glm::dvec3 galacticCoord);
}
    
    

