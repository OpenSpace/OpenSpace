#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/util/camera.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp> // For printing glm data
#define _USE_MATH_DEFINES
#include <math.h>


namespace openspace {
    namespace skybrowser {
        // Constants
        constexpr const double ScreenSpaceZ = -2.1;
        constexpr const glm::dvec3 NorthPole = { 0.0 , 0.0 , 1.0 };
        constexpr const double CelestialSphereRadius = std::numeric_limits<float>::max();  

        // Conversion matrix - J2000 equatorial <-> galactic
        // https://arxiv.org/abs/1010.3773v1
        const glm::dmat3 conversionMatrix = glm::dmat3({
                   -0.054875539390,  0.494109453633, -0.867666135681, // col 0
                   -0.873437104725, -0.444829594298, -0.198076389622, // col 1
                   -0.483834991775,  0.746982248696,  0.455983794523 // col 2
            });

        // Galactic coordinates are projected onto the celestial sphere
        // Equatorial coordinates are unit length
        // Conversion spherical <-> Cartesian
        glm::dvec2 cartesianToSpherical(glm::dvec3 coords);
        glm::dvec3 sphericalToCartesian(glm::dvec2 coords);

        // Conversion J2000 equatorial <-> galactic
        glm::dvec3 galacticToEquatorial(glm::dvec3 coords);
        glm::dvec3 galacticToLocalCamera(glm::dvec3 coords);
        glm::dvec3 equatorialToGalactic(glm::dvec3 coords);

        // Conversion J2000 equatorial / galactic <-> screen space
        glm::dvec3 equatorialToScreenSpace(glm::dvec3 coords);
        glm::dvec3 galacticToScreenSpace(glm::dvec3 coords);
        glm::dvec3 localCameraToGalactic(glm::dvec3 coords);
        glm::dvec3 localCameraToEquatorial(glm::dvec3 coords);

        // Camera roll and direction
        // Camera roll is with respect to the equatorial North Pole
        bool isCoordinateInView(glm::dvec3 equatorial);
        float windowRatio();
        double cameraRoll();
        glm::vec2 pixelToScreenSpace(glm::vec2& mouseCoordinate);
        glm::dvec2 fovWindow();
        glm::dvec3 cameraDirectionGalactic();
        glm::dvec3 cameraDirectionEquatorial();
        double angleVector(glm::dvec3 start, glm::dvec3 end);
        glm::dmat4 incrementalAnimationMatrix(glm::dvec3 start,
                                        glm::dvec3 end, double deltaTime, 
                                        double speedFactor = 1.0);       
    }
    // WorldWide Telescope messages
    namespace wwtmessage {
        inline int messageCounter{ 0 };
        ghoul::Dictionary moveCamera(const glm::dvec2 celestCoords,
            const double fov, const double roll, const bool shouldMoveInstantly = true);
        ghoul::Dictionary loadCollection(const std::string& url);
        ghoul::Dictionary setForeground(const std::string& name);
        ghoul::Dictionary addImage(const std::string& id, const std::string& url);
        ghoul::Dictionary removeImage(const std::string& id);
        ghoul::Dictionary setImageOpacity(const std::string& id, double opacity);
        ghoul::Dictionary setForegroundOpacity(double val);
        ghoul::Dictionary setLayerOrder(const std::string& id, int version);
    }
}


    
    

