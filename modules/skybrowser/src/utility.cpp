
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/util/camera.h>
#include <modules/skybrowser/include/utility.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp> // For printing glm data
#define _USE_MATH_DEFINES
#include <cmath>



namespace openspace::skybrowser {

    glm::dvec3 sphericalToCartesian(glm::dvec2 coords) {

        glm::dvec2 coordsRadians = glm::radians(coords);

        glm::dvec3 cartesian = glm::dvec3(
            cos(coordsRadians.x) * cos(coordsRadians.y),
            sin(coordsRadians.x) * cos(coordsRadians.y),
            sin(coordsRadians.y)
        );

        return cartesian;
    }

    glm::dvec2 cartesianToSpherical(glm::dvec3 coord) {
        // Equatorial coordinates RA = right ascension, Dec = declination
        double ra = atan2(coord.y, coord.x);
        double dec = atan2(coord.z, glm::sqrt((coord.x * coord.x) + (coord.y * coord.y)));

        ra = ra > 0 ? ra : ra + (2 * M_PI);

        glm::dvec2 celestialCoords{ ra, dec };

        return glm::degrees(celestialCoords);
    }

    glm::dvec3 galacticToEquatorial(glm::dvec3 coords) {
        return glm::transpose(conversionMatrix) * glm::normalize(coords);
    }
        
    glm::dvec3 equatorialToGalactic(glm::dvec3 coords) {
        // On the unit sphere
        glm::dvec3 rGalactic = conversionMatrix * glm::normalize(coords); 
        return rGalactic * CelestialSphereRadius;
    }
        
    glm::dvec3 galacticToScreenSpace(glm::dvec3 coords) {

        glm::dvec3 localCameraSpace = galacticToCameraLocal(coords);
        // Ensure that if the coord is behind the camera, 
        // the converted coordinate will be there too
        double zCoord = localCameraSpace.z > 0 ? -ScreenSpaceZ : ScreenSpaceZ;

        // Calculate screen space coords x and y
        double tan_x = localCameraSpace.x / localCameraSpace.z;
        double tan_y = localCameraSpace.y / localCameraSpace.z;

        glm::dvec3 screenSpace = glm::dvec3(zCoord * tan_x, zCoord * tan_y, zCoord);
            
        return screenSpace;
    }

    glm::dvec3 screenSpaceToGalactic(glm::dvec3 coords) {
        glm::dmat4 rotation = glm::inverse(
            global::navigationHandler->camera()->viewRotationMatrix());
        glm::dvec4 position = glm::dvec4(coords, 1.0);

        return glm::normalize(rotation * position) * skybrowser::CelestialSphereRadius;
    }

    glm::dvec3 screenSpaceToEquatorial(glm::dvec3 coords) {
        // Calculate the galactic coordinate of the target direction 
        // projected onto the celestial sphere
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        glm::dvec3 galactic = camPos + skybrowser::screenSpaceToGalactic(coords);

        return skybrowser::galacticToEquatorial(galactic);
    }
    
    glm::dvec3 galacticToCameraLocal(glm::dvec3 coords) {
        // Transform vector to camera's local coordinate system
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        glm::dmat4 camMat = global::navigationHandler->camera()->viewRotationMatrix();
        glm::dvec3 viewDirectionWorld = glm::normalize(coords - camPos);
        glm::dvec3 viewDirectionLocal = camMat * glm::dvec4(viewDirectionWorld, 1.0);

        return glm::normalize(viewDirectionLocal);
    }

    glm::dvec3 equatorialToScreenSpace(glm::dvec3 coords) {
        // Transform equatorial J2000 to galactic coord with infinite radius    
        glm::dvec3 galactic = equatorialToGalactic(coords);
        // Transform galactic coord to screen space
        return galacticToScreenSpace(galactic);
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
}

// WWT messages
namespace openspace::wwtmessage {
    // WWT messages
    ghoul::Dictionary moveCamera(const glm::dvec2 celestCoords, const double fov, 
                                 const double roll, const bool moveInstantly) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;

        // Create message
        msg.setValue("event", "center_on_coordinates"s);
        msg.setValue("ra", celestCoords.x);
        msg.setValue("dec", celestCoords.y);
        msg.setValue("fov", fov);
        msg.setValue("roll", roll);
        msg.setValue("instant", moveInstantly);

        return msg;
    }

    ghoul::Dictionary loadCollection(const std::string& url) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "load_image_collection"s);
        msg.setValue("url", url);
        msg.setValue("loadChildFolders", true);

        return msg;
    }

    ghoul::Dictionary setForeground(const std::string& name) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "set_foreground_by_name"s);
        msg.setValue("name", name);

        return msg;
    }

    ghoul::Dictionary addImage(const std::string& id, const std::string& url) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_create"s);
        msg.setValue("id", id);
        msg.setValue("url", url);
        msg.setValue("mode", "preloaded"s);
        msg.setValue("goto", false);

        return msg;
    }

    ghoul::Dictionary removeImage(const std::string& imageId) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_remove"s);
        msg.setValue("id", imageId);

        return msg;
    }

    ghoul::Dictionary setImageOpacity(const std::string& imageId, double opacity) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_set"s);
        msg.setValue("id", imageId);
        msg.setValue("setting", "opacity"s);
        msg.setValue("value", opacity);

        return msg;
    }

    ghoul::Dictionary setForegroundOpacity(double val) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "set_foreground_opacity"s);
        msg.setValue("value", val);

        return msg;
    }

    ghoul::Dictionary setLayerOrder(const std::string& id, int order, int version) {
        // The lower the layer order, the more towards the back the image is placed
        // 0 is the background
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_order"s);
        msg.setValue("id", id);
        msg.setValue("order", order);
        msg.setValue("version", version);

        return msg;
    }
}
  
    
    

