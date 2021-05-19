
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

// WWT messages
namespace openspace::wwtmessage {
    // WWT messages
    ghoul::Dictionary moveCamera(const glm::dvec2 celestCoords, const double fov, const bool moveInstantly) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;

        // Calculate roll between up vector of camera and J2000 equatorial north
        glm::dvec3 upVector = global::navigationHandler->camera()->lookUpVectorWorldSpace();
        glm::dvec3 viewVector = global::navigationHandler->camera()->viewDirectionWorldSpace();
        glm::dvec3 camUpJ2000 = skybrowser::galacticCartesianToJ2000Cartesian(upVector);
        glm::dvec3 camForwardJ2000 = skybrowser::galacticCartesianToJ2000Cartesian(viewVector);

        glm::dvec3 crossUpNorth = glm::cross(camUpJ2000, skybrowser::NORTH_POLE);
        double dotNorthUp = glm::dot(skybrowser::NORTH_POLE, camUpJ2000);
        double dotCrossUpNorthForward = glm::dot(crossUpNorth, camForwardJ2000);
        double roll = glm::degrees(atan2(dotCrossUpNorthForward, dotNorthUp));

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

    ghoul::Dictionary createImageLayer(ImageData& image, int id) {
        std::string idString = std::to_string(id);
        image.id = id;

        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_create"s);
        msg.setValue("id", idString);
        msg.setValue("url", image.imageUrl);
        return msg;
    }

    ghoul::Dictionary removeImageLayer(const std::string& id) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_remove"s);
        msg.setValue("id", id);

        return msg;
    }

    ghoul::Dictionary setLayerOpacity(const ImageData& image, double opacity) {
        std::string idString = std::to_string(image.id);
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_set"s);
        msg.setValue("id", idString);
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
}
  
    
    

