#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/helper.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/string_cast.hpp>
#include <glm/gtx/matrix_decompose.hpp>
#include <glm/gtx/vector_angle.hpp>
#include <optional>
#define _USE_MATH_DEFINES
#include <cmath>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyTarget";

    constexpr const std::array<const char*, 7> UniformNames = {
        "modelTransform", "viewProj", "showCrosshair", "showRectangle", "lineWidth", 
        "dimensions", "lineColor"
    };

    constexpr const openspace::properties::Property::PropertyInfo BrowserIDInfo =
    {
        "BrowserID",
        "Browser Identifier",
        "The identifier of the corresponding sky browser."
    };

    constexpr const openspace::properties::Property::PropertyInfo CrosshairThresholdInfo =
    {
        "CrosshairThreshold",
        "Crosshair Threshold",
        "When the field of view is smaller than the crosshair threshold, a crosshair will"
        "be rendered in the target."
    }; 
    
    constexpr const openspace::properties::Property::PropertyInfo RectangleThresholdInfo =
    {
        "RectangleThreshold",
        "Rectangle Threshold",
        "When the field of view is larger than the rectangle threshold, a rectangle will"
        "be rendered in the target."
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyTarget)]] Parameters {

        // [[codegen::verbatim(BrowserIDInfo.description)]]
        std::optional<std::string> browserID;

        // [[codegen::verbatim(CrosshairThresholdInfo.description)]]
        std::optional<float> crosshairThreshold;

        // [[codegen::verbatim(RectangleThresholdInfo.description)]]
        std::optional<float> rectangleThreshold;
    };

#include "screenspaceskytarget_codegen.cpp"

} //namespace

namespace openspace {
    ScreenSpaceSkyTarget::ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , _skyBrowserID(BrowserIDInfo)
        , _skyBrowser(nullptr)
        , _showCrosshairThreshold(CrosshairThresholdInfo, 2.0f, 0.1f, 70.f)
        , _showRectangleThreshold(RectangleThresholdInfo, 0.6f, 0.1f, 70.f)
        , _color(220, 220, 220)  
    {
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _skyBrowserID = p.browserID.value_or(_skyBrowserID);
        _showCrosshairThreshold = p.crosshairThreshold.value_or(_showCrosshairThreshold);
        _showRectangleThreshold = p.rectangleThreshold.value_or(_showRectangleThreshold);
         
        addProperty(_skyBrowserID);
        addProperty(_showCrosshairThreshold);
        addProperty(_showRectangleThreshold);

        // If the ID changes for the corresponding browser, update
        _skyBrowserID.onChange([&]() {
            initializeWithBrowser();
            });

        // Always make sure that the target and browser are visible together
        _enabled.onChange([&]() {
            if (_skyBrowser) {
                _skyBrowser->property("Enabled")->set(_enabled.value());
            }
            });

        // Set a unique identifier
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyTarget";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

        // Set the position to screen space z
        glm::dvec3 startPos{ _cartesianPosition.value().x, _cartesianPosition.value().y, 
            skybrowser::SCREENSPACE_Z };

        _cartesianPosition.setValue(startPos);
    }

    ScreenSpaceSkyTarget::~ScreenSpaceSkyTarget() {
        if (_lockTargetThread.joinable()) {
            _lockTargetThread.join();
        }
    }

    // Pure virtual in the screen space renderable class and hence must be defined 
    void ScreenSpaceSkyTarget::bindTexture() {

    }

    void ScreenSpaceSkyTarget::initializeWithBrowser() {
        _skyBrowser = dynamic_cast<ScreenSpaceSkyBrowser*>(
            global::renderEngine->screenSpaceRenderable(_skyBrowserID.value()));
        if (_skyBrowser) {
            _color = _skyBrowser->getColor();
            _objectSize = _skyBrowser->getBrowserPixelDimensions();
            setVerticalFOV(_skyBrowser->_vfieldOfView.value());
        }
    }

    bool ScreenSpaceSkyTarget::isReady() const {
        return _shader != nullptr;
    }

    bool ScreenSpaceSkyTarget::initializeGL() {

        glGenVertexArrays(1, &_vertexArray);
        glGenBuffers(1, &_vertexBuffer);
        createShaders();

        return isReady();
    }

    glm::mat4 ScreenSpaceSkyTarget::scaleMatrix() {
        // To ensure the plane has the right ratio
        // The _scale us how much of the windows height the browser covers: eg a browser 
        // that covers 0.25 of the height of the window will have scale = 0.25
        float ratio = static_cast<float>(_objectSize.x) / 
            static_cast<float>(_objectSize.y);

        glm::mat4 scale = glm::scale(
            glm::mat4(1.f),
            glm::vec3(ratio * _scale, _scale, 1.f)
        );

        return scale;
    }


    void ScreenSpaceSkyTarget::createShaders() {

        _shader = global::renderEngine->buildRenderProgram(
            "ScreenSpaceProgram",
            absPath("${MODULE_SKYBROWSER}/shaders/target_vs.glsl"),
            absPath("${MODULE_SKYBROWSER}/shaders/target_fs.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

    }

    void ScreenSpaceSkyTarget::setColor(glm::ivec3 color) {
        _color = color;
    }

    glm::ivec3 ScreenSpaceSkyTarget::getColor() {
        return _color;
    }

    ScreenSpaceSkyBrowser* ScreenSpaceSkyTarget::getSkyBrowser() {
        return _skyBrowser;
    }

    void ScreenSpaceSkyTarget::render() {

        glDisable(GL_CULL_FACE);
       
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() * 
            localRotationMatrix() * scaleMatrix();
        float lineWidth = 0.0016f/_scale.value();

        _shader->activate();

        bool showCrosshair = _verticalFOV < _showCrosshairThreshold;
        bool showRect = _verticalFOV > _showRectangleThreshold;
        glm::vec4 color = { glm::vec3(_color) / 255.f, _opacity.value() };
       
        _shader->setUniform(_uniformCache.showCrosshair, showCrosshair);
        _shader->setUniform(_uniformCache.showRectangle, showRect);
        _shader->setUniform(_uniformCache.lineWidth, lineWidth);
        _shader->setUniform(_uniformCache.dimensions, glm::vec2(_objectSize));
        _shader->setUniform(_uniformCache.modelTransform, modelTransform);
        _shader->setUniform(_uniformCache.lineColor, color);
        _shader->setUniform(
            _uniformCache.viewProj,
            global::renderEngine->scene()->camera()->viewProjectionMatrix()
        );

        glBindVertexArray(rendering::helper::vertexObjects.square.vao);
        glDrawArrays(GL_TRIANGLES, 0, 6);

        glEnable(GL_CULL_FACE);

        _shader->deactivate();
    }

    void ScreenSpaceSkyTarget::setDimensions(glm::vec2 dimensions) {
        _objectSize = dimensions;
    }

    glm::dvec3 ScreenSpaceSkyTarget::getTargetDirectionGalactic() {
        glm::dmat4 rotation = glm::inverse(
            global::navigationHandler->camera()->viewRotationMatrix());
        glm::dvec4 position = glm::dvec4(_cartesianPosition.value(), 1.0);

        return glm::normalize(rotation * position);
    }

    void ScreenSpaceSkyTarget::setVerticalFOV(float VFOV) { 
        _verticalFOV = VFOV;

        // Update the scale of the target
        float horizFOV = global::windowDelegate->getHorizFieldOfView();
        glm::ivec2 windowRatio = global::windowDelegate->currentWindowSize();
        float verticFOV = horizFOV * (static_cast<float>(windowRatio.y) / static_cast<float>(windowRatio.x));
        _scale = std::max((VFOV / verticFOV), (_showRectangleThreshold.value() / verticFOV));
    }

    void ScreenSpaceSkyTarget::unlock() {
        _isLocked = false;
        if (_lockTargetThread.joinable()) {
            _lockTargetThread.join();
        }
    }

    void ScreenSpaceSkyTarget::lock() {
        if (_isLocked) {
            unlock();
        }
        _isLocked = true;
        _lockedCoords = getTargetDirectionCelestial();

        // Start a thread to enable user interactions while locking target
        _lockTargetThread = std::thread([&] {
            while (_isLocked) {
                glm::vec3 imageCoordsScreenSpace = skybrowser::J2000SphericalToScreenSpace(_lockedCoords);
                _cartesianPosition = imageCoordsScreenSpace;
            }
        });
    }

    bool ScreenSpaceSkyTarget::isLocked() {
        return _isLocked;
    }

    glm::dvec2 ScreenSpaceSkyTarget::getTargetDirectionCelestial() {
        // Calculate the galactic coordinate of the target direction 
        // with infinite radius
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        constexpr double infinity = std::numeric_limits<float>::max();
        glm::dvec3 galCoord = camPos + (infinity * getTargetDirectionGalactic());
        return skybrowser::galacticCartesianToJ2000Spherical(galCoord);
    }

    void ScreenSpaceSkyTarget::animateToCoord(double deltaTime) {
        if (_isAnimated) {
            // Find smallest angle between the two vectors
            double smallestAngle = std::acos(glm::dot(_coordsStartAnimation, _coordsEndAnimation) / (glm::length(_coordsStartAnimation) * glm::length(_coordsEndAnimation)));
            // Only keep animating when target is not at final position
            if (abs(smallestAngle) > 0.0005) {
                // Calculate rotation this frame
                double rotationAngle = smallestAngle * deltaTime * 5.0;
                // Create the rotation matrix
                glm::dvec3 rotationAxis = glm::normalize(glm::cross(_coordsStartAnimation, _coordsEndAnimation));
                glm::dmat4 rotmat = glm::rotate(rotationAngle, rotationAxis);
                // Rotate target direction
                glm::dvec3 newDir = rotmat * glm::dvec4(_coordsStartAnimation, 1.0);
                // Convert to screenspace
                _cartesianPosition = skybrowser::J2000CartesianToScreenSpace(newDir);
                // Update position
                _coordsStartAnimation = glm::normalize(newDir);
            }
            else {
                // Set the exact target position 
                _cartesianPosition = skybrowser::J2000CartesianToScreenSpace(_coordsEndAnimation);
                // Lock target when it first arrives to the position
                if (!_isLocked && _lockAfterAnimation) {
                    lock();
                }
                // When target is in position, animate the FOV until it has finished
                if(animateToFOV(_FOVEndAnimation, deltaTime)) {
                    _isAnimated = false;
                }
            }
        }
    }

    bool ScreenSpaceSkyTarget::animateToFOV(float endFOV, float deltaTime) {
        if (!_skyBrowser) {
            initializeWithBrowser();
        }
        if (_skyBrowser) {
            double distance = static_cast<double>(_skyBrowser->_vfieldOfView.value()) - endFOV;
            // If distance is too large, keep animating
            if (abs(distance) > 0.01) {
                _skyBrowser->scrollZoom(distance);
                return false;
            }
            // Animation is finished
            return true;
        }
        else {
            LINFO("Target can't connect to browser!");
        }
        
        return true;     
    }

    void ScreenSpaceSkyTarget::startAnimation(glm::dvec2 coordsEnd, float FOVEnd,
        bool lockAfterwards) {
        // Save the Cartesian celestial coordinates for animation
        // The coordinates are Cartesian to avoid wrap-around issues
        _coordsEndAnimation = glm::normalize(skybrowser::sphericalToCartesian(coordsEnd));
        _coordsStartAnimation = glm::normalize(skybrowser::sphericalToCartesian(
            getTargetDirectionCelestial()));
        _FOVEndAnimation = FOVEnd;
        _isAnimated = true;
        _lockAfterAnimation = lockAfterwards;
    }
    properties::FloatProperty& ScreenSpaceSkyTarget::getOpacity() {
        return _opacity;
    }
}
