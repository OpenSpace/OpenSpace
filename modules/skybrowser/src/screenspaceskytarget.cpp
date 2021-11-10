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
        "BrowserId",
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

    constexpr const openspace::properties::Property::PropertyInfo AnimationSpeedInfo =
    {
        "AnimationSpeed",
        "Animation Speed",
        "The factor which is multiplied with the animation speed of the target."
    };

    constexpr const openspace::properties::Property::PropertyInfo AnimationThresholdInfo =
    {
        "AnimationThreshold",
        "Animation Threshold",
        "The threshold for when the target is determined to have appeared at its "
        "destination. Angle in radians between the destination and the target position in"
        "equatorial Cartesian coordinate system."
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyTarget)]] Parameters {

        // [[codegen::verbatim(BrowserIDInfo.description)]]
        std::optional<std::string> browserId;

        // [[codegen::verbatim(CrosshairThresholdInfo.description)]]
        std::optional<float> crosshairThreshold;

        // [[codegen::verbatim(RectangleThresholdInfo.description)]]
        std::optional<float> rectangleThreshold;

        // [[codegen::verbatim(RectangleThresholdInfo.description)]]
        std::optional<double> animationSpeed;

        // [[codegen::verbatim(RectangleThresholdInfo.description)]]
        std::optional<float> animationThreshold;

    };

#include "screenspaceskytarget_codegen.cpp"

} //namespace

namespace openspace {
    ScreenSpaceSkyTarget::ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , _skyBrowserId(BrowserIDInfo)
        , _skyBrowser(nullptr)
        , _showCrosshairThreshold(CrosshairThresholdInfo, 2.0f, 0.1f, 70.f)
        , _showRectangleThreshold(RectangleThresholdInfo, 0.6f, 0.1f, 70.f)
        , _stopAnimationThreshold(AnimationThresholdInfo, 0.0005, 0.0, 1.0)
        , _animationSpeed(AnimationSpeedInfo, 5.0, 0.1, 10.0)
        , _color(220, 220, 220)  
    {
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _skyBrowserId = p.browserId.value_or(_skyBrowserId);
        _showCrosshairThreshold = p.crosshairThreshold.value_or(_showCrosshairThreshold);
        _showRectangleThreshold = p.rectangleThreshold.value_or(_showRectangleThreshold);
        _stopAnimationThreshold = p.crosshairThreshold.value_or(_stopAnimationThreshold);
        _animationSpeed = p.animationSpeed.value_or(_animationSpeed);
         
        addProperty(_skyBrowserId);
        addProperty(_showCrosshairThreshold);
        addProperty(_showRectangleThreshold);
        addProperty(_stopAnimationThreshold);
        addProperty(_animationSpeed);

        // If the ID changes for the corresponding browser, update
        _skyBrowserId.onChange([&]() {
            connectoToSkyBrowser();
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
            skybrowser::ScreenSpaceZ };

        _cartesianPosition.setValue(startPos);
    }

    ScreenSpaceSkyTarget::~ScreenSpaceSkyTarget() {
        if (_lockTarget.joinable()) {
            _lockTarget.join();
        }
    }

    // Pure virtual in the screen space renderable class and hence must be defined 
    void ScreenSpaceSkyTarget::bindTexture() {

    }

    bool ScreenSpaceSkyTarget::connectoToSkyBrowser() {
        _skyBrowser = dynamic_cast<ScreenSpaceSkyBrowser*>(
            global::renderEngine->screenSpaceRenderable(_skyBrowserId.value()));
        matchAppearanceToSkyBrowser();
        return _skyBrowser;
    }

    void ScreenSpaceSkyTarget::matchAppearanceToSkyBrowser() {
        if (_skyBrowser) {
            _color = _skyBrowser->borderColor();
            _objectSize = _skyBrowser->browserPixelDimensions();
            setScale(_skyBrowser->verticalFov());
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

    bool ScreenSpaceSkyTarget::deinitializeGL()
    {
        if (isLocked()) {
            unlock();
         }
        return ScreenSpaceRenderable::deinitializeGL();
    }

    glm::mat4 ScreenSpaceSkyTarget::scaleMatrix() {
        // To ensure the plane has the right ratio
        // The _scale us how much of the windows height the browser covers: e.g. a browser 
        // that covers 0.25 of the height of the window will have scale = 0.25
        glm::vec2 floatObjectSize = glm::abs(_objectSize);
        float ratio = floatObjectSize.x / floatObjectSize.y;

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

    glm::ivec3 ScreenSpaceSkyTarget::borderColor() const {
        return _color;
    }

    ScreenSpaceSkyBrowser* ScreenSpaceSkyTarget::getSkyBrowser() {
        return _skyBrowser;
    }

    void ScreenSpaceSkyTarget::render() {

        bool showCrosshair = false;
        bool showRectangle = true;
        if (_skyBrowser) {
            showCrosshair = _skyBrowser->verticalFov() < _showCrosshairThreshold;
            showRectangle = _skyBrowser->verticalFov() > _showRectangleThreshold;
        }
        
        glm::vec4 color = { glm::vec3(_color) / 255.f, _opacity.value() };
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() * 
            localRotationMatrix() * scaleMatrix();
        float lineWidth = 0.0016f/_scale.value();

        glDisable(GL_CULL_FACE);

        _shader->activate();
        _shader->setUniform(_uniformCache.showCrosshair, showCrosshair);
        _shader->setUniform(_uniformCache.showRectangle, showRectangle);
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

    glm::dvec3 ScreenSpaceSkyTarget::directionGalactic() const {

        glm::dmat4 rotation = glm::inverse(
            global::navigationHandler->camera()->viewRotationMatrix()
        );
        glm::dvec4 position = glm::dvec4(_cartesianPosition.value(), 1.0);

        return glm::normalize(rotation * position);
    }

    // Update the scale of the target (the height of the target in relation to the 
    // OpenSpace window)
    void ScreenSpaceSkyTarget::setScale(float verticalFov) {

        glm::dvec2 fovs = skybrowser::fovWindow();
        
        // Cap the scale at small scales so it is still visible
        float heightRatio = verticalFov / fovs.y;
        float smallestHeightRatio = _showRectangleThreshold.value() / fovs.y;
        
        _scale = std::max(heightRatio, smallestHeightRatio);
    }

    void ScreenSpaceSkyTarget::unlock() {
        _isLocked = false;
        if (_lockTarget.joinable()) {
            _lockTarget.join();
        }
    }

    void ScreenSpaceSkyTarget::lock() {
        if (_isLocked) {
            unlock();
        }
        _isLocked = true;
        _lockedCoordinates = directionEquatorial();

        // Start a thread to enable user interactions while locking target
        _lockTarget = std::thread([&] {
            while (_isLocked) {
                _cartesianPosition = skybrowser::equatorialToScreenSpace3d(
                    _lockedCoordinates
                );
            }
        });
    }

    bool ScreenSpaceSkyTarget::isLocked() {
        return _isLocked;
    }

    bool ScreenSpaceSkyTarget::isAnimated()
    {
        return _isAnimated;
    }

    void ScreenSpaceSkyTarget::startAnimation(glm::dvec3 end, bool shouldLockAfter)
    {
        _animationStart = glm::normalize(directionEquatorial());
        _animationEnd = glm::normalize(end);
        _shouldLockAfterAnimation = shouldLockAfter;
        _isAnimated = true;
    }

    void ScreenSpaceSkyTarget::incrementallyAnimateToCoordinate(float deltaTime)
    {
        // Find smallest angle between the two vectors
        double smallestAngle = skybrowser::angleBetweenVectors(_animationStart, _animationEnd);
        const bool shouldAnimate = smallestAngle > _stopAnimationThreshold;

        // Only keep animating when target is not at goal position
        if (shouldAnimate) {
            glm::dmat4 rotMat = skybrowser::incrementalAnimationMatrix(
                _animationStart,
                _animationEnd,
                deltaTime,
                _animationSpeed
            );

            // Rotate target direction
            glm::dvec3 newDir = rotMat * glm::dvec4(_animationStart, 1.0);
            
            // Convert to screen space
            _cartesianPosition = skybrowser::equatorialToScreenSpace3d(newDir);
            
            // Update position
            _animationStart = glm::normalize(newDir);
        }
        else {
            // Set the exact target position 
            _cartesianPosition = skybrowser::equatorialToScreenSpace3d(_animationEnd);
            _isAnimated = false;
            
            // Lock target when it first arrives to the position
            if (!isLocked() && _shouldLockAfterAnimation) {
                lock();
            }
        }      
    }

    glm::dvec3 ScreenSpaceSkyTarget::directionEquatorial() const {
        // Calculate the galactic coordinate of the target direction 
        // projected onto the celestial sphere
        return skybrowser::localCameraToEquatorial(_cartesianPosition.value());
    }


    void ScreenSpaceSkyTarget::highlight(glm::ivec3 addition)
    {
        _color += addition;
    }

    void ScreenSpaceSkyTarget::removeHighlight(glm::ivec3 removal)
    {
        _color -= removal;
    }

    float ScreenSpaceSkyTarget::opacity() const {
        return _opacity.value();
    }

    void ScreenSpaceSkyTarget::setOpacity(float opacity) {
        _opacity = opacity;
    }
}
