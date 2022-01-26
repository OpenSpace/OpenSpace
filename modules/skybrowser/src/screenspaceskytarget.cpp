#include <modules/skybrowser/include/screenspaceskytarget.h>

#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/helper.h>
#include <openspace/scene/scene.h>
#include <openspace/camera/camera.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyTarget";

    constexpr const std::array<const char*, 7> UniformNames = {
        "modelTransform", "viewProj", "showCrosshair", "showRectangle", "lineWidth", 
        "dimensions", "lineColor"
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
        "TrAnimation Speed",
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

        // [[codegen::verbatim(CrosshairThresholdInfo.description)]]
        std::optional<float> crosshairThreshold;

        // [[codegen::verbatim(RectangleThresholdInfo.description)]]
        std::optional<float> rectangleThreshold;

        // [[codegen::verbatim(AnimationSpeedInfo.description)]]
        std::optional<double> animationSpeed;

        // [[codegen::verbatim(AnimationThresholdInfo.description)]]
        std::optional<float> animationThreshold;

    };

#include "screenspaceskytarget_codegen.cpp"

} //namespace

namespace openspace {
    ScreenSpaceSkyTarget::ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , _showCrosshairThreshold(CrosshairThresholdInfo, 2.0f, 0.1f, 70.f)
        , _showRectangleThreshold(RectangleThresholdInfo, 0.6f, 0.1f, 70.f)
        , _stopAnimationThreshold(AnimationThresholdInfo, 0.0005, 0.0, 0.005)
        , _animationSpeed(AnimationSpeedInfo, 5.0, 0.1, 10.0)
        , _borderColor(220, 220, 220)  
    {
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _showCrosshairThreshold = p.crosshairThreshold.value_or(_showCrosshairThreshold);
        _showRectangleThreshold = p.rectangleThreshold.value_or(_showRectangleThreshold);
        _stopAnimationThreshold = p.crosshairThreshold.value_or(_stopAnimationThreshold);
        _animationSpeed = p.animationSpeed.value_or(_animationSpeed);
         
        addProperty(_showCrosshairThreshold);
        addProperty(_showRectangleThreshold);
        addProperty(_stopAnimationThreshold);
        addProperty(_animationSpeed);

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
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        if (module && module->getPair(identifier())) {
            module->removeTargetBrowserPair(identifier());
        }
    }

    // Pure virtual in the screen space renderable class and hence must be defined 
    void ScreenSpaceSkyTarget::bindTexture() {

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
        _borderColor = color;
    }

    glm::ivec3 ScreenSpaceSkyTarget::borderColor() const {
        return _borderColor;
    }

    void ScreenSpaceSkyTarget::render() {

        bool showCrosshair = _verticalFov < _showCrosshairThreshold;
        bool showRectangle = _verticalFov > _showRectangleThreshold;
        
        glm::vec4 color = { glm::vec3(_borderColor) / 255.f, _opacity.value() };
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

    void ScreenSpaceSkyTarget::update()
    {
        if (_isLocked) {
            glm::dvec3 localCamera = skybrowser::equatorialToLocalCamera(
                _lockedCoordinates
            );

            if (_useRadiusAzimuthElevation) {
                // Keep the set radius
                glm::vec3 position = _raePosition.value().x * glm::vec3(localCamera);
                _raePosition = cartesianToRae(position);
            }
            else {
                _cartesianPosition = skybrowser::localCameraToScreenSpace3d(
                    localCamera
                );
            }
           
        }
    }

    void ScreenSpaceSkyTarget::setDimensions(glm::vec2 dimensions) {
        // To avoid flooring of the size of the target, multiply by factor of 100
        // Object size is really the pixel size so this calculation is not exact
        _objectSize = glm::ivec2(dimensions * 100.f);
    }

    glm::dvec3 ScreenSpaceSkyTarget::directionGalactic() const {

        glm::vec3 localCamera = _cartesianPosition.value();

        if (_useRadiusAzimuthElevation) {

            localCamera = raeToCartesian(_raePosition.value());
        }

        glm::dmat4 rotation = glm::inverse(
            global::navigationHandler->camera()->viewRotationMatrix()
        );
        glm::dvec4 position = glm::dvec4(localCamera, 1.0);

        return glm::normalize(rotation * position);
    }

    // Update the scale of the target (the height of the target in relation to the 
    // OpenSpace window)
    void ScreenSpaceSkyTarget::setScaleFromVfov(float verticalFov) {
        _verticalFov = verticalFov;
        glm::dvec2 fovs = skybrowser::fovWindow();
        
        // Cap the scale at small scales so it is still visible
        float heightRatio = verticalFov / fovs.y;
        float smallestHeightRatio = _showRectangleThreshold.value() / fovs.y;
        
        _scale = std::max(heightRatio, smallestHeightRatio);
    }

    void ScreenSpaceSkyTarget::setFovFromScale()
    {
        glm::dvec2 fovs = skybrowser::fovWindow();
        _verticalFov = _scale * fovs.y;
    }

    bool ScreenSpaceSkyTarget::isLocked() const {
        return _isLocked;
    }

    bool ScreenSpaceSkyTarget::isAnimated()
    {
        return _isAnimated;
    }

    void ScreenSpaceSkyTarget::startAnimation(glm::dvec3 equatorialCoordsEnd, 
                                              bool shouldLockAfter)
    {
        _animationStart = glm::normalize(
            skybrowser::sphericalToCartesian(equatorialAim())
        );
        _animationEnd = glm::normalize(equatorialCoordsEnd);
        _shouldLockAfterAnimation = shouldLockAfter;
        _isAnimated = true;
        _isLocked = false;
    }

    void ScreenSpaceSkyTarget::incrementallyAnimateToCoordinate(float deltaTime)
    {
        // At fps that are too low, the animation stops working. Just place target instead
        bool fpsTooLow = deltaTime > DeltaTimeThreshold;
        // Find smallest angle between the two vectors
        double smallestAngle = skybrowser::angleBetweenVectors(_animationStart, 
            _animationEnd);
        bool hasArrived = smallestAngle < _stopAnimationThreshold;

        // Only keep animating when target is not at goal position
        if (!hasArrived && !fpsTooLow) {
            glm::dmat4 rotMat = skybrowser::incrementalAnimationMatrix(
                _animationStart,
                _animationEnd,
                deltaTime,
                _animationSpeed
            );

            // Rotate target direction
            glm::dvec3 equatorial = glm::dvec3(rotMat * glm::dvec4(_animationStart, 1.0));
            glm::dvec3 localCamera = skybrowser::equatorialToLocalCamera(equatorial);
            // Convert to screen space
            if (_useRadiusAzimuthElevation) {
                // Keep the radius
                glm::vec3 position = _raePosition.value().x * glm::vec3(localCamera);
                _raePosition = cartesianToRae(position);
            }
            else {
                _cartesianPosition = skybrowser::localCameraToScreenSpace3d(localCamera);
            }
            
            // Update position
            glm::dvec3 cartesian = skybrowser::sphericalToCartesian(equatorialAim());
            _animationStart = glm::normalize(cartesian);
        }
        else {
            // Set the exact target position 
            glm::dvec3 localCamera = skybrowser::equatorialToLocalCamera(_animationEnd);
          
            if (_useRadiusAzimuthElevation) {
                glm::vec3 position = _raePosition.value().x * glm::vec3(localCamera);
                _raePosition = cartesianToRae(position);
            }
            else {
                _cartesianPosition = skybrowser::localCameraToScreenSpace3d(localCamera);
            }
            
            _isAnimated = false;
            // Lock target when it first arrives to the position
            setLock(_shouldLockAfterAnimation);
        }      
    }

    glm::dvec2 ScreenSpaceSkyTarget::equatorialAim() const {

        // Get the local camera coordinates of the target
        if (_useRadiusAzimuthElevation) {
            glm::vec3 cartesian = raeToCartesian(_raePosition.value());
            glm::dvec3 equatorial = skybrowser::localCameraToEquatorial(cartesian);
            return skybrowser::cartesianToSpherical(equatorial);
            
        }
        else {
            glm::dvec3 cartesian = skybrowser::localCameraToEquatorial(_cartesianPosition.value());
            return skybrowser::cartesianToSpherical(cartesian);
        }
    }


    void ScreenSpaceSkyTarget::highlight(glm::ivec3 addition)
    {
        _borderColor += addition;
    }

    void ScreenSpaceSkyTarget::removeHighlight(glm::ivec3 removal)
    {
        _borderColor -= removal;
    }

    float ScreenSpaceSkyTarget::opacity() const {
        return _opacity.value();
    }

    glm::dvec2 ScreenSpaceSkyTarget::lockedCoordinates() const
    {
        return skybrowser::cartesianToSpherical(_lockedCoordinates);
    }

    void ScreenSpaceSkyTarget::setOpacity(float opacity) {
        _opacity = opacity;
    }
    void ScreenSpaceSkyTarget::setLock(bool isLocked)
    {
        _isLocked = isLocked;
        if (_isLocked) {
            _lockedCoordinates = skybrowser::sphericalToCartesian(equatorialAim());
        }
    }
    void ScreenSpaceSkyTarget::setEquatorialAim(const glm::dvec2& aim)
    {
        _isAnimated = false;
        _isLocked = false;

        glm::dvec3 cartesianAim = skybrowser::sphericalToCartesian(aim);
        glm::dvec3 localCamera = skybrowser::equatorialToLocalCamera(cartesianAim);
        if (_useRadiusAzimuthElevation) {
            // Keep the set radius
            glm::vec3 position = _raePosition.value().x * glm::vec3(localCamera);
            _raePosition = cartesianToRae(position);
        }
        else {
            _cartesianPosition = skybrowser::localCameraToScreenSpace3d(localCamera);
        }
    }
}
