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
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <glm/gtx/string_cast.hpp>
#include <glm/gtx/matrix_decompose.hpp>
#include <glm/gtx/vector_angle.hpp>
#include <optional>
#define _USE_MATH_DEFINES
#include <cmath>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyTarget";

    constexpr const openspace::properties::Property::PropertyInfo TargetDimensionInfo =
    {
        "TargetDimensions",
        "Target Dimensions Info",
        "Set the dimensions of the SkyTarget according to the SkyBrowser ratio "
    };

    constexpr const std::array<const char*, 7> UniformNames = {
        "ModelTransform", "ViewProjectionMatrix", "texture1", "showCrosshair", "borderWidth", "targetDimensions", "borderColor"
    };

    constexpr const openspace::properties::Property::PropertyInfo BrowserIDInfo =
    {
        "BrowserID",
        "Browser Info",
        "tjobidabidobidabidopp plupp"
    };

    constexpr const openspace::properties::Property::PropertyInfo CrosshairThresholdInfo =
    {
        "CrosshairThreshold",
        "Crosshair Threshold Info",
        "tjobidabidobidabidopp plupp"
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyTarget)]] Parameters {

        // [[codegen::verbatim(TargetDimensionInfo.description)]]
        std::optional<glm::vec2> targetDimensions;

        // [[codegen::verbatim(BrowserIDInfo.description)]]
        std::optional<std::string> browserID;

        // [[codegen::verbatim(CrosshairThresholdInfo.description)]]
        std::optional<float> crosshairThreshold;
    };

#include "screenspaceskytarget_codegen.cpp"

} //namespace

namespace openspace {
    ScreenSpaceSkyTarget::ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , _targetDimensions(TargetDimensionInfo, glm::ivec2(1000.f), glm::ivec2(0.f), glm::ivec2(6000.f))
        , _skyBrowserID(BrowserIDInfo)
        , _showCrosshairThreshold(CrosshairThresholdInfo, 2.f, 1.f, 70.f)
        , _borderColor(220, 220, 220)
        , _skyBrowser(nullptr)
    {
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _targetDimensions = p.targetDimensions.value_or(_targetDimensions);
        _skyBrowserID = p.browserID.value_or(_skyBrowserID);
        _showCrosshairThreshold = p.crosshairThreshold.value_or(_showCrosshairThreshold);
        
        addProperty(_targetDimensions);  
        addProperty(_skyBrowserID);
        addProperty(_showCrosshairThreshold);

        _skyBrowserID.onChange([&]() {
            setConnectedBrowser();
            });


        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyTarget";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

        _cartesianPosition.setValue(glm::dvec3(_cartesianPosition.value().x, _cartesianPosition.value().y, skybrowser::SCREENSPACE_Z));
        //_useRadiusAzimuthElevation.setValue(true);

        // Always make sure that the target and browser are visible together
        _enabled.onChange([&]() {
            if (_skyBrowser) {
                _skyBrowser->property("Enabled")->set(_enabled.value());
            }
            });

    }

    ScreenSpaceSkyTarget::~ScreenSpaceSkyTarget() {
        if (_lockTargetThread.joinable()) {
            _lockTargetThread.join();
        }
    }

    void ScreenSpaceSkyTarget::bindTexture() {
        if (_texture) {
            _texture->bind();
        }
    }

    bool ScreenSpaceSkyTarget::setConnectedBrowser() {
        _skyBrowser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable(_skyBrowserID.value()));
        return _skyBrowser;
    }

    void ScreenSpaceSkyTarget::initializeWithBrowser() {
        if (_skyBrowser) {
            _borderColor = _skyBrowser->getColor();
            updateFOV(_skyBrowser->_vfieldOfView.value());
            _targetDimensions = _skyBrowser->getBrowserPixelDimensions();
        }
    }

    bool ScreenSpaceSkyTarget::isReady() const {
        return _shader != nullptr;
    }

    bool ScreenSpaceSkyTarget::initializeGL() {

        glGenVertexArrays(1, &_vertexArray);
        glGenBuffers(1, &_vertexBuffer);

        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath("${MODULE_SKYBROWSER}/square.png"));
   

        if (texture) {
            // Images don't need to start on 4-byte boundaries, for example if the
            // image is only RGB
            glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

            texture->uploadTexture();
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            texture->purgeFromRAM();

            _texture = std::move(texture);
            _objectSize = _texture->dimensions();
        }

        createShaders();

        return isReady();
    }

    glm::mat4 ScreenSpaceSkyTarget::scaleMatrix() {
        // To ensure the plane has the right ratio
        // The _scale us how much of the windows height the
        // browser covers: eg a browser that covers 0.25 of the 
        // height of the window will have scale = 0.25
        float textureRatio =
            static_cast<float>(_targetDimensions.value().x) / static_cast<float>(_targetDimensions.value().y);

        glm::mat4 scale = glm::scale(
            glm::mat4(1.f),
            glm::vec3(textureRatio * _scale, _scale, 1.f)
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

    void ScreenSpaceSkyTarget::setBorderColor(glm::ivec3 color) {
        _borderColor = color;
    }

    glm::ivec3 ScreenSpaceSkyTarget::getColor() {
        return _borderColor;
    }

    void ScreenSpaceSkyTarget::setBrowser(ScreenSpaceSkyBrowser* browser) {
        _skyBrowser = browser;
    }

    ScreenSpaceSkyBrowser* ScreenSpaceSkyTarget::getSkyBrowser() {
        return _skyBrowser;
    }

    void ScreenSpaceSkyTarget::render() {

        glDisable(GL_CULL_FACE);
       
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() * localRotationMatrix() * scaleMatrix();
        float borderWidth = 0.002f/_scale.value();
        glm::vec2 targetDim;
        bool showCrosshair;
        _targetDimensions.value() == glm::vec2(0) ? targetDim = glm::vec2(1) : targetDim = _targetDimensions.value();
        _shader->activate();

        _fieldOfView < _showCrosshairThreshold ? showCrosshair = true : showCrosshair = false;

        _shader->setUniform(_uniformCache.showCrosshair, showCrosshair);
        _shader->setUniform(_uniformCache.borderWidth, borderWidth);
        _shader->setUniform(_uniformCache.targetDimensions, targetDim);
        _shader->setUniform(_uniformCache.modelTransform, modelTransform);
        glm::vec4 color = { glm::vec3(_borderColor) / 255.f, _opacity.value() };
        _shader->setUniform(_uniformCache.borderColor, color);

        _shader->setUniform(
            _uniformCache.viewProj,
            global::renderEngine->scene()->camera()->viewProjectionMatrix()
        );

        ghoul::opengl::TextureUnit unit;
        unit.activate();
        bindTexture();
        _shader->setUniform(_uniformCache.texture, unit);


        glBindVertexArray(rendering::helper::vertexObjects.square.vao);
        glDrawArrays(GL_TRIANGLES, 0, 6);

        glEnable(GL_CULL_FACE);

        _shader->deactivate();
        unbindTexture();
    }

    glm::dvec2  ScreenSpaceSkyTarget::getScreenSpacePosition() {
        return glm::vec2(_cartesianPosition.value().x, _cartesianPosition.value().y);
    }

    glm::dvec2  ScreenSpaceSkyTarget::getScreenSpaceDimensions() {
        return glm::dvec2(2.f * _scale * static_cast<float>(_objectSize.x) / static_cast<float>(_objectSize.y), 2.f * _scale);
    }

    glm::dvec2 ScreenSpaceSkyTarget::getUpperRightCornerScreenSpace() {

        return getScreenSpacePosition() + (getScreenSpaceDimensions() / 2.0);
    }

    glm::dvec2 ScreenSpaceSkyTarget::getLowerLeftCornerScreenSpace() {
        return getScreenSpacePosition() - (getScreenSpaceDimensions() / 2.0);
    }

    bool ScreenSpaceSkyTarget::coordIsInsideCornersScreenSpace(glm::dvec2 coord) {
        bool lessThanUpperRight = coord.x < getUpperRightCornerScreenSpace().x && coord.y < getUpperRightCornerScreenSpace().y;
        bool moreThanLowerLeft = coord.x > getLowerLeftCornerScreenSpace().x && coord.y > getLowerLeftCornerScreenSpace().y;
        return  lessThanUpperRight && moreThanLowerLeft;
    }

    void ScreenSpaceSkyTarget::setDimensions(glm::vec2 currentBrowserDimensions) {
        _targetDimensions = currentBrowserDimensions;
    }

    glm::dvec3 ScreenSpaceSkyTarget::getTargetDirectionGalactic() {
        // Get camera view direction and orthogonal coordinate system of camera view direction
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        glm::dvec3 targetPosWorldSpace = glm::inverse(global::navigationHandler->camera()->combinedViewMatrix()) * glm::dvec4(_cartesianPosition.value(), 1.0);
        glm::dvec3 targetDirection = glm::normalize(targetPosWorldSpace - camPos);

        return targetDirection;
    }

    void ScreenSpaceSkyTarget::updateFOV(float VFOV) {
        float horizFOV = global::windowDelegate->getHorizFieldOfView();
        glm::ivec2 windowRatio = global::windowDelegate->currentWindowSize();

        float verticFOV = horizFOV * (static_cast<float>(windowRatio.y) / static_cast<float>(windowRatio.x));
        _scale = std::max((VFOV / verticFOV),(_showCrosshairThreshold.value()/ verticFOV));
        
        _fieldOfView = VFOV;
    }

    void ScreenSpaceSkyTarget::unlock() {
        isLocked = false;
        if (_lockTargetThread.joinable()) {
            _lockTargetThread.join();
        }
    }

    void ScreenSpaceSkyTarget::lock() {
        if (isLocked) {
            unlock();
        }
        isLocked = true;
        lockedCelestialCoords = getTargetDirectionCelestial();

        // Start a thread to enable user interactions while locking target
        _lockTargetThread = std::thread([&] {
            while (isLocked) {
                glm::vec3 imageCoordsScreenSpace = skybrowser::J2000SphericalToScreenSpace(lockedCelestialCoords);
                _cartesianPosition = imageCoordsScreenSpace;
            }
        });
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
        if (isAnimated) {
            // Find smallest angle between the two vectors
            double smallestAngle = std::acos(glm::dot(_coordsStartAnimation, _coordsToAnimateTo) / (glm::length(_coordsStartAnimation) * glm::length(_coordsToAnimateTo)));
            // Only keep animating when target is not at final position
            if (abs(smallestAngle) > 0.0005) {
                // Calculate rotation this frame
                double rotationAngle = smallestAngle * deltaTime * 5.0;
                // Create the rotation matrix
                glm::dvec3 rotationAxis = glm::normalize(glm::cross(_coordsStartAnimation, _coordsToAnimateTo));
                glm::dmat4 rotmat = glm::rotate(rotationAngle, rotationAxis);
                // Rotate target direction
                glm::dvec3 newDir = rotmat * glm::dvec4(_coordsStartAnimation, 1.0);
                // Convert to screenspace
                _cartesianPosition = skybrowser::J2000CartesianToScreenSpace(newDir);
                // Update position
                _coordsStartAnimation = glm::normalize(newDir);
            }
            else {
                // Set the exact target position and lock target when it first arrives
                // to the position
                if (!isLocked) {
                    _cartesianPosition = skybrowser::J2000CartesianToScreenSpace(_coordsToAnimateTo);
                    lock();
                }
                // When target is in position, animate the FOV until it has finished
                if(animateFOV(FOVToAnimateTo, deltaTime)) {
                    isAnimated = false;
                }
            }
        }
    }

    bool ScreenSpaceSkyTarget::animateFOV(float endFOV, float deltaTime) {
        if (!_skyBrowser) {
            ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable(_skyBrowserID.value()));
            setBrowser(browser);
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

    void ScreenSpaceSkyTarget::startAnimation(glm::dvec2 coordsEnd, float FOVEnd) {
        // Save the Cartesian celestial coordinates for animation
        // to make sure wrap around works
        _coordsToAnimateTo = glm::normalize(skybrowser::sphericalToCartesian(coordsEnd));
        _coordsStartAnimation = glm::normalize(skybrowser::sphericalToCartesian(getTargetDirectionCelestial()));
        FOVToAnimateTo = FOVEnd;
        isAnimated = true;
    }
    properties::FloatProperty& ScreenSpaceSkyTarget::getOpacity() {
        return _opacity;
    }
}
