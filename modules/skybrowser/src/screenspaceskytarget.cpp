#include <modules/skybrowser/include/screenspaceskytarget.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/factorymanager.h>
#include <openspace/rendering/helper.h>
#include <ghoul/opengl/textureunit.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/scripting/scriptengine.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/filesystem/filesystem.h>
#include <optional>
#include <ghoul/opengl/openglstatecache.h>
#include <glm/gtx/string_cast.hpp>

#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/windowdelegate.h>
#include <glm/gtx/matrix_decompose.hpp>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyTarget";

    constexpr const std::array<const char*, 4> UniformNames = {
    "Alpha", "ModelTransform", "ViewProjectionMatrix", "texture1"
    };

} //namespace

namespace openspace {
    ScreenSpaceSkyTarget::ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
    {
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyTarget";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

        std::unique_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(absPath("D:/Ylvas/OpenSpace/modules/skybrowser/target.png"));

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
    }

    void ScreenSpaceSkyTarget::bindTexture() {
        if (_texture) {
            _texture->bind();
        }
    }

    void ScreenSpaceSkyTarget::translate(glm::vec2 translation, glm::vec2 position) {

        glm::vec2 windowRatio = global::windowDelegate->currentWindowSize();
        windowRatio /= windowRatio.y;

        _cartesianPosition = glm::translate(glm::mat4(1.f), glm::vec3(translation * windowRatio, 0.0f)) * glm::vec4(position * windowRatio, _cartesianPosition.value().z, 1.0f);
    }
    
    glm::vec2 ScreenSpaceSkyTarget::getScreenSpacePosition() {
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() *
            localRotationMatrix() * scaleMatrix();
        glm::mat4 viewProj = global::renderEngine->scene()->camera()->viewProjectionMatrix();
        glm::mat4 screenSpaceTransform = viewProj * modelTransform;

        glm::vec3 scale;
        glm::quat rotation;
        glm::vec3 translation;
        glm::vec3 skew;
        glm::vec4 perspective;
        glm::decompose(screenSpaceTransform, scale, rotation, translation, skew, perspective);

        return translation;
    }
    
    glm::vec2  ScreenSpaceSkyTarget::getScreenSpaceDimensions() {
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() *
            localRotationMatrix() * scaleMatrix();
        glm::mat4 viewProj = global::renderEngine->scene()->camera()->viewProjectionMatrix();
        glm::mat4 screenSpaceTransform = viewProj * modelTransform;


        glm::vec3 scale;
        glm::quat rotation;
        glm::vec3 translation;
        glm::vec3 skew;
        glm::vec4 perspective;
        glm::decompose(screenSpaceTransform, scale, rotation, translation, skew, perspective);

        // Scale is negative and relative to the whole screen
        // Changing to positive and View Coordinates [-1,1] 
        // E.g. a full screen screenspacebrowser will have [2,2]
        scale = -2.0f * scale;

        return scale;
    }

    glm::vec2 ScreenSpaceSkyTarget::getUpperRightCornerScreenSpace() {

        return getScreenSpacePosition() + (getScreenSpaceDimensions() / 2.0f);
    }

    glm::vec2 ScreenSpaceSkyTarget::getLowerLeftCornerScreenSpace() {
        return getScreenSpacePosition() - (getScreenSpaceDimensions() / 2.0f);
    }

    bool ScreenSpaceSkyTarget::coordIsInsideCornersScreenSpace(glm::vec2 coord) {
        bool lessThanUpperRight = coord.x < getUpperRightCornerScreenSpace().x && coord.y < getUpperRightCornerScreenSpace().y;
        bool moreThanLowerLeft = coord.x > getLowerLeftCornerScreenSpace().x && coord.y > getLowerLeftCornerScreenSpace().y;
        return  lessThanUpperRight && moreThanLowerLeft;
    }
}
