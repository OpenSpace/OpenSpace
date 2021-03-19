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
#include <openspace/engine/windowdelegate.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <optional>
#include <ghoul/opengl/openglstatecache.h>
#include <glm/gtx/string_cast.hpp>

#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/windowdelegate.h>
#include <glm/gtx/matrix_decompose.hpp>


namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyTarget";

    constexpr const openspace::properties::Property::PropertyInfo TargetDimensionInfo =
    {
        "TargetDimensions",
        "Target Dimensions Info",
        "Set the dimensions of the SkyTarget according to the SkyBrowser ratio "
    };

    constexpr const std::array<const char*, 5> UniformNames = {
        "ModelTransform", "ViewProjectionMatrix", "texture1", "borderWidth", "targetRatio"
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyTarget)]] Parameters {

        // [[codegen::verbatim(TargetDimensionInfo.description)]]
        std::optional<glm::vec2> targetDimensions;
    };

#include "screenspaceskytarget_codegen.cpp"

} //namespace

namespace openspace {
    ScreenSpaceSkyTarget::ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , _targetDimensions(TargetDimensionInfo, glm::ivec2(1000.f), glm::ivec2(0.f), glm::ivec2(6000.f))
    {
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _targetDimensions = p.targetDimensions.value_or(_targetDimensions);
        
        addProperty(_targetDimensions);


        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyTarget";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

       

        /*
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
        */
        _cartesianPosition.setValue(glm::vec3(_cartesianPosition.value().x, _cartesianPosition.value().y, -2.1f));

    }

    void ScreenSpaceSkyTarget::bindTexture() {
        if (_texture) {
            _texture->bind();
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

    void ScreenSpaceSkyTarget::render() {

        glDisable(GL_CULL_FACE);
       
        glm::mat4 modelTransform = globalRotationMatrix() * translationMatrix() * localRotationMatrix() * scaleMatrix();
        float borderWidth = 0.005f / _scale.value();
        glm::vec2 targetRatio;

        _targetDimensions.value() == glm::vec2(0) ? targetRatio = glm::vec2(1) : targetRatio = _targetDimensions.value() / _targetDimensions.value().y;
        _shader->activate();

        _shader->setUniform(_uniformCache.borderWidth, borderWidth);
        _shader->setUniform(_uniformCache.targetRatio, targetRatio);
        _shader->setUniform(_uniformCache.modelTransform, modelTransform);

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

    void ScreenSpaceSkyTarget::update() {


    }

    void ScreenSpaceSkyTarget::translate(glm::vec2 translation, glm::vec2 position) {
        _cartesianPosition = glm::translate(glm::mat4(1.f), glm::vec3(translation, 0.0f)) * glm::vec4(position, _cartesianPosition.value().z, 1.0f);
    }

    glm::vec2 ScreenSpaceSkyTarget::getAnglePosition() {
        glm::vec3 pos = _cartesianPosition.value();
        return glm::vec2(atan(pos.x / pos.z), atan(pos.y / pos.z));
    }

    glm::vec2  ScreenSpaceSkyTarget::getScreenSpacePosition() {
        return glm::vec2(_cartesianPosition.value().x, _cartesianPosition.value().y);
    }

    glm::vec2  ScreenSpaceSkyTarget::getScreenSpaceDimensions() {
        return glm::vec2(2.f * _scale * static_cast<float>(_objectSize.x) / static_cast<float>(_objectSize.y), 2.f * _scale);
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

    void ScreenSpaceSkyTarget::setScreenSpaceTargetDimension(glm::vec2 currentBrowserDimension) {
        _targetDimensions = currentBrowserDimension;    // TA IN SOM EN PROP!
    }
    
  

}
