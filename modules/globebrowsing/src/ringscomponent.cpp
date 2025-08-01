/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/globebrowsing/src/ringscomponent.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/io/texture/texturereader.h>
#include <filesystem>
#include <fstream>
#include <cstdlib>
#include <locale>

namespace {
    constexpr std::string_view _loggerCat = "RingsComponent";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Enable/Disable Rings.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value is the path to a texture on disk that contains a one-dimensional "
        "texture which is used for these rings.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextureFwrdInfo = {
        "TextureFwrd",
        "TextureFwrd",
        "This value is the path to a texture on disk that contains a one-dimensional "
        "texture which is used for forward scattering light in these rings.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextureBckwrdInfo = {
        "TextureBckwrd",
        "TextureBckwrd",
        "This value is the path to a texture on disk that contains a one-dimensional "
        "texture which is used for backward scattering light in these rings.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextureUnlitInfo = {
        "TextureUnlit",
        "TextureUnlit",
        "This value is the path to a texture on disk that contains a one-dimensional "
        "texture which is used for unlit part in these rings.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextureColorInfo = {
        "TextureColor",
        "TextureColor",
        "This value is the path to a texture on disk that contains a one-dimensional "
        "texture color which is used for unlit part in these rings.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextureTransparencyInfo = {
        "TextureTransparency",
        "TextureTransparency",
        "This value is the path to a texture on disk that contains a one-dimensional "
        "texture transparency which is used for unlit part in these rings.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "This value specifies the radius of the rings in meters.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OffsetInfo = {
        "Offset",
        "Offset",
        "This value is used to limit the width of the rings. Each of the two values is "
        "a value between 0 and 1, where 0 is the center of the ring and 1 is the "
        "maximum extent at the radius. For example, if the value is {0.5, 1.0}, the "
        "ring is only shown between radius/2 and radius. It defaults to {0.0, 1.0}.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo NightFactorInfo = {
        "NightFactor",
        "Night Factor",
        "This value is a multiplicative factor that is applied to the side of the rings "
        "that is facing away from the Sun. If this value is equal to '1', no darkening "
        "of the night side occurs.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorFilterInfo = {
        "ColorFilter",
        "Color Filter",
        "This value affects the filtering out of part of the rings depending on the "
        "color values of the texture. The higher value, the more rings are filtered out.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ZFightingPercentageInfo = {
        "ZFightingPercentage",
        "Z-Fighting Percentage",
        "The percentage of the correct distance to the surface being shadowed. "
        "Possible values: [0.0, 1.0].",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo NumberShadowSamplesInfo = {
        "NumberShadowSamples",
        "Number of Shadow Samples",
        "The number of samples used during shadow mapping calculation "
        "(Percentage Closer Filtering).",
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(RingsComponent)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // This value determines the overall opacity of the rings
        std::optional<float> opacity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(TextureInfo.description)]]
        std::optional<std::filesystem::path> texture;

        // [[codegen::verbatim(TextureFwrdInfo.description)]]
        std::optional<std::filesystem::path> textureFwrd;

        // [[codegen::verbatim(TextureBckwrdInfo.description)]]
        std::optional<std::filesystem::path> textureBckwrd;

        // [[codegen::verbatim(TextureUnlitInfo.description)]]
        std::optional<std::filesystem::path> textureUnlit;

        // [[codegen::verbatim(TextureColorInfo.description)]]
        std::optional<std::filesystem::path> textureColor;

        // [[codegen::verbatim(TextureTransparencyInfo.description)]]
        std::optional<std::filesystem::path> textureTransparency;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(OffsetInfo.description)]]
        std::optional<glm::vec2> offset;

        // [[codegen::verbatim(NightFactorInfo.description)]]
        std::optional<float> nightFactor;

        // [[codegen::verbatim(ColorFilterInfo.description)]]
        std::optional<float> colorFilter;

        // [[codegen::verbatim(ZFightingPercentageInfo.description)]]
        std::optional<float> zFightingPercentage;

        // [[codegen::verbatim(NumberShadowSamplesInfo.description)]]
        std::optional<int> numberShadowSamples;
    };
#include "ringscomponent_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RingsComponent::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_rings_component");
}

RingsComponent::RingsComponent(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Rings" })
    , _texturePath(TextureInfo)
    , _textureFwrdPath(TextureFwrdInfo)
    , _textureBckwrdPath(TextureBckwrdInfo)
    , _textureUnlitPath(TextureUnlitInfo)
    , _textureColorPath(TextureColorInfo)
    , _textureTransparencyPath(TextureTransparencyInfo)
    , _size(SizeInfo, 1.f, 0.f, 1e25f)
    , _offset(OffsetInfo, glm::vec2(0.f, 1.f), glm::vec2(0.f), glm::vec2(1.f))
    , _nightFactor(NightFactorInfo, 0.33f, 0.f, 1.f)
    , _colorFilter(ColorFilterInfo, 0.15f, 0.f, 1.f)
    , _enabled(EnabledInfo, true)
    , _zFightingPercentage(ZFightingPercentageInfo, 0.95f, 0.000001f, 1.f)
    , _nShadowSamples(NumberShadowSamplesInfo, 2, 1, 7)
    // @TODO (abock, 2019-12-16) It would be better to not store the dictionary long
    // term and rather extract the values directly here.  This would require a bit of
    // a rewrite in the RenderableGlobe class to not create the RingsComponent in the
    // class-initializer list though
    // @TODO (abock, 2021-03-25) Righto!  The RenderableGlobe passes this dictionary
    // in as-is so it would be easy to just pass it directly to the initialize method
    // instead
    // @TODO (abock, 2025-02-16) Why haven't you done it yet?!
    , _ringsDictionary(dictionary)
{}

void RingsComponent::initialize() {
    ZoneScoped;

    using ghoul::filesystem::File;

    const Parameters p = codegen::bake<Parameters>(_ringsDictionary);

    _enabled = p.enabled.value_or(_enabled);
    addProperty(_enabled);
    _opacity = p.opacity.value_or(_opacity);
    addProperty(Fadeable::_opacity);
    addProperty(Fadeable::_fade);

    _size.setExponent(15.f);
    _size = p.size.value_or(_size);
    _size.onChange([this]() { _planeIsDirty = true; });
    addProperty(_size);

    if (p.texture.has_value()) {
        _texturePath = absPath(*p.texture).string();
        _textureFile = std::make_unique<File>(_texturePath.value());
        _texturePath.onChange([this]() { loadTexture(); });
        addProperty(_texturePath);
        _textureFile->setCallback([this]() { _textureIsDirty = true; });
    }

    if (p.textureFwrd.has_value()) {
        _textureFwrdPath = absPath(*p.textureFwrd).string();
        _textureFileForwards = std::make_unique<File>(_textureFwrdPath.value());
        _textureFwrdPath.onChange([this]() { loadTexture(); });
        addProperty(_textureFwrdPath);
        _textureFileForwards->setCallback([this]() { _textureIsDirty = true; });
    }

    if (p.textureBckwrd.has_value()) {
        _textureBckwrdPath = absPath(*p.textureBckwrd).string();
        _textureFileBackwards = std::make_unique<File>(_textureBckwrdPath.value());
        _textureBckwrdPath.onChange([this]() { loadTexture(); });
        addProperty(_textureBckwrdPath);
        _textureFileBackwards->setCallback([this]() { _textureIsDirty = true; });
    }

    if (p.textureUnlit.has_value()) {
        _textureUnlitPath = absPath(*p.textureUnlit).string();
        _textureFileUnlit = std::make_unique<File>(_textureUnlitPath.value());
        _textureUnlitPath.onChange([this]() { loadTexture(); });
        addProperty(_textureUnlitPath);
        _textureFileUnlit->setCallback([this]() { _textureIsDirty = true; });
    }

    if (p.textureColor.has_value()) {
        _textureColorPath = absPath(*p.textureColor).string();
        _textureFileColor = std::make_unique<File>(_textureColorPath.value());
        _textureColorPath.onChange([this]() { loadTexture(); });
        addProperty(_textureColorPath);
        _textureFileColor->setCallback([this]() { _textureIsDirty = true; });
    }

    if (p.textureTransparency.has_value()) {
        _textureTransparencyPath = absPath(*p.textureTransparency).string();
        _textureFileTransparency = std::make_unique<File>(
            _textureTransparencyPath.value()
        );
        _textureTransparencyPath.onChange([this]() { loadTexture(); });
        addProperty(_textureTransparencyPath);
        _textureFileTransparency->setCallback([this]() { _textureIsDirty = true; });
    }

    _offset = p.offset.value_or(_offset);
    _offset.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_offset);

    _nightFactor = p.nightFactor.value_or(_nightFactor);
    addProperty(_nightFactor);

    _colorFilter = p.colorFilter.value_or(_colorFilter);
    addProperty(_colorFilter);

    // Shadow Mapping Quality Controls
    _zFightingPercentage = p.zFightingPercentage.value_or(_zFightingPercentage);
    addProperty(_zFightingPercentage);

    _nShadowSamples = p.numberShadowSamples.value_or(_nShadowSamples);
    _nShadowSamples.onChange([this]() { compileShadowShader(); });
    addProperty(_nShadowSamples);
}

bool RingsComponent::isReady() const {
    return (_shader || _geometryOnlyShader) && _texture;
}

void RingsComponent::initializeGL() {
    ZoneScoped;

    loadTexture();
    compileShadowShader();

    try {
        //global::renderEngine.removeRenderProgram(_geometryOnlyShader.get());
        _geometryOnlyShader = global::renderEngine->buildRenderProgram(
            "RingsGeomOnlyProgram",
            absPath("${MODULE_GLOBEBROWSING}/shaders/rings_geom_vs.glsl"),
            absPath("${MODULE_GLOBEBROWSING}/shaders/rings_geom_fs.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_geometryOnlyShader, _geomUniformCache);
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(e.message);
    }

    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);

    createPlane();
    
    // Check if readiness state has changed after shader compilation
    checkAndNotifyReadinessChange();
}

void RingsComponent::deinitializeGL() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _textureFile = nullptr;
    _texture = nullptr;
    _textureFileForwards = nullptr;
    _textureForwards = nullptr;
    _textureFileBackwards = nullptr;
    _textureBackwards = nullptr;
    _textureFileUnlit = nullptr;
    _textureUnlit = nullptr;


    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;

    global::renderEngine->removeRenderProgram(_geometryOnlyShader.get());
    _geometryOnlyShader = nullptr;
}

void RingsComponent::draw(const RenderData& data,
                          const ShadowComponent::ShadowMapData& shadowData)
{
    _shader->activate();

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewProjectionTransform =
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
        * modelTransform;

    ghoul::opengl::TextureUnit ringTextureUnit;
    ghoul::opengl::TextureUnit ringTextureFwrdUnit;
    ghoul::opengl::TextureUnit ringTextureBckwrdUnit;
    ghoul::opengl::TextureUnit ringTextureUnlitUnit;
    ghoul::opengl::TextureUnit ringTextureColorUnit;
    ghoul::opengl::TextureUnit ringTextureTransparencyUnit;
    if (_isAdvancedTextureEnabled) {
        _shader->setUniform(
            _uniformCacheAdvancedRings.modelViewProjectionMatrix,
            modelViewProjectionTransform
        );
        _shader->setUniform(_uniformCacheAdvancedRings.textureOffset, _offset);
        _shader->setUniform(_uniformCacheAdvancedRings.colorFilterValue, _colorFilter);
        _shader->setUniform(_uniformCacheAdvancedRings.nightFactor, _nightFactor);
        _shader->setUniform(_uniformCacheAdvancedRings.sunPosition, _sunPosition);

        const glm::dmat4 inverseModelTransform = glm::inverse(modelTransform);

        const glm::vec3 sunPositionObjectSpace = glm::normalize(
            glm::vec3(inverseModelTransform * glm::vec4(_sunPosition, 0.f))
        );

        _shader->setUniform(
            _uniformCacheAdvancedRings.sunPositionObj,
            sunPositionObjectSpace
        );
        _shader->setUniform(
            _uniformCacheAdvancedRings.zFightingPercentage,
            _zFightingPercentage
        );
        _shader->setUniform(
            _uniformCacheAdvancedRings.modelViewProjectionMatrix,
            modelViewProjectionTransform
        );

        ringTextureFwrdUnit.activate();
        _textureForwards->bind();
        _shader->setUniform(
            _uniformCacheAdvancedRings.textureForwards,
            ringTextureFwrdUnit
        );

        ringTextureBckwrdUnit.activate();
        _textureBackwards->bind();
        _shader->setUniform(
            _uniformCacheAdvancedRings.textureBackwards,
            ringTextureBckwrdUnit
        );

        ringTextureUnlitUnit.activate();
        _textureUnlit->bind();
        _shader->setUniform(
            _uniformCacheAdvancedRings.textureUnlit,
            ringTextureUnlitUnit
        );

        ringTextureColorUnit.activate();
        _textureColor->bind();
        _shader->setUniform(
            _uniformCacheAdvancedRings.textureColor,
            ringTextureColorUnit
        );

        ringTextureTransparencyUnit.activate();
        _textureTransparency->bind();
        _shader->setUniform(
            _uniformCacheAdvancedRings.textureTransparency,
            ringTextureTransparencyUnit
        );
        _shader->setUniform(_uniformCacheAdvancedRings.opacity, opacity());

        // Adding the model transformation to the final shadow matrix so we have a
        // complete transformation from the model coordinates to the clip space of
        // the light position.
        _shader->setUniform(
            _uniformCacheAdvancedRings.shadowMatrix,
            shadowData.shadowMatrix * modelTransform
        );

        const glm::dmat4 camToObjectTransform = glm::inverse(
            data.camera.combinedViewMatrix() * modelTransform
        );

        _camPositionObjectSpace = glm::normalize(
            glm::vec3(camToObjectTransform * glm::dvec4(0.0, 0.0, 0.0, 1.0))
        );

        _shader->setUniform(
            _uniformCacheAdvancedRings.camPositionObj,
            _camPositionObjectSpace
        );

        ghoul::opengl::TextureUnit shadowMapUnit;
        shadowMapUnit.activate();
        glBindTexture(GL_TEXTURE_2D, shadowData.shadowDepthTexture);
        _shader->setUniform(
            _uniformCacheAdvancedRings.shadowMapTexture,
            shadowMapUnit
        );

        glEnable(GL_DEPTH_TEST);
        glEnablei(GL_BLEND, 0);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }
    else {
        _shader->setUniform(
            _uniformCache.modelViewProjectionMatrix,
            modelViewProjectionTransform
        );
        _shader->setUniform(_uniformCache.textureOffset, _offset);
        _shader->setUniform(_uniformCache.colorFilterValue, _colorFilter);
        _shader->setUniform(_uniformCache.nightFactor, _nightFactor);
        _shader->setUniform(_uniformCache.sunPosition, _sunPosition);
        _shader->setUniform(_uniformCache.zFightingPercentage, _zFightingPercentage);
        _shader->setUniform(
            _uniformCache.modelViewProjectionMatrix,
            modelViewProjectionTransform
        );
        _shader->setUniform(_uniformCache.opacity, opacity());

        ringTextureUnit.activate();
        _texture->bind();
        _shader->setUniform(_uniformCache.ringTexture, ringTextureUnit);

        // Adding the model transformation to the final shadow matrix so we have a
        // complete transformation from the model coordinates to the clip space of
        // the light position.
        _shader->setUniform(
            _uniformCache.shadowMatrix,
            shadowData.shadowMatrix * modelTransform
        );

        ghoul::opengl::TextureUnit shadowMapUnit;
        shadowMapUnit.activate();
        glBindTexture(GL_TEXTURE_2D, shadowData.shadowDepthTexture);
        _shader->setUniform(_uniformCache.shadowMapTexture, shadowMapUnit);
    }

    glEnable(GL_DEPTH_TEST);
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glEnable(GL_CULL_FACE);

    _shader->deactivate();
    global::renderEngine->openglStateCache().resetBlendState();
}

void RingsComponent::update(const UpdateData& data) {
    ZoneScoped;

    if (_shader && _shader->isDirty()) [[unlikely]] {
        compileShadowShader();
    }

    if (_geometryOnlyShader->isDirty()) [[unlikely]] {
        _geometryOnlyShader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_geometryOnlyShader, _geomUniformCache);
    }

    if (_planeIsDirty) [[unlikely]] {
        createPlane();
        _planeIsDirty = false;
    }

    if (_textureIsDirty) [[unlikely]] {
        loadTexture();
        _textureIsDirty = false;
    }

    // @TODO (abock, 2022-02-20) This should be replaced with the more general light
    // source solution that we are using in other places
    SceneGraphNode* sun = global::renderEngine->scene()->sceneGraphNode("Sun");
    if (sun) {
        _sunPosition = glm::normalize(
            sun->worldPosition() - data.modelTransform.translation
        );
    }
    else {
        // If the Sun node is not found, we assume the light source to be in the origin
        _sunPosition = glm::normalize(-data.modelTransform.translation);
    }
}

void RingsComponent::loadTexture() {
    using namespace ghoul::io;
    using namespace ghoul::opengl;

    if (!_texturePath.value().empty()) {
        std::unique_ptr<Texture> texture = TextureReader::ref().loadTexture(
            absPath(_texturePath),
            1
        );

        if (texture) {
            LDEBUG(std::format("Loaded texture from '{}'", absPath(_texturePath)));
            _texture = std::move(texture);

            _texture->uploadTexture();
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

            _textureFile = std::make_unique<ghoul::filesystem::File>(
                _texturePath.value()
            );
            _textureFile->setCallback([this]() { _textureIsDirty = true; });
        }
    }

    if (!_textureFwrdPath.value().empty()) {
        std::unique_ptr<Texture> textureForwards = TextureReader::ref().loadTexture(
            absPath(_textureFwrdPath),
            1
        );

        if (textureForwards) {
            LDEBUG(std::format(
                "Loaded forwards scattering texture from '{}'",
                absPath(_textureFwrdPath)
            ));
            _textureForwards = std::move(textureForwards);

            _textureForwards->uploadTexture();
            _textureForwards->setFilter(Texture::FilterMode::AnisotropicMipMap);

            _textureFileForwards = std::make_unique<ghoul::filesystem::File>(
                _textureFwrdPath.value()
            );
            _textureFileForwards->setCallback([this]() { _textureIsDirty = true; });
        }
    }

    if (!_textureBckwrdPath.value().empty()) {
        std::unique_ptr<Texture> textureBackwards = TextureReader::ref().loadTexture(
            absPath(_textureBckwrdPath),
            1
        );

        if (textureBackwards) {
            LDEBUG(std::format(
                "Loaded backwards scattering texture from '{}'",
                absPath(_textureBckwrdPath)
            ));
            _textureBackwards = std::move(textureBackwards);

            _textureBackwards->uploadTexture();
            _textureBackwards->setFilter(Texture::FilterMode::AnisotropicMipMap);

            _textureFileBackwards = std::make_unique<ghoul::filesystem::File>(
                _textureBckwrdPath.value()
            );
            _textureFileBackwards->setCallback([this]() { _textureIsDirty = true; });
        }
    }

    if (!_textureUnlitPath.value().empty()) {
        std::unique_ptr<Texture> textureUnlit = TextureReader::ref().loadTexture(
            absPath(_textureUnlitPath),
            1
        );

        if (textureUnlit) {
            LDEBUG(std::format(
                "Loaded unlit texture from '{}'", absPath(_textureUnlitPath)
            ));
            _textureUnlit = std::move(textureUnlit);

            _textureUnlit->uploadTexture();
            _textureUnlit->setFilter(Texture::FilterMode::AnisotropicMipMap);

            _textureFileUnlit = std::make_unique<ghoul::filesystem::File>(
                _textureUnlitPath.value()
            );
            _textureFileUnlit->setCallback([this]() { _textureIsDirty = true; });
        }
    }

    if (!_textureColorPath.value().empty()) {
        std::unique_ptr<Texture> textureColor = TextureReader::ref().loadTexture(
            absPath(_textureColorPath),
            1
        );

        if (textureColor) {
            LDEBUG(
                std::format("Loaded color texture from '{}'", absPath(_textureColorPath))
            );
            _textureColor = std::move(textureColor);

            _textureColor->uploadTexture();
            _textureColor->setFilter(Texture::FilterMode::AnisotropicMipMap);

            _textureFileColor = std::make_unique<ghoul::filesystem::File>(
                _textureColorPath.value()
            );
            _textureFileColor->setCallback([this]() { _textureIsDirty = true; });
        }
    }

    if (!_textureTransparencyPath.value().empty()) {
        std::unique_ptr<Texture> textureTransparency = TextureReader::ref().loadTexture(
            absPath(_textureTransparencyPath),
            1
        );

        if (textureTransparency) {
            LDEBUG(std::format(
                "Loaded transparency texture from '{}'", absPath(_textureTransparencyPath)
            ));
            _textureTransparency = std::move(textureTransparency);
            
            _textureTransparency->uploadTexture();
            _textureTransparency->setFilter(Texture::FilterMode::AnisotropicMipMap);

            _textureFileTransparency = std::make_unique<ghoul::filesystem::File>(
                _textureTransparencyPath.value()
            );
            _textureFileTransparency->setCallback([this]() { _textureIsDirty = true; });
        }
    }

    _isAdvancedTextureEnabled = _textureForwards && _textureBackwards && _textureUnlit;
    
    // Check if readiness state has changed after loading textures
    checkAndNotifyReadinessChange();
}

void RingsComponent::createPlane() {
    const GLfloat size = _size;

    struct VertexData {
        GLfloat x;
        GLfloat y;
        GLfloat s;
        GLfloat t;
        GLfloat nx;
        GLfloat ny;
        GLfloat nz;
    };

    const std::array<VertexData, 6> vertices = {
        VertexData{ -size, -size, 0.f, 0.f, 0.f, 0.f, 1.f },
        VertexData{  size,  size, 1.f, 1.f, 0.f, 0.f, 1.f },
        VertexData{ -size,  size, 0.f, 1.f, 0.f, 0.f, 1.f },
        VertexData{ -size, -size, 0.f, 0.f, 0.f, 0.f, 1.f },
        VertexData{  size, -size, 1.f, 0.f, 0.f, 0.f, 1.f },
        VertexData{  size,  size, 1.f, 1.f, 0.f, 0.f, 1.f },
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VertexData),
        nullptr
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VertexData),
        reinterpret_cast<void*>(offsetof(VertexData, s))
    );
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(
        2,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VertexData),
        reinterpret_cast<void*>(offsetof(VertexData, nx))
    );
}

void RingsComponent::compileShadowShader() {
    ghoul::Dictionary dict;
    dict.setValue("nShadowSamples", std::to_string(_nShadowSamples - 1));

    try {
        global::renderEngine->removeRenderProgram(_shader.get());

        // Uses multiple textures for the Rings
        // See https://bjj.mmedia.is/data/s_rings/index.html for theory behind it
        if (_isAdvancedTextureEnabled) {
            _shader = global::renderEngine->buildRenderProgram(
                "AdvancedRingsProgram",
                absPath("${MODULE_GLOBEBROWSING}/shaders/advanced_rings_vs.glsl"),
                absPath("${MODULE_GLOBEBROWSING}/shaders/advanced_rings_fs.glsl"),
                dict
            );

            ghoul::opengl::updateUniformLocations(*_shader, _uniformCacheAdvancedRings);
        }
        else {
            // Uses simple texture for the Rings
            _shader = global::renderEngine->buildRenderProgram(
                "RingsProgram",
                absPath("${MODULE_GLOBEBROWSING}/shaders/rings_vs.glsl"),
                absPath("${MODULE_GLOBEBROWSING}/shaders/rings_fs.glsl"),
                dict
            );

            ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
        }
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(e.message);
    }
    
    // Check if readiness state has changed after shader compilation
    checkAndNotifyReadinessChange();
}

bool RingsComponent::isEnabled() const {
    return _enabled;
}

double RingsComponent::size() const {
    return _size;
}

ghoul::opengl::Texture* RingsComponent::textureForwards() const {
    return _textureForwards.get();
}

ghoul::opengl::Texture* RingsComponent::textureBackwards() const {
    return _textureBackwards.get();
}

ghoul::opengl::Texture* RingsComponent::textureUnlit() const {
    return _textureUnlit.get();
}

ghoul::opengl::Texture* RingsComponent::textureColor() const {
    return _textureColor.get();
}

ghoul::opengl::Texture* RingsComponent::textureTransparency() const {
    return _textureTransparency.get();
}

glm::vec2 RingsComponent::textureOffset() const {
    return _offset;
}

glm::vec3 RingsComponent::sunPositionObj() const {
    return _sunPosition;
}

glm::vec3 RingsComponent::camPositionObj() const {
    return _camPositionObjectSpace;
}

void RingsComponent::onReadinessChange(ReadinessChangeCallback callback) {
    _readinessChangeCallback = std::move(callback);
}

void RingsComponent::checkAndNotifyReadinessChange() {
    const bool currentlyReady = isReady();
    if (currentlyReady != _wasReady) {
        _wasReady = currentlyReady;
        if (_readinessChangeCallback) {
            _readinessChangeCallback();
        }
    }
}

} // namespace openspace
