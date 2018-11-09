/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/base/rendering/renderablemodel.h>

#include <modules/base/basemodule.h>
#include <modules/base/rendering/modelgeometry.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/lightsource.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* ProgramName = "ModelProgram";
    constexpr const char* KeyGeometry = "Geometry";

    constexpr const std::array<const char*, 11> UniformNames = {
        "opacity", "nLightSources", "lightDirectionsViewSpace", "lightIntensities",
        "modelViewTransform", "projectionTransform", "performShading", "texture1",
        "ambientIntensity", "diffuseIntensity", "specularIntensity"
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "ColorTexture",
        "Color Texture",
        "This value points to a color texture file that is applied to the geometry "
        "rendered in this object."
    };

    constexpr openspace::properties::Property::PropertyInfo AmbientIntensityInfo = {
        "AmbientIntensity",
        "Ambient Intensity",
        "A multiplier for ambient lighting."
    };

    constexpr openspace::properties::Property::PropertyInfo DiffuseIntensityInfo = {
        "DiffuseIntensity",
        "Diffuse Intensity",
        "A multiplier for diffuse lighting."
    };

    constexpr openspace::properties::Property::PropertyInfo SpecularIntensityInfo = {
        "SpecularIntensity",
        "Specular Intensity",
        "A multiplier for specular lighting."
    };

    constexpr openspace::properties::Property::PropertyInfo ShadingInfo = {
        "PerformShading",
        "Perform Shading",
        "This value determines whether this model should be shaded by using the position "
        "of the Sun."
    };

    constexpr openspace::properties::Property::PropertyInfo ModelTransformInfo = {
        "ModelTransform",
        "Model Transform",
        "This value specifies the model transform that is applied to the model before "
        "all other transformations are applied."
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourcesInfo = {
        "LightSources",
        "Light Sources",
        "A list of light sources that this model should accept light from."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableModel::Documentation() {
    using namespace documentation;
    return {
        "RenderableModel",
        "base_renderable_model",
        {
            {
                KeyGeometry,
                new ReferencingVerifier("base_geometry_model"),
                Optional::No,
                "This specifies the model that is rendered by the Renderable."
            },
            {
                TextureInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                TextureInfo.description
            },
            {
                AmbientIntensityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                AmbientIntensityInfo.description
            },
            {
                DiffuseIntensityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                DiffuseIntensityInfo.description
            },
            {
                SpecularIntensityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                SpecularIntensityInfo.description
            },
            {
                ShadingInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                ShadingInfo.description
            },
            {
                ModelTransformInfo.identifier,
                new DoubleMatrix3Verifier,
                Optional::Yes,
                ModelTransformInfo.description
            },
            {
                LightSourcesInfo.identifier,
                new TableVerifier({
                    {
                        "*",
                        new ReferencingVerifier("core_light_source"),
                        Optional::Yes
                    }
                }),
                Optional::Yes,
                LightSourcesInfo.description
            }
        }
    };
}

RenderableModel::RenderableModel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath(TextureInfo)
    , _ambientIntensity(AmbientIntensityInfo, 0.2f, 0.f, 1.f)
    , _diffuseIntensity(DiffuseIntensityInfo, 1.f, 0.f, 1.f)
    , _specularIntensity(SpecularIntensityInfo, 1.f, 0.f, 1.f)
    , _performShading(ShadingInfo, true)
    , _modelTransform(ModelTransformInfo, glm::mat3(1.f))
    , _lightSourcePropertyOwner({ "LightSources", "Light Sources" })
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableModel"
    );

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();


    if (dictionary.hasKey(KeyGeometry)) {
        ghoul::Dictionary dict = dictionary.value<ghoul::Dictionary>(KeyGeometry);
        _geometry = modelgeometry::ModelGeometry::createFromDictionary(dict);
    }

    if (dictionary.hasKey(TextureInfo.identifier)) {
        _colorTexturePath = absPath(dictionary.value<std::string>(
            TextureInfo.identifier
        ));
    }

    if (dictionary.hasKey(ModelTransformInfo.identifier)) {
        _modelTransform = dictionary.value<glm::dmat3>(ModelTransformInfo.identifier);
    }

    if (dictionary.hasKey(AmbientIntensityInfo.identifier)) {
        _ambientIntensity = dictionary.value<float>(AmbientIntensityInfo.identifier);
    }
    if (dictionary.hasKey(DiffuseIntensityInfo.identifier)) {
        _diffuseIntensity = dictionary.value<float>(DiffuseIntensityInfo.identifier);
    }
    if (dictionary.hasKey(SpecularIntensityInfo.identifier)) {
        _specularIntensity = dictionary.value<float>(SpecularIntensityInfo.identifier);
    }

    if (dictionary.hasKey(ShadingInfo.identifier)) {
        _performShading = dictionary.value<bool>(ShadingInfo.identifier);
    }

    if (dictionary.hasKey(LightSourcesInfo.identifier)) {
        const ghoul::Dictionary& lsDictionary =
            dictionary.value<ghoul::Dictionary>(LightSourcesInfo.identifier);

        for (const std::string& k : lsDictionary.keys()) {
            std::unique_ptr<LightSource> lightSource = LightSource::createFromDictionary(
                lsDictionary.value<ghoul::Dictionary>(k)
            );
            _lightSourcePropertyOwner.addPropertySubOwner(lightSource.get());
            _lightSources.push_back(std::move(lightSource));
        }
    }

    addPropertySubOwner(_lightSourcePropertyOwner);
    addPropertySubOwner(_geometry.get());

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderableModel::loadTexture, this));


    addProperty(_ambientIntensity);
    addProperty(_diffuseIntensity);
    addProperty(_specularIntensity);
    addProperty(_performShading);
}

RenderableModel::~RenderableModel() {}

bool RenderableModel::isReady() const {
    return _program && _texture;
}

void RenderableModel::initialize() {
    for (const std::unique_ptr<LightSource>& ls : _lightSources) {
        ls->initialize();
    }
}

void RenderableModel::initializeGL() {
    _program = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/model_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/model_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    loadTexture();

    _geometry->initialize(this);
}

void RenderableModel::deinitializeGL() {
    if (_geometry) {
        _geometry->deinitialize();
        _geometry = nullptr;
    }
    _texture = nullptr;

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _program = nullptr;
}

void RenderableModel::render(const RenderData& data, RendererTasks&) {
    _program->activate();

    _program->setUniform(_uniformCache.opacity, _opacity);

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(
            glm::dmat4(_modelTransform.value()), glm::dvec3(data.modelTransform.scale)
        );
    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() *
                                          modelTransform;

    int nLightSources = 0;
    _lightIntensitiesBuffer.resize(_lightSources.size());
    _lightDirectionsViewSpaceBuffer.resize(_lightSources.size());
    for (const std::unique_ptr<LightSource>& lightSource : _lightSources) {
        if (!lightSource->isEnabled()) {
            continue;
        }
        _lightIntensitiesBuffer[nLightSources] = lightSource->intensity();
        _lightDirectionsViewSpaceBuffer[nLightSources] =
            lightSource->directionViewSpace(data);

        ++nLightSources;
    }

    _program->setUniform(
        _uniformCache.nLightSources,
        nLightSources
    );
    _program->setUniform(
        _uniformCache.lightIntensities,
        _lightIntensitiesBuffer.data(),
        nLightSources
    );
    _program->setUniform(
        _uniformCache.lightDirectionsViewSpace,
        _lightDirectionsViewSpaceBuffer.data(),
        nLightSources
    );
    _program->setUniform(
        _uniformCache.modelViewTransform,
        glm::mat4(modelViewTransform)
    );
    _program->setUniform(
        _uniformCache.projectionTransform,
        data.camera.projectionMatrix()
    );
    _program->setUniform(
        _uniformCache.ambientIntensity,
        _ambientIntensity
    );
    _program->setUniform(
        _uniformCache.diffuseIntensity,
        _diffuseIntensity
    );
    _program->setUniform(
        _uniformCache.specularIntensity,
        _specularIntensity
    );
    _program->setUniform(
        _uniformCache.performShading,
        _performShading
    );

    _geometry->setUniforms(*_program);

    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _program->setUniform(_uniformCache.texture, unit);

    _geometry->render();

    _program->deactivate();
}

void RenderableModel::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

void RenderableModel::loadTexture() {
    _texture = nullptr;
    if (!_colorTexturePath.value().empty()) {
        _texture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(_colorTexturePath)
        );
        if (_texture) {
            LDEBUGC(
                "RenderableModel",
                fmt::format("Loaded texture from '{}'", absPath(_colorTexturePath))
            );
            _texture->uploadTexture();
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        }
    }
}

}  // namespace openspace
