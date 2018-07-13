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
#include <openspace/engine/openspaceengine.h>
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
    , _ambientIntensity(AmbientIntensityInfo, 0.2, 0.0, 1.0)
    , _diffuseIntensity(DiffuseIntensityInfo, 1.0, 0.0, 1.0)
    , _specularIntensity(SpecularIntensityInfo, 1.0, 0.0, 1.0)
    , _performShading(ShadingInfo, true)
    , _modelTransform(ModelTransformInfo, glm::mat3(1.0))
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
    return _programObject && _texture;
}

void RenderableModel::initialize() {
    for (const std::unique_ptr<LightSource>& ls : _lightSources) {
        ls->initialize();
    }
}

void RenderableModel::initializeGL() {
    _programObject = BaseModule::ProgramObjectManager.requestProgramObject(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return OsEng.renderEngine().buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/model_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/model_fs.glsl")
            );
        }
    );
    updateUniformCache();

    loadTexture();

    _geometry->initialize(this);
}

void RenderableModel::deinitializeGL() {
    if (_geometry) {
        _geometry->deinitialize();
        _geometry = nullptr;
    }
    _texture = nullptr;

    BaseModule::ProgramObjectManager.releaseProgramObject(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            OsEng.renderEngine().removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

void RenderableModel::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();

    _programObject->setUniform(_uniformCache.opacity, _opacity);

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

    _programObject->setUniform(
        _uniformCache.nLightSources,
        nLightSources
    );
    _programObject->setUniform(
        _uniformCache.lightIntensities,
        _lightIntensitiesBuffer.data(),
        nLightSources
    );
    _programObject->setUniform(
        _uniformCache.lightDirectionsViewSpace,
        _lightDirectionsViewSpaceBuffer.data(),
        nLightSources
    );
    _programObject->setUniform(
        _uniformCache.modelViewTransform,
        glm::mat4(modelViewTransform)
    );
    _programObject->setUniform(
        _uniformCache.projectionTransform,
        data.camera.projectionMatrix()
    );
    _programObject->setUniform(
        _uniformCache.ambientIntensity,
        _ambientIntensity
    );
    _programObject->setUniform(
        _uniformCache.diffuseIntensity,
        _diffuseIntensity
    );
    _programObject->setUniform(
        _uniformCache.specularIntensity,
        _specularIntensity
    );
    _programObject->setUniform(
        _uniformCache.performShading,
        _performShading
    );

    _geometry->setUniforms(*_programObject);

    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform(_uniformCache.texture, unit);

    _geometry->render();

    _programObject->deactivate();
}

void RenderableModel::update(const UpdateData&) {
    if (_programObject->isDirty()) {
        _programObject->rebuildFromFile();
        updateUniformCache();
    }
}


void RenderableModel::updateUniformCache() {
    _uniformCache.opacity = _programObject->uniformLocation("opacity");
    _uniformCache.nLightSources = _programObject->uniformLocation(
        "nLightSources"
    );
    _uniformCache.lightDirectionsViewSpace = _programObject->uniformLocation(
        "lightDirectionsViewSpace"
    );
    _uniformCache.lightIntensities = _programObject->uniformLocation(
        "lightIntensities"
    );
    _uniformCache.modelViewTransform = _programObject->uniformLocation(
        "modelViewTransform"
    );
    _uniformCache.projectionTransform = _programObject->uniformLocation(
        "projectionTransform"
    );
    _uniformCache.performShading = _programObject->uniformLocation(
        "performShading"
    );
    _uniformCache.ambientIntensity = _programObject->uniformLocation(
        "ambientIntensity"
    );
    _uniformCache.diffuseIntensity = _programObject->uniformLocation(
        "diffuseIntensity"
    );
    _uniformCache.specularIntensity = _programObject->uniformLocation(
        "specularIntensity"
    );
    _uniformCache.texture = _programObject->uniformLocation("texture1");
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
