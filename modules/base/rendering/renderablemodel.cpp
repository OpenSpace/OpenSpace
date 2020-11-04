/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* ProgramName = "ModelProgram";
    constexpr const char* KeyGeometry = "Geometry";

    constexpr const int DefaultBlending = 0;
    constexpr const int AdditiveBlending = 1;
    constexpr const int PointsAndLinesBlending = 2;
    constexpr const int PolygonBlending = 3;
    constexpr const int ColorAddingBlending = 4;

    std::map<std::string, int> BlendingMapping = {
        { "Default", DefaultBlending },
        { "Additive", AdditiveBlending },
        { "Points and Lines", PointsAndLinesBlending },
        { "Polygon", PolygonBlending },
        { "Color Adding", ColorAddingBlending }
    };

    constexpr const std::array<const char*, 13> UniformNames = {
        "opacity", "nLightSources", "lightDirectionsViewSpace", "lightIntensities",
        "modelViewTransform", "crippedModelViewTransform", "projectionTransform", 
        "performShading", "texture1", "ambientIntensity", "diffuseIntensity", 
        "specularIntensity", "opacityBlending"
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

    constexpr openspace::properties::Property::PropertyInfo DisableFaceCullingInfo = {
        "DisableFaceCulling",
        "Disable Face Culling",
        "Disable OpenGL automatic face culling optimization."
    };

    constexpr openspace::properties::Property::PropertyInfo ModelTransformInfo = {
        "ModelTransform",
        "Model Transform",
        "This value specifies the model transform that is applied to the model before "
        "all other transformations are applied."
    };

    constexpr openspace::properties::Property::PropertyInfo RotationVecInfo = {
        "RotationVector",
        "Rotation Vector",
        "Rotation Vector using degrees"
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourcesInfo = {
        "LightSources",
        "Light Sources",
        "A list of light sources that this model should accept light from."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableDepthTestInfo = {
        "DisableDepthTest",
        "Disable Depth Test",
        "Disable Depth Testing for the Model."
    };

    constexpr openspace::properties::Property::PropertyInfo BlendingOptionInfo = {
        "BledingOption",
        "Blending Options",
        "Debug option for blending colors."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableOpacityBlendingInfo = {
        "EnableOpacityBlending",
        "Enable Opacity Blending",
        "Enable Opacity Blending."
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
                new TableVerifier({
                    {
                        "*",
                        new ReferencingVerifier("base_geometry_model"),
                        Optional::Yes
                    }
                }),
                Optional::No,
                "This specifies the model that is rendered by the Renderable."
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
                DisableFaceCullingInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DisableFaceCullingInfo.description
            },
            {
                ModelTransformInfo.identifier,
                new DoubleMatrix3Verifier,
                Optional::Yes,
                ModelTransformInfo.description
            },
           {
                RotationVecInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                RotationVecInfo.description
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
            },
            {
                DisableDepthTestInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DisableDepthTestInfo.description
            },
            {
                BlendingOptionInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                BlendingOptionInfo.description
            },
            {
                EnableOpacityBlendingInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                EnableOpacityBlendingInfo.description
            },
        }
    };
}

RenderableModel::RenderableModel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _ambientIntensity(AmbientIntensityInfo, 0.2f, 0.f, 1.f)
    , _diffuseIntensity(DiffuseIntensityInfo, 1.f, 0.f, 1.f)
    , _specularIntensity(SpecularIntensityInfo, 1.f, 0.f, 1.f)
    , _performShading(ShadingInfo, true)
    , _disableFaceCulling(DisableFaceCullingInfo, false)
    , _modelTransform(
        ModelTransformInfo,
        glm::dmat3(1.0),
        glm::dmat3(-1.0),
        glm::dmat3(1.0)
    )
    , _rotationVec(RotationVecInfo, glm::dvec3(0.0), glm::dvec3(0.0), glm::dvec3(360.0))
    , _enableOpacityBlending(EnableOpacityBlendingInfo, false)
    , _disableDepthTest(DisableDepthTestInfo, false)
    , _blendingFuncOption(
        BlendingOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
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
        for (int i = 1; i <= dict.size(); ++i) {
            std::string key = std::to_string(i);
            ghoul::Dictionary geom = dict.value<ghoul::Dictionary>(key);
            _geometry.push_back(modelgeometry::ModelGeometry::createFromDictionary(geom));
        }
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

    if (dictionary.hasKey(DisableDepthTestInfo.identifier)) {
        _disableDepthTest = dictionary.value<bool>(DisableDepthTestInfo.identifier);
    }

    if (dictionary.hasKey(DisableFaceCullingInfo.identifier)) {
        _disableFaceCulling = dictionary.value<bool>(DisableFaceCullingInfo.identifier);
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
    addProperty(_ambientIntensity);
    addProperty(_diffuseIntensity);
    addProperty(_specularIntensity);
    addProperty(_performShading);
    addProperty(_disableFaceCulling);
    addProperty(_disableDepthTest);
    addProperty(_modelTransform);
    addProperty(_rotationVec);

    _rotationVec.onChange([this]() {
        _modelTransform = glm::mat4_cast(glm::quat(glm::radians(_rotationVec.value())));
    });


    if (dictionary.hasKey(RotationVecInfo.identifier)) {
        _rotationVec = dictionary.value<glm::vec3>(RotationVecInfo.identifier);
    }

    _blendingFuncOption.addOption(DefaultBlending, "Default");
    _blendingFuncOption.addOption(AdditiveBlending, "Additive");
    _blendingFuncOption.addOption(PointsAndLinesBlending, "Points and Lines");
    _blendingFuncOption.addOption(PolygonBlending, "Polygon");
    _blendingFuncOption.addOption(ColorAddingBlending, "Color Adding");

    addProperty(_blendingFuncOption);

    if (dictionary.hasKey(BlendingOptionInfo.identifier)) {
        const std::string blendingOpt = dictionary.value<std::string>(
            BlendingOptionInfo.identifier
        );
        _blendingFuncOption.set(BlendingMapping[blendingOpt]);
    }

    if (dictionary.hasKey(DisableDepthTestInfo.identifier)) {
        _enableOpacityBlending = dictionary.value<bool>(
            EnableOpacityBlendingInfo.identifier
        );
    }

    addProperty(_enableOpacityBlending);
}

bool RenderableModel::isReady() const {
    return _program;
}

void RenderableModel::initialize() {
    ZoneScoped

    for (const std::unique_ptr<LightSource>& ls : _lightSources) {
        ls->initialize();
    }
}

void RenderableModel::initializeGL() {
    ZoneScoped

    _program = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/model_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/model_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    for (const ghoul::mm_unique_ptr<modelgeometry::ModelGeometry>& geom : _geometry) {
        geom->initialize(this);
    }
}

void RenderableModel::deinitializeGL() {
    for (const ghoul::mm_unique_ptr<modelgeometry::ModelGeometry>& geom : _geometry) {
        geom->deinitialize();
    }
    _geometry.clear();

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
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

    glm::dmat4 crippedModelViewTransform = glm::transpose(glm::inverse(
        glm::dmat4(glm::inverse(data.camera.sgctInternal.viewMatrix())) * modelViewTransform
    ));

    _program->setUniform(
        _uniformCache.crippedModelViewTransform,
        glm::mat4(crippedModelViewTransform)
    );

    _program->setUniform(
        _uniformCache.projectionTransform,
        data.camera.projectionMatrix()
    );
    _program->setUniform(_uniformCache.ambientIntensity, _ambientIntensity);
    _program->setUniform(_uniformCache.diffuseIntensity, _diffuseIntensity);
    _program->setUniform(_uniformCache.specularIntensity, _specularIntensity);
    _program->setUniform(_uniformCache.performShading, _performShading);
    _program->setUniform(_uniformCache.opacityBlending, _enableOpacityBlending);

    if (_disableFaceCulling) {
        glDisable(GL_CULL_FACE);
    }

    glEnablei(GL_BLEND, 0);
    switch (_blendingFuncOption) {
        case DefaultBlending:
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            break;
        case AdditiveBlending:
            glBlendFunc(GL_ONE, GL_ONE);
            break;
        case PointsAndLinesBlending:
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            break;
        case PolygonBlending:
            glBlendFunc(GL_SRC_ALPHA_SATURATE, GL_ONE);
            break;
        case ColorAddingBlending:
            glBlendFunc(GL_SRC_COLOR, GL_DST_COLOR);
            break;
    };

    if (_disableDepthTest) {
        glDisable(GL_DEPTH_TEST);
    }

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _program->setUniform(_uniformCache.texture, unit);
    for (const ghoul::mm_unique_ptr<modelgeometry::ModelGeometry>& geom : _geometry) {
        geom->setUniforms(*_program);
        geom->bindTexture();
        geom->render();
    }
    if (_disableFaceCulling) {
        glEnable(GL_CULL_FACE);
    }

    global::renderEngine->openglStateCache().resetBlendState();

    if (_disableDepthTest) {
        glEnable(GL_DEPTH_TEST);
    }

    _program->deactivate();
}

void RenderableModel::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

}  // namespace openspace
