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
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <openspace/scene/scene.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* ProgramName = "ModelProgram";
    constexpr const char* KeyGeometry = "Geometry";

    static const openspace::properties::Property::PropertyInfo TextureInfo = {
        "ColorTexture",
        "Color Texture",
        "This value points to a color texture file that is applied to the geometry "
        "rendered in this object."
    };

    static const openspace::properties::Property::PropertyInfo ShadingInfo = {
        "PerformShading",
        "Perform Shading",
        "This value determines whether this model should be shaded by using the position "
        "of the Sun."
    };

    static const openspace::properties::Property::PropertyInfo ModelTransformInfo = {
        "ModelTransform",
        "Model Transform",
        "This value specifies the model transform that is applied to the model before "
        "all other transformations are applied."
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
            }
        }
    };
}

RenderableModel::RenderableModel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _geometry(nullptr)
    , _colorTexturePath(TextureInfo)
    , _performShading(ShadingInfo, true)
    , _modelTransform(ModelTransformInfo, glm::mat3(1.0))
    , _programObject(nullptr)
    , _texture(nullptr)
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

    if (dictionary.hasKey(ShadingInfo.identifier)) {
        _performShading = dictionary.value<bool>(ShadingInfo.identifier);
    }

    addPropertySubOwner(_geometry.get());

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderableModel::loadTexture, this));

    addProperty(_performShading);
}

bool RenderableModel::isReady() const {
    return _programObject && _texture;
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

    _uniformCache.opacity = _programObject->uniformLocation("opacity");
    _uniformCache.directionToSunViewSpace = _programObject->uniformLocation(
        "directionToSunViewSpace"
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
    _uniformCache.texture = _programObject->uniformLocation("texture1");


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
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(
            glm::dmat4(_modelTransform.value()), glm::dvec3(data.modelTransform.scale)
        );
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    glm::vec3 directionToSun = glm::normalize(_sunPos - data.modelTransform.translation);
    glm::vec3 directionToSunViewSpace =
        glm::normalize(glm::mat3(data.camera.combinedViewMatrix()) * directionToSun);

    _programObject->setUniform(
        _uniformCache.directionToSunViewSpace,
        directionToSunViewSpace
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

        _uniformCache.opacity = _programObject->uniformLocation("opacity");
        _uniformCache.directionToSunViewSpace = _programObject->uniformLocation(
            "directionToSunViewSpace"
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
        _uniformCache.texture = _programObject->uniformLocation("texture1");
    }

    _sunPos = OsEng.renderEngine().scene()->sceneGraphNode(
        "SolarSystemBarycenter"
    )->worldPosition();
}

void RenderableModel::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
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
