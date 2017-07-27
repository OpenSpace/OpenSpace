/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/base/rendering/modelgeometry.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/configurationmanager.h>
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
    const char* KeyGeometry = "Geometry";
    const char* KeyTexture = "Textures.Color";
    const char* KeyModelTransform = "Rotation.ModelTransform";

    static const openspace::properties::Property::PropertyInfo TextureInfo = {
        "ColorTexture", // @TODO replace with only "Texture"
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
                "This specifies the model that is rendered by the Renderable.",
                Optional::No
            },
            {
                KeyTexture, // @TODO replace with TextureInfo.identifier
                new StringVerifier,
                TextureInfo.description,
                Optional::Yes
            },
            {
                ShadingInfo.identifier,
                new BoolVerifier,
                ShadingInfo.description,
                Optional::Yes
            },
            {
                KeyModelTransform,
                new DoubleMatrix3Verifier,
                "Specifies a distinct transformation matrix that is applied to the "
                "model. If it is not specified, it is equal to the Identity matrix.",
                Optional::Yes
            }
        }
    };
}

RenderableModel::RenderableModel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _geometry(nullptr)
    , _colorTexturePath(TextureInfo)
    , _performShading(ShadingInfo, true)
    , _programObject(nullptr)
    , _texture(nullptr)
    , _modelTransform(1.0)
{
    ghoul_precondition(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        "Name was not passed to RenderableModel"
    );

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableModel"
    );

    if (dictionary.hasKey(KeyGeometry)) {
        std::string name = dictionary.value<std::string>(SceneGraphNode::KeyName);
        ghoul::Dictionary dict = dictionary.value<ghoul::Dictionary>(KeyGeometry);
        dict.setValue(SceneGraphNode::KeyName, name);
        _geometry = modelgeometry::ModelGeometry::createFromDictionary(dict);
    }

    if (dictionary.hasKey(KeyTexture)) {
        _colorTexturePath = absPath(dictionary.value<std::string>(KeyTexture));
    }

    if (dictionary.hasKey(KeyModelTransform)) {
        _modelTransform = dictionary.value<glm::dmat3>(KeyModelTransform);
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

bool RenderableModel::initialize() {
    _programObject = OsEng.renderEngine().buildRenderProgram(
        "ModelProgram",
        "${MODULE_BASE}/shaders/model_vs.glsl",
        "${MODULE_BASE}/shaders/model_fs.glsl"
    );

    loadTexture();

    bool completeSuccess = true;
    completeSuccess &= (_texture != nullptr);
    completeSuccess &= _geometry->initialize(this); 
    return completeSuccess;
}

bool RenderableModel::deinitialize() {
    if (_geometry) {
        _geometry->deinitialize();
        _geometry = nullptr;
    }
    _texture = nullptr;

    if (_programObject) {
        OsEng.renderEngine().removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    return true;
}

void RenderableModel::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();
    
    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(_modelTransform), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    glm::vec3 directionToSun = glm::normalize(_sunPos - data.modelTransform.translation);
    glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;

    _programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
    _programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
    _programObject->setUniform("performShading", _performShading);
    _programObject->setUniform("fading", 1.f); // @TODO remove this

    _geometry->setUniforms(*_programObject);

    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", unit);

    _geometry->render();

    _programObject->deactivate();
}

void RenderableModel::update(const UpdateData& data) {
    if (_programObject->isDirty()) {
        _programObject->rebuildFromFile();
    }

    _sunPos = OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldPosition();
}

void RenderableModel::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            LDEBUGC(
                "RenderableModel",
                "Loaded texture from '" << absPath(_colorTexturePath) << "'"
            );
            _texture->uploadTexture();
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        }
    }
}

}  // namespace openspace
