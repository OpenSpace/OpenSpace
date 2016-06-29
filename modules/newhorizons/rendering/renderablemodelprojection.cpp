/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/newhorizons/rendering/renderablemodelprojection.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const std::string _loggerCat = "RenderableModelProjection";
    const std::string keySource = "Rotation.Source";
    const std::string keyDestination = "Rotation.Destination";
    const std::string keyBody = "Body";
    const std::string keyGeometry = "Geometry";

    const std::string keyTextureColor = "Textures.Color";
    const std::string keyTextureProject = "Textures.Project";
    const std::string keyTextureDefault = "Textures.Default";
}

namespace openspace {

RenderableModelProjection::RenderableModelProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _rotation("rotation", "Rotation", glm::vec3(0.f), glm::vec3(0.f), glm::vec3(360.f))
    , _programObject(nullptr)
    , _fboProgramObject(nullptr)
    , _baseTexture(nullptr)
    , _geometry(nullptr)
    , _performShading("performShading", "Perform Shading", true)
{
    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(success, "Name was not passed to RenderableModelProjection");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(keyGeometry, geometryDictionary);
    if (success) {
        using modelgeometry::ModelGeometry;
        geometryDictionary.setValue(SceneGraphNode::KeyName, name);
        _geometry = std::unique_ptr<ModelGeometry>(
            ModelGeometry::createFromDictionary(geometryDictionary)
        );
    }

    std::string texturePath = "";
    success = dictionary.getValue(keyTextureColor, texturePath);
    if (success)
        _colorTexturePath = absPath(texturePath);
        
    success = dictionary.getValue(keyTextureDefault, texturePath);
    if (success)
        _defaultProjImage = absPath(texturePath);

    addPropertySubOwner(_geometry.get());
    addPropertySubOwner(_projectionComponent);

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderableModelProjection::loadTextures, this));

    dictionary.getValue(keySource, _source);
    dictionary.getValue(keyDestination, _destination);
    dictionary.getValue(keyBody, _target);
    if (_target != "")
        setBody(_target);

    bool completeSuccess = true;
    completeSuccess &= _projectionComponent.initializeProjectionSettings(dictionary);
    
    openspace::SpiceManager::ref().addFrame(_target, _source);
    setBoundingSphere(pss(1.f, 9.f));

    Renderable::addProperty(_performShading);
    Renderable::addProperty(_rotation);

    success = _projectionComponent.initializeParser(dictionary);
    ghoul_assert(success, "");
}

bool RenderableModelProjection::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_baseTexture != nullptr);
    ready &= (_projectionComponent.isReady());
    return ready;
}

bool RenderableModelProjection::initialize() {
    bool completeSuccess = true;
        
    RenderEngine& renderEngine = OsEng.renderEngine();
    _programObject = renderEngine.buildRenderProgram("ModelShader",
        "${MODULE_NEWHORIZONS}/shaders/renderableModel_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/renderableModel_fs.glsl");


    _fboProgramObject = ghoul::opengl::ProgramObject::Build("ProjectionPass",
        "${MODULE_NEWHORIZONS}/shaders/renderableModelProjection_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/renderableModelProjection_fs.glsl");
    _fboProgramObject->setIgnoreUniformLocationError(
        ghoul::opengl::ProgramObject::IgnoreError::Yes
    );

    completeSuccess &= loadTextures();

    completeSuccess &= _projectionComponent.initialize();

    completeSuccess &= _geometry->initialize(this);
    completeSuccess &= !_source.empty();
    completeSuccess &= !_destination.empty();

    return completeSuccess;
}

bool RenderableModelProjection::deinitialize() {
    if (_geometry)
        _geometry->deinitialize();

    _geometry = nullptr;
    _baseTexture = nullptr;

    _projectionComponent.deinitialize();

    OsEng.renderEngine().removeRenderProgram(_programObject);
    _programObject = nullptr;

    return true;
}

ghoul::opengl::Texture& RenderableModelProjection::baseTexture() const {
    return _projectionComponent.projectionTexture();
}

void RenderableModelProjection::render(const RenderData& data) {
    if (_projectionComponent.needsClearProjection())
        _projectionComponent.clearAllProjections();

    _camScaling = data.camera.scaling();
    _up = data.camera.lookUpVectorCameraSpace();

    if (_capture && _projectionComponent.doesPerformProjection())
        project();

    _programObject->activate();

    attitudeParameters(_time);
    _imageTimes.clear();
        
    _programObject->setUniform("_performShading", _performShading);
    _programObject->setUniform("sun_pos", _sunPosition.vec3());
    _programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _programObject->setUniform("ModelTransform", _transform);
    _programObject->setUniform("_projectionFading", _projectionComponent.projectionFading());
    setPscUniforms(*_programObject, data.camera, data.position);

    _geometry->setUniforms(*_programObject);
    
    ghoul::opengl::TextureUnit unit[2];
    unit[0].activate();
    _baseTexture->bind();
    _programObject->setUniform("baseTexture", unit[0]);

    unit[1].activate();
    _projectionComponent.projectionTexture().bind();
    _programObject->setUniform("projectionTexture", unit[1]);

    _geometry->render();
        
    _programObject->deactivate();
}

void RenderableModelProjection::update(const UpdateData& data) {
    if (_programObject->isDirty())
        _programObject->rebuildFromFile();

    if (_fboProgramObject->isDirty())
        _fboProgramObject->rebuildFromFile();
        
    _time = data.time;

    if (openspace::ImageSequencer::ref().isReady() && _projectionComponent.doesPerformProjection()) {
        openspace::ImageSequencer::ref().updateSequencer(_time);
        _capture = openspace::ImageSequencer::ref().getImagePaths(
            _imageTimes,
            _projectionComponent.projecteeId(),
            _projectionComponent.instrumentId()
        );
    }
        
    // set spice-orientation in accordance to timestamp
    if (!_source.empty()) {
        _stateMatrix = SpiceManager::ref().positionTransformMatrix(
            _source, _destination, _time
        );
    }

    double lt;
    glm::dvec3 p =
        openspace::SpiceManager::ref().targetPosition(
            "SUN", _target, "GALACTIC", {}, _time, lt
    );
    _sunPosition = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
}

void RenderableModelProjection::imageProjectGPU(
                                std::shared_ptr<ghoul::opengl::Texture> projectionTexture)
{
    _projectionComponent.imageProjectBegin();

    _fboProgramObject->activate();

    ghoul::opengl::TextureUnit unitFbo;
    unitFbo.activate();
    projectionTexture->bind();
    _fboProgramObject->setUniform("projectionTexture", unitFbo);

    _fboProgramObject->setUniform("ProjectorMatrix", _projectorMatrix);
    _fboProgramObject->setUniform("ModelTransform", _transform);
    _fboProgramObject->setUniform("_scaling", _camScaling);
    _fboProgramObject->setUniform("boresight", _boresight);

    _geometry->setUniforms(*_fboProgramObject);
    _geometry->render();

    _fboProgramObject->deactivate();

    _projectionComponent.imageProjectEnd();
}

void RenderableModelProjection::attitudeParameters(double time) {
    try {
        _stateMatrix = SpiceManager::ref().positionTransformMatrix(_source, _destination, time);
        _instrumentMatrix = SpiceManager::ref().positionTransformMatrix(_projectionComponent.instrumentId(), _destination, time);
    }
    catch (const SpiceManager::SpiceException& e) {
        return;
    }

    _transform = glm::mat4(1);
    glm::mat4 rotPropX = glm::rotate(
        _transform,
        glm::radians(static_cast<float>(_rotation.value().x)),
        glm::vec3(1, 0, 0)
    );
    glm::mat4 rotPropY = glm::rotate(
        _transform,
        glm::radians(static_cast<float>(_rotation.value().y)),
        glm::vec3(0, 1, 0)
    );
    glm::mat4 rotPropZ = glm::rotate(
        _transform, 
        glm::radians(static_cast<float>(_rotation.value().z)),
        glm::vec3(0, 0, 1)
    );
        
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            _transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }
    _transform = _transform * rotPropX * rotPropY * rotPropZ;

    glm::dvec3 boresight;
    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_projectionComponent.instrumentId());
        boresight = std::move(res.boresightVector);
    } catch (const SpiceManager::SpiceException& e) {
        return;
    }

    double lightTime;
    glm::dvec3 p =
        SpiceManager::ref().targetPosition(
            _projectionComponent.projectorId(),
            _projectionComponent.projecteeId(),
            _destination,
            _projectionComponent.aberration(),
            time, lightTime);
    psc position = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
 
    position[3] += (3 + _camScaling[1]);
    glm::vec3 cpos = position.vec3();

    _projectorMatrix = _projectionComponent.computeProjectorMatrix(
        cpos, boresight, _up, _instrumentMatrix,
        _projectionComponent.fieldOfViewY(),
        _projectionComponent.aspectRatio(),
        _projectionComponent.nearPlane(),
        _projectionComponent.farPlane(),
        _boresight
    );
}

void RenderableModelProjection::project() {
    for (auto img : _imageTimes) {
        attitudeParameters(img.startTime);
        imageProjectGPU(_projectionComponent.loadProjectionTexture(img.path));
    }
    _capture = false;
}

bool RenderableModelProjection::loadTextures() {
    _baseTexture = nullptr;
    if (_colorTexturePath.value() != "") {
        _baseTexture = std::move(
            ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath))
        );
        if (_baseTexture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
            _baseTexture->uploadTexture();
            _baseTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }

    return _baseTexture != nullptr;
}

}  // namespace openspace
