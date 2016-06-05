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

#include <modules/newhorizons/rendering/renderableplanetprojection.h>

#include <modules/base/rendering/planetgeometry.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const std::string _loggerCat = "RenderablePlanetProjection";

    const std::string keyFrame = "Frame";
    const std::string keyGeometry = "Geometry";
    const std::string keyShading = "PerformShading";
    const std::string keyBody = "Body";
    const std::string _mainFrame = "GALACTIC";
}

namespace openspace {

RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("planetTexture", "RGB Texture")
    , _heightMapTexturePath("heightMap", "Heightmap Texture")
    , _rotation("rotation", "Rotation", 0, 0, 360)
    , _heightExaggeration("heightExaggeration", "Height Exaggeration", 1.f, 0.f, 100.f)
    , _debugProjectionTextureRotation("debug.projectionTextureRotation", "Projection Texture Rotation", 0.f, 0.f, 360.f)
    , _programObject(nullptr)
    , _fboProgramObject(nullptr)
    , _baseTexture(nullptr)
    , _heightMapTexture(nullptr)
    , _capture(false)
{
    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(success, "");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(
        keyGeometry, geometryDictionary);
    if (success) {
        geometryDictionary.setValue(SceneGraphNode::KeyName, name);
        using planetgeometry::PlanetGeometry;
        _geometry = std::unique_ptr<PlanetGeometry>(
            PlanetGeometry::createFromDictionary(geometryDictionary)
        );
    }

    dictionary.getValue(keyFrame, _frame);
    dictionary.getValue(keyBody, _target);
    if (_target != "")
        setBody(_target);

    success = initializeProjectionSettings(dictionary);
    ghoul_assert(success, "");

    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
    success = dictionary.getValue("Textures.Color", texturePath);
    if (success){
        _colorTexturePath = absPath(texturePath); 
    }

    std::string heightMapPath = "";
    success = dictionary.getValue("Textures.Height", heightMapPath);
    if (success)
        _heightMapTexturePath = absPath(heightMapPath);

    addPropertySubOwner(_geometry.get());
    addProperty(_performProjection);
    addProperty(_clearAllProjections);

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTextures, this));

    addProperty(_heightMapTexturePath);
    _heightMapTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTextures, this));

    addProperty(_projectionFading);
    addProperty(_heightExaggeration);
    addProperty(_debugProjectionTextureRotation);

    success = initializeParser(dictionary);
    ghoul_assert(success, "");
}

RenderablePlanetProjection::~RenderablePlanetProjection() {}

bool RenderablePlanetProjection::initialize() {
    bool completeSuccess = true;

    _programObject = OsEng.renderEngine().buildRenderProgram("projectiveProgram",
        "${MODULE_NEWHORIZONS}/shaders/renderablePlanet_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/renderablePlanet_fs.glsl"
    );

    _fboProgramObject = ghoul::opengl::ProgramObject::Build("fboPassProgram",
        "${MODULE_NEWHORIZONS}/shaders/renderablePlanetProjection_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/renderablePlanetProjection_fs.glsl"
    );

    completeSuccess &= loadTextures();
    completeSuccess &= ProjectionComponent::initialize();

    completeSuccess &= _geometry->initialize(this);

    if (completeSuccess) {
        //completeSuccess &= auxiliaryRendertarget();
        // SCREEN-QUAD 
        const GLfloat size = 1.f;
        const GLfloat w = 1.f;
        const GLfloat vertex_data[] = {
            -size, -size, 0.f, w, 0.f, 0.f,
            size, size, 0.f, w, 1.f, 1.f,
            -size, size, 0.f, w, 0.f, 1.f,
            -size, -size, 0.f, w, 0.f, 0.f,
            size, -size, 0.f, w, 1.f, 0.f,
            size, size, 0.f, w, 1.f, 1.f,
        };

        glGenVertexArrays(1, &_quad);
        glBindVertexArray(_quad);
        glGenBuffers(1, &_vertexPositionBuffer);
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));

        glBindVertexArray(0);
    }

    return completeSuccess;
}

bool RenderablePlanetProjection::deinitialize() {
    ProjectionComponent::deinitialize();
    _baseTexture = nullptr;
    _geometry = nullptr;

    glDeleteVertexArrays(1, &_quad);
    glDeleteBuffers(1, &_vertexPositionBuffer);

    OsEng.renderEngine().removeRenderProgram(_programObject);
    _programObject = nullptr;

    _fboProgramObject = nullptr;

    return true;
}
bool RenderablePlanetProjection::isReady() const {
    return _geometry && _programObject && _baseTexture && _projectionTexture;
}

void RenderablePlanetProjection::imageProjectGPU(
                                std::shared_ptr<ghoul::opengl::Texture> projectionTexture)
{
    imageProjectBegin();

    _fboProgramObject->activate();

    ghoul::opengl::TextureUnit unitFbo;
    unitFbo.activate();
    projectionTexture->bind();
    _fboProgramObject->setUniform("projectionTexture", unitFbo);
        
    _fboProgramObject->setUniform("ProjectorMatrix", _projectorMatrix);
    _fboProgramObject->setUniform("ModelTransform" , _transform);
    _fboProgramObject->setUniform("_scaling"       , _camScaling);
    _fboProgramObject->setUniform("boresight"      , _boresight);

    if (_geometry->hasProperty("radius")){ 
        ghoul::any r = _geometry->property("radius")->get();
        if (glm::vec4* radius = ghoul::any_cast<glm::vec4>(&r)){
            _fboProgramObject->setUniform("_radius", radius);
        }
    }else{
        LERROR("Geometry object needs to provide radius");
    }
    if (_geometry->hasProperty("segments")){
        ghoul::any s = _geometry->property("segments")->get();
        if (int* segments = ghoul::any_cast<int>(&s)){
            _fboProgramObject->setUniform("_segments", segments[0]);
        }
    }else{
        LERROR("Geometry object needs to provide segment count");
    }

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    _fboProgramObject->deactivate();

    imageProjectEnd();
}

void RenderablePlanetProjection::attitudeParameters(double time) {
    // precomputations for shader
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, _mainFrame, time);
    _instrumentMatrix = SpiceManager::ref().positionTransformMatrix(
        _instrumentID, _mainFrame, time
    );

    _transform = glm::mat4(1);
    //90 deg rotation w.r.t spice req. 
    glm::mat4 rot = glm::rotate(
        _transform,
        static_cast<float>(M_PI_2),
        glm::vec3(1, 0, 0)
    );
    glm::mat4 roty = glm::rotate(
        _transform,
        static_cast<float>(M_PI_2),
        glm::vec3(0, -1, 0)
    );
    glm::mat4 rotProp = glm::rotate(
        _transform,
        static_cast<float>(glm::radians(static_cast<float>(_rotation))),
        glm::vec3(0, 1, 0)
    );

    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            _transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }
    _transform = _transform * rot * roty * rotProp;

    glm::dvec3 bs;
    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_instrumentID);
        bs = std::move(res.boresightVector);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERRORC(e.component, e.what());
        return;
    }

    double lightTime;
    glm::dvec3 p = SpiceManager::ref().targetPosition(
        _projectorID, _projecteeID, _mainFrame, _aberration, time, lightTime
    );
    psc position = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
   
    //change to KM and add psc camera scaling. 
    position[3] += (3 + _camScaling[1]);
    //position[3] += 3;
    glm::vec3 cpos = position.vec3();

    _projectorMatrix = computeProjectorMatrix(cpos, bs, _up, _instrumentMatrix, 
        _fovy, _aspectRatio, _nearPlane, _farPlane, _boresight
    );
}

void RenderablePlanetProjection::render(const RenderData& data) {
    if (_clearAllProjections)
        clearAllProjections();

    _camScaling = data.camera.scaling();
    _up = data.camera.lookUpVector();

    if (_capture && _performProjection) {
        for (const Image& img : _imageTimes) {
            RenderablePlanetProjection::attitudeParameters(img.startTime);
            imageProjectGPU(loadProjectionTexture(img.path));
        }
        _capture = false;
    }
    attitudeParameters(_time);
    _imageTimes.clear();

    double  lt;
    glm::dvec3 p =
        SpiceManager::ref().targetPosition("SUN", _projecteeID, "GALACTIC", {}, _time, lt);
    psc sun_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);

    // Main renderpass
    _programObject->activate();
    _programObject->setUniform("sun_pos", sun_pos.vec3());
    _programObject->setUniform("ViewProjection" ,  data.camera.viewProjectionMatrix());
    _programObject->setUniform("ModelTransform" , _transform);

    _programObject->setUniform("_hasHeightMap", _heightMapTexture != nullptr);
    _programObject->setUniform("_heightExaggeration", _heightExaggeration);
    _programObject->setUniform("_projectionFading", _projectionFading);

    //_programObject->setUniform("debug_projectionTextureRotation", glm::radians(_debugProjectionTextureRotation.value()));

    setPscUniforms(*_programObject.get(), data.camera, data.position);
    
    ghoul::opengl::TextureUnit unit[3];
    unit[0].activate();
    _baseTexture->bind();
    _programObject->setUniform("baseTexture", unit[0]);

    unit[1].activate();
    _projectionTexture->bind();
    _programObject->setUniform("projectionTexture", unit[1]);

    if (_heightMapTexture) {
        unit[2].activate();
        _heightMapTexture->bind();
        _programObject->setUniform("heightTexture", unit[2]);
    }
    
    _geometry->render();
    _programObject->deactivate();
}

void RenderablePlanetProjection::update(const UpdateData& data) {
    if (_fboProgramObject->isDirty()) {
        _fboProgramObject->rebuildFromFile();
    }

    if (_programObject->isDirty())
        _programObject->rebuildFromFile();

    _time = Time::ref().currentTime();
    _capture = false;

    if (openspace::ImageSequencer::ref().isReady() && _performProjection){
        openspace::ImageSequencer::ref().updateSequencer(_time);
        _capture = openspace::ImageSequencer::ref().getImagePaths(
            _imageTimes, _projecteeID, _instrumentID
        );
    }

}

bool RenderablePlanetProjection::loadTextures() {
    using ghoul::opengl::Texture;
    _baseTexture = nullptr;
    if (_colorTexturePath.value() != "") {
        _baseTexture = ghoul::io::TextureReader::ref().loadTexture(_colorTexturePath);
        if (_baseTexture) {
            ghoul::opengl::convertTextureFormat(Texture::Format::RGB, *_baseTexture);
            _baseTexture->uploadTexture();
            _baseTexture->setFilter(Texture::FilterMode::Linear);
        }
    }

    _heightMapTexture = nullptr;
    if (_heightMapTexturePath.value() != "") {
        _heightMapTexture = ghoul::io::TextureReader::ref().loadTexture(_heightMapTexturePath);
        if (_heightMapTexture) {
            ghoul::opengl::convertTextureFormat(Texture::Format::RGB, *_heightMapTexture);
            _heightMapTexture->uploadTexture();
            _heightMapTexture->setFilter(Texture::FilterMode::Linear);
        }
    }

    return _baseTexture != nullptr;

}

}  // namespace openspace
