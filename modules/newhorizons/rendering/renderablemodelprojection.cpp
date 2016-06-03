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

#include <modules/newhorizons/util/imagesequencer.h>
#include <modules/newhorizons/util/labelparser.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

namespace {
    const std::string _loggerCat = "RenderableModelProjection";
    const std::string keySource = "Rotation.Source";
    const std::string keyDestination = "Rotation.Destination";
    const std::string keyBody = "Body";
    const std::string keyGeometry = "Geometry";

    const std::string keyTextureColor = "Textures.Color";
    const std::string keyTextureProject = "Textures.Project";
    const std::string keyTextureDefault = "Textures.Default";

    const std::string keySequenceDir = "Projection.Sequence";
    const std::string keySequenceType = "Projection.SequenceType";
    const std::string keyProjObserver = "Projection.Observer";
    const std::string keyProjTarget = "Projection.Target";
    const std::string keyProjAberration = "Projection.Aberration";

    const std::string keyInstrument = "Instrument.Name";
    const std::string keyInstrumentFovy = "Instrument.Fovy";
    const std::string keyInstrumentAspect = "Instrument.Aspect";
    const std::string keyInstrumentNear = "Instrument.Near";
    const std::string keyInstrumentFar = "Instrument.Far";

    const std::string keyTranslation = "DataInputTranslation";
    const std::string sequenceTypeImage = "image-sequence";
}

namespace openspace {

RenderableModelProjection::RenderableModelProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _rotationX("rotationX", "RotationX", 0, 0, 360)
    , _rotationY("rotationY", "RotationY", 0, 0, 360)
    , _rotationZ("rotationZ", "RotationZ", 0, 0, 360)
    , _programObject(nullptr)
    , _fboProgramObject(nullptr)
    , _baseTexture(nullptr)
    , _projectionTexture(nullptr)
    , _projectionFading("projectionFading", "Projection Fading", 1.f, 0.f, 1.f)
    , _geometry(nullptr)
    , _alpha(1.f)
    , _performShading("performShading", "Perform Shading", true)
    , _performProjection("performProjection", "Perform Projections", true)
    , _clearAllProjections("clearAllProjections", "Clear Projections", false)
    , _frameCount(0)
    , _programIsDirty(false)
{
    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(success, "Name was not passed to RenderableModelProjection");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(keyGeometry, geometryDictionary);
    if (success) {
        geometryDictionary.setValue(SceneGraphNode::KeyName, name);
        _geometry = std::unique_ptr<modelgeometry::ModelGeometry>(modelgeometry::ModelGeometry::createFromDictionary(geometryDictionary));
    }

    std::string texturePath = "";
    success = dictionary.getValue(keyTextureColor, texturePath);
    if (success)
        _colorTexturePath = absPath(texturePath);
        
    success = dictionary.getValue(keyTextureDefault, texturePath);
    if (success)
        _defaultProjImage = absPath(texturePath);

    addPropertySubOwner(_geometry.get());

    addProperty(_projectionFading);

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderableModelProjection::loadTextures, this));

    dictionary.getValue(keySource, _source);
    dictionary.getValue(keyDestination, _destination);
    dictionary.getValue(keyBody, _target);
    if (_target != "")
        setBody(_target);

    bool completeSuccess = true;
    completeSuccess &= dictionary.getValue(keyInstrument, _instrumentID);
    completeSuccess &= dictionary.getValue(keyProjObserver, _projectorID);
    completeSuccess &= dictionary.getValue(keyProjTarget, _projecteeID);
    completeSuccess &= dictionary.getValue(keyInstrumentFovy, _fovy);
    completeSuccess &= dictionary.getValue(keyInstrumentAspect, _aspectRatio);
    completeSuccess &= dictionary.getValue(keyInstrumentNear, _nearPlane);
    completeSuccess &= dictionary.getValue(keyInstrumentFar, _farPlane);
    ghoul_assert(completeSuccess, "All neccessary attributes not found in modfile");
    
    std::string a = "NONE";
    bool s = dictionary.getValue(keyProjAberration, a);
    _aberration = SpiceManager::AberrationCorrection(a);
    completeSuccess &= s;
    ghoul_assert(completeSuccess, "All neccessary attributes not found in modfile");
    
    openspace::SpiceManager::ref().addFrame(_target, _source);
    setBoundingSphere(pss(1.f, 9.f));

    addProperty(_performShading);
    addProperty(_performProjection);
    addProperty(_clearAllProjections);
    addProperty(_rotationX);
    addProperty(_rotationY);
    addProperty(_rotationZ);

    SequenceParser* parser;

    bool foundSequence = dictionary.getValue(keySequenceDir, _sequenceSource);
    if (foundSequence) {
        _sequenceSource = absPath(_sequenceSource);

        foundSequence = dictionary.getValue(keySequenceType, _sequenceType);
        ghoul_assert(foundSequence, "Did not find sequence");
        //Important: client must define translation-list in mod file IFF playbook
        if (dictionary.hasKey(keyTranslation)) {
            ghoul::Dictionary translationDictionary;
            //get translation dictionary
            dictionary.getValue(keyTranslation, translationDictionary);
            if (_sequenceType == sequenceTypeImage) {
                parser = new LabelParser(name, _sequenceSource, translationDictionary);
                openspace::ImageSequencer::ref().runSequenceParser(parser);

            }
        }
        else {
            LWARNING("No translation provided, please make sure all spice calls match playbook!");
        }
    }

}

bool RenderableModelProjection::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_baseTexture != nullptr);
    ready &= (_projectionTexture != nullptr);
    return ready;
}

bool RenderableModelProjection::initialize() {
    bool completeSuccess = true;
        
    if (_programObject == nullptr) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _programObject = renderEngine.buildRenderProgram("ModelShader",
            "${MODULE_NEWHORIZONS}/shaders/modelShader_vs.glsl",
            "${MODULE_NEWHORIZONS}/shaders/modelShader_fs.glsl");


        if (!_programObject)
            return false;
    }
    _programObject->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*) { this->_programIsDirty = true; } );

    if (_fboProgramObject == nullptr) {
        _fboProgramObject = ghoul::opengl::ProgramObject::Build("ProjectionPass",
            "${MODULE_NEWHORIZONS}/shaders/projectionPass_vs.glsl",
            "${MODULE_NEWHORIZONS}/shaders/projectionPass_fs.glsl");
        _fboProgramObject->setIgnoreUniformLocationError(ghoul::opengl::ProgramObject::IgnoreError::Yes);
        if (!_fboProgramObject)
            return false;
    }
    _fboProgramObject->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*) { this->_programIsDirty = true; } );

    loadTextures();

    completeSuccess &= (_baseTexture != nullptr);
    completeSuccess &= (_projectionTexture != nullptr);

    completeSuccess &= _geometry->initialize(this);
    completeSuccess &= !_source.empty();
    completeSuccess &= !_destination.empty();
        

    bool gotverts = _geometry->getVertices(&_geometryVertecies) && _geometry->getIndices(&_geometryIndeces);
    if (!gotverts)
        LWARNING("Lack of vertex data from geometry for image projection");

    completeSuccess &= auxiliaryRendertarget();

    return completeSuccess;
}

bool RenderableModelProjection::auxiliaryRendertarget() {
    bool completeSuccess = true;
    // set FBO to texture to project to

    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    glGenFramebuffers(1, &_fboID);
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, *_projectionTexture, 0);
    // check FBO status
    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE)
        completeSuccess &= false;
    // switch back to window-system-provided framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

    int vertexSize = sizeof(modelgeometry::ModelGeometry::Vertex);

    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vbo);
    glGenBuffers(1, &_ibo);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, _geometryVertecies.size() * vertexSize, &_geometryVertecies[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, vertexSize,
        reinterpret_cast<const GLvoid*>(offsetof(modelgeometry::ModelGeometry::Vertex, location)));
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, vertexSize,
        reinterpret_cast<const GLvoid*>(offsetof(modelgeometry::ModelGeometry::Vertex, tex)));
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, vertexSize,
        reinterpret_cast<const GLvoid*>(offsetof(modelgeometry::ModelGeometry::Vertex, normal)));
            
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, _geometryIndeces.size() * sizeof(int), &_geometryIndeces[0], GL_STATIC_DRAW);

    glBindVertexArray(0);

    return completeSuccess;
}

bool RenderableModelProjection::deinitialize() {
    if (_geometry)
        _geometry->deinitialize();

    _geometry = nullptr;
    _baseTexture = nullptr;
    _projectionTexture = nullptr;

    glDeleteBuffers(1, &_vbo);

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    return true;
}

void RenderableModelProjection::clearAllProjections() {
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);

    glViewport(0, 0, static_cast<GLsizei>(_projectionTexture->width()), static_cast<GLsizei>(_projectionTexture->height()));

    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(m_viewport[0], m_viewport[1],
               m_viewport[2], m_viewport[3]);



    _clearAllProjections = false;
}

void RenderableModelProjection::render(const RenderData& data) {
    if (!_programObject)
        return;

    if (_clearAllProjections)
        clearAllProjections();

    _frameCount++;

    _camScaling = data.camera.scaling();
    _up = data.camera.lookUpVector();

    if (_capture && _performProjection)
        project();

    _programObject->activate();


    attitudeParameters(_time);
    _imageTimes.clear();
        
    double time = openspace::Time::ref().currentTime();
    bool targetPositionCoverage = openspace::SpiceManager::ref().hasSpkCoverage(_target, time);
    if (!targetPositionCoverage) {
        int frame = _frameCount % 180;

        float fadingFactor = static_cast<float>(sin((frame * M_PI) / 180));
        _alpha = 0.5f + fadingFactor * 0.5f;
    }
    else
        _alpha = 1.0f;
        
    _programObject->setUniform("_performShading", _performShading);
    _programObject->setUniform("sun_pos", _sunPosition.vec3());
    _programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _programObject->setUniform("ModelTransform", _transform);
    _programObject->setUniform("_projectionFading", _projectionFading);
    setPscUniforms(*_programObject, data.camera, data.position);

    _geometry->setUniforms(*_programObject);
    
    ghoul::opengl::TextureUnit unit[2];
    unit[0].activate();
    _baseTexture->bind();
    _programObject->setUniform("baseTexture", unit[0]);

    unit[1].activate();
    _projectionTexture->bind();
    _programObject->setUniform("projectionTexture", unit[1]);

    _geometry->render();
        
    // disable shader
    _programObject->deactivate();
}

void RenderableModelProjection::update(const UpdateData& data) {
    if (_programIsDirty) {
        _programObject->rebuildFromFile();
        _fboProgramObject->rebuildFromFile();
        _programIsDirty = false;
    }
        
    _time = data.time;

    if (openspace::ImageSequencer::ref().isReady() && _performProjection) {
        openspace::ImageSequencer::ref().updateSequencer(_time);
        _capture = openspace::ImageSequencer::ref().getImagePaths(_imageTimes, _projecteeID, _instrumentID);
    }
        
    // set spice-orientation in accordance to timestamp
    if (!_source.empty()) {
        _stateMatrix = SpiceManager::ref().positionTransformMatrix(_source, _destination, _time);
    }

    double  lt;
    glm::dvec3 p =
    openspace::SpiceManager::ref().targetPosition("SUN", _target, "GALACTIC", {}, _time, lt);
    _sunPosition = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
}

void RenderableModelProjection::imageProjectGPU(std::unique_ptr<ghoul::opengl::Texture> projectionTexture) {
    // keep handle to the current bound FBO
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    
    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);    
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);

    glViewport(0, 0, static_cast<GLsizei>(_projectionTexture->width()), static_cast<GLsizei>(_projectionTexture->height()));


    _fboProgramObject->activate();

    ghoul::opengl::TextureUnit unitFboProject;
    unitFboProject.activate();
    projectionTexture->bind();
    _fboProgramObject->setUniform("projectionTexture", unitFboProject);

    _fboProgramObject->setUniform("ProjectorMatrix", _projectorMatrix);
    _fboProgramObject->setUniform("ModelTransform", _transform);
    _fboProgramObject->setUniform("_scaling", _camScaling);
    _fboProgramObject->setUniform("boresight", _boresight);

    _geometry->setUniforms(*_fboProgramObject);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
    glDrawElements(GL_TRIANGLES, static_cast<GLsizei>(_geometryIndeces.size()), GL_UNSIGNED_INT, 0);
    glBindVertexArray(0);
    
    _fboProgramObject->deactivate();

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(m_viewport[0], m_viewport[1],
        m_viewport[2], m_viewport[3]);
}

void RenderableModelProjection::attitudeParameters(double time) {
    try {
        _stateMatrix = SpiceManager::ref().positionTransformMatrix(_source, _destination, time);
        _instrumentMatrix = SpiceManager::ref().positionTransformMatrix(_instrumentID, _destination, time);
    }
    catch (const SpiceManager::SpiceException& e) {
        return;
    }

    _transform = glm::mat4(1);

    glm::mat4 rotPropX = glm::rotate(_transform, glm::radians(static_cast<float>(_rotationX)), glm::vec3(1, 0, 0));
    glm::mat4 rotPropY = glm::rotate(_transform, glm::radians(static_cast<float>(_rotationY)), glm::vec3(0, 1, 0));
    glm::mat4 rotPropZ = glm::rotate(_transform, glm::radians(static_cast<float>(_rotationZ)), glm::vec3(0, 0, 1));
        
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            _transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }
    _transform = _transform * rotPropX * rotPropY * rotPropZ;

    glm::dvec3 boresight;
    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_instrumentID);
        boresight = std::move(res.boresightVector);
    } catch (const SpiceManager::SpiceException& e) {
        return;
    }

    double lightTime;
    glm::dvec3 p =
        SpiceManager::ref().targetPosition(_projectorID, _projecteeID, _destination, _aberration, time, lightTime);
    psc position = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
 
    position[3] += (3 + _camScaling[1]);
    glm::vec3 cpos = position.vec3();

    _projectorMatrix = computeProjectorMatrix(cpos, boresight, _up);
}

glm::mat4 RenderableModelProjection::computeProjectorMatrix(const glm::vec3 loc, glm::dvec3 aim, const glm::vec3 up) {
    //rotate boresight into correct alignment
    _boresight = _instrumentMatrix*aim;
    glm::vec3 uptmp(_instrumentMatrix*glm::dvec3(up));

    // create view matrix
    glm::vec3 e3 = glm::normalize(_boresight);
    glm::vec3 e1 = glm::normalize(glm::cross(uptmp, e3));
    glm::vec3 e2 = glm::normalize(glm::cross(e3, e1));
    glm::mat4 projViewMatrix = glm::mat4(e1.x, e2.x, e3.x, 0.f,
                                            e1.y, e2.y, e3.y, 0.f,
                                            e1.z, e2.z, e3.z, 0.f,
                                            -glm::dot(e1, loc), -glm::dot(e2, loc), -glm::dot(e3, loc), 1.f);
        
    // create perspective projection matrix
    glm::mat4 projProjectionMatrix = glm::perspective(glm::radians(_fovy), _aspectRatio, _nearPlane, _farPlane);
    // bias matrix
    glm::mat4 projNormalizationMatrix = glm::mat4(0.5f, 0, 0, 0,
                                                    0, 0.5f, 0, 0,
                                                    0, 0, 0.5f, 0,
                                                    0.5f, 0.5f, 0.5f, 1);
    return projNormalizationMatrix*projProjectionMatrix*projViewMatrix;
}


void RenderableModelProjection::project() {
    for (auto img : _imageTimes) {
        //std::thread t1(&RenderableModelProjection::attitudeParameters, this, img.startTime);
        //t1.join();
        attitudeParameters(img.startTime);
        //_projectionTexturePath = img.path;
        imageProjectGPU(loadProjectionTexture(img.path)); //fbopass
    }
    _capture = false;
}

void RenderableModelProjection::loadTextures() {
    _baseTexture = nullptr;
    if (_colorTexturePath.value() != "") {
        _baseTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath)));
        if (_baseTexture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
            _baseTexture->uploadTexture();
            _baseTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }

    int maxSize = OpenGLCap.max2DTextureSize() / 2;

    LINFO("Creating projection texture of size '" << maxSize << ", " << maxSize / 2 << "'");
    _projectionTexture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(maxSize, maxSize / 2, 1),
        ghoul::opengl::Texture::Format::RGBA
        );
    _projectionTexture->uploadTexture();
}

std::unique_ptr<ghoul::opengl::Texture> RenderableModelProjection::loadProjectionTexture(const std::string& texturePath) {
    std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(texturePath));

    if (texture) {
        texture->uploadTexture();
        // TODO: AnisotropicMipMap crashes on ATI cards ---abock
        //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
    }

    return texture;
}

}  // namespace openspace
