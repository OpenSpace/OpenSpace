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

#include <modules/base/rendering/modelgeometry.h>

#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const std::string _loggerCat = "RenderableModelProjection";
    const char* keySource = "Rotation.Source";
    const char* keyDestination = "Rotation.Destination";
    const char* keyGeometry = "Geometry";
    const char* keyProjection = "Projection";
    const char* keyBoundingSphereRadius = "BoundingSphereRadius";

    const char* keyTextureColor = "Textures.Color";

    const char* _destination = "GALACTIC";
}

namespace openspace {

Documentation RenderableModelProjection::Documentation() {
    using namespace documentation;

    return {
        "Renderable Model Projection",
        "newhorizons_renderable_modelprojection",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableModelProjection"),
                "",
                Optional::No
            },
            {
                keyGeometry,
                new ReferencingVerifier("base_geometry_model"),
                "The geometry that is used for rendering this model.",
                Optional::No
            },
            {
                keyProjection,
                new ReferencingVerifier("newhorizons_projectioncomponent"),
                "Contains information about projecting onto this planet.",
                Optional::No
            },
            {
                keyTextureColor,
                new StringVerifier,
                "The base texture for the model that is shown before any projection "
                "occurred.",
                Optional::No
            },
            {
                keyBoundingSphereRadius,
                new DoubleVerifier,
                "The radius of the bounding sphere of this object. This has to be a "
                "radius that is larger than anything that is rendered by it. It has to "
                "be at least as big as the convex hull of the object. The default value "
                "is 10e9 meters.",
                Optional::Yes
            }
        }
    };
}

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
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableModelProjection"
    );

    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(success, "Name was not passed to RenderableModelProjection");

    using ghoul::Dictionary;
    Dictionary geometryDictionary = dictionary.value<Dictionary>(keyGeometry);
    using modelgeometry::ModelGeometry;
    geometryDictionary.setValue(SceneGraphNode::KeyName, name);
    _geometry = std::unique_ptr<ModelGeometry>(
        ModelGeometry::createFromDictionary(geometryDictionary)
    );

    _colorTexturePath = absPath(dictionary.value<std::string>(keyTextureColor));
        
    addPropertySubOwner(_geometry.get());
    addPropertySubOwner(_projectionComponent);

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderableModelProjection::loadTextures, this));

    _projectionComponent.initialize(
        dictionary.value<ghoul::Dictionary>(keyProjection)
    );
    
    float boundingSphereRadius = 1.0e9;
    dictionary.getValue(keyBoundingSphereRadius, boundingSphereRadius);
    setBoundingSphere(PowerScaledScalar::CreatePSS(boundingSphereRadius));

    Renderable::addProperty(_performShading);
    Renderable::addProperty(_rotation);
}

RenderableModelProjection::~RenderableModelProjection() {
    // This empty method needs to be here in order to use forward declaration with 
    // std::unique_ptr
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

    _depthFboProgramObject = ghoul::opengl::ProgramObject::Build("DepthPass",
        "${MODULE_NEWHORIZONS}/shaders/renderableModelDepth_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/renderableModelDepth_fs.glsl");


    completeSuccess &= loadTextures();
    completeSuccess &= _projectionComponent.initializeGL();

    auto bs = getBoundingSphere();
    completeSuccess &= _geometry->initialize(this);
    setBoundingSphere(bs); // ignore bounding sphere set by geometry.

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

    _up = data.camera.lookUpVectorCameraSpace();

    if (_capture && _projectionComponent.doesPerformProjection())
        project();

    _programObject->activate();

    attitudeParameters(_time);
    _imageTimes.clear();

    // Calculate variables to be used as uniform variables in shader
    glm::dvec3 bodyPosition = data.modelTransform.translation;

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) * // Rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)); // Scale
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
    glm::vec3 directionToSun = glm::normalize(_sunPosition.vec3() - glm::vec3(bodyPosition));
    glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;
        
    _programObject->setUniform("_performShading", _performShading);
    _programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
    _programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
    _programObject->setUniform("_projectionFading", _projectionComponent.projectionFading());


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
    if (_programObject->isDirty()) {
        _programObject->rebuildFromFile();
    }

    if (_fboProgramObject->isDirty()) {
        _fboProgramObject->rebuildFromFile();
    }

    _projectionComponent.update();
        
    if (_depthFboProgramObject->isDirty())
        _depthFboProgramObject->rebuildFromFile();

    _time = data.time;

    if (openspace::ImageSequencer::ref().isReady()) {
        openspace::ImageSequencer::ref().updateSequencer(_time);
        if (_projectionComponent.doesPerformProjection()) {
            _capture = openspace::ImageSequencer::ref().getImagePaths(
                _imageTimes,
                _projectionComponent.projecteeId(),
                _projectionComponent.instrumentId()
            );
        }
    }
        
    _stateMatrix = data.modelTransform.rotation;
    
    glm::dvec3 p =
        OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldPosition() -
        data.modelTransform.translation;

    _sunPosition = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
}

void RenderableModelProjection::imageProjectGPU(
                                std::shared_ptr<ghoul::opengl::Texture> projectionTexture)
{
    if (_projectionComponent.needsShadowMap()) {
        _projectionComponent.depthMapRenderBegin();
        _depthFboProgramObject->activate();
        _depthFboProgramObject->setUniform("ProjectorMatrix", _projectorMatrix);
        _depthFboProgramObject->setUniform("ModelTransform", _transform);
        _geometry->setUniforms(*_fboProgramObject);

        _geometry->render();

        _depthFboProgramObject->deactivate();
        _projectionComponent.depthMapRenderEnd();
    }

    _projectionComponent.imageProjectBegin();
    _fboProgramObject->activate();

    ghoul::opengl::TextureUnit unitFbo;
    unitFbo.activate();
    projectionTexture->bind();
    _fboProgramObject->setUniform("projectionTexture", unitFbo);

    _fboProgramObject->setUniform("needShadowMap", _projectionComponent.needsShadowMap());

    ghoul::opengl::TextureUnit unitDepthFbo;
    if (_projectionComponent.needsShadowMap()) {
        unitDepthFbo.activate();
        _projectionComponent.depthTexture().bind();
        _fboProgramObject->setUniform("depthTexture", unitDepthFbo);
    }

    _fboProgramObject->setUniform("ProjectorMatrix", _projectorMatrix);
    _fboProgramObject->setUniform("ModelTransform", _transform);
    _fboProgramObject->setUniform("boresight", _boresight);

    _geometry->setUniforms(*_fboProgramObject);
    _geometry->render();

    _fboProgramObject->deactivate();

    _projectionComponent.imageProjectEnd();
}

void RenderableModelProjection::attitudeParameters(double time) {
    try {
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
 
    position[3] += 4;
    glm::vec3 cpos = position.vec3();

    float distance = glm::length(cpos);
    float radius = getBoundingSphere().lengthf();

    _projectorMatrix = _projectionComponent.computeProjectorMatrix(
        cpos, boresight, _up, _instrumentMatrix,
        _projectionComponent.fieldOfViewY(),
        _projectionComponent.aspectRatio(),
        distance - radius,
        distance + radius,
        _boresight
    );
}



void RenderableModelProjection::project() {
    for (auto img : _imageTimes) {
        attitudeParameters(img.timeRange.start);
        auto projTexture = _projectionComponent.loadProjectionTexture(img.path, img.isPlaceholder);
        imageProjectGPU(projTexture);
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
