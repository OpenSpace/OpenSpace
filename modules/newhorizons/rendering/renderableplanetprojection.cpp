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

#include <modules/newhorizons/rendering/renderableplanetprojection.h>

#include <modules/space/rendering/planetgeometry.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/opengl/textureunit.h>

#include <modules/newhorizons/util/imagesequencer.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#ifdef WIN32
#define _USE_MATH_DEFINES
#include <math.h>
#endif

namespace {
    const std::string _loggerCat = "RenderablePlanetProjection";

    const char* keyGeometry = "Geometry";
    const char* keyProjection = "Projection";
    const char* keyMeridianShift = "Textures.MeridianShift";
    const char* keyColorTexture = "Textures.Color";
    const char* keyHeightTexture = "Textures.Height";

    const char* keyRadius = "Geometry.Radius";
//    const char* keyShading = "PerformShading";
    const char* _mainFrame = "GALACTIC";
}

namespace openspace {

documentation::Documentation RenderablePlanetProjection::Documentation() {
    using namespace openspace::documentation;
    return {
        "Renderable Planet Projection",
        "newhorizons_renderable_planetprojection",
        {
            {
                "Type",
                new StringEqualVerifier("RenderablePlanetProjection"),
                "",
                Optional::No
            },
            {
                keyGeometry,
                new ReferencingVerifier("space_geometry_planet"),
                "The geometry that is used for rendering this planet.",
                Optional::No
            },
            {
                keyProjection,
                new ReferencingVerifier("newhorizons_projectioncomponent"),
                "Contains information about projecting onto this planet.",
                Optional::No
            },
            {
                keyMeridianShift,
                new BoolVerifier,
                "Determines whether the meridian of the planet should be shifted by 180 "
                "degrees. The default value is 'false'",
                Optional::Yes
            },
            {
                keyColorTexture,
                new StringVerifier,
                "The path to the base color texture that is used on the planet prior to "
                "any image projection. The path can use tokens of the form '${...}' or "
                "be specified relative to the directory of the mod file.",
                Optional::No
            },
            {
                keyHeightTexture,
                new StringVerifier,
                "The path to the height map texture that is used on the planet. The path "
                "can use tokens of the form '${...}' or be specified relative to the "
                "directory of the mod file. If no height map is specified the planet "
                "does not use a height field.",
                Optional::Yes
            }
        }
    };
}

RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("planetTexture", "RGB Texture")
    , _heightMapTexturePath("heightMap", "Heightmap Texture")
    , _rotation("rotation", "Rotation", 0, 0, 360)
    , _programObject(nullptr)
    , _fboProgramObject(nullptr)
    , _baseTexture(nullptr)
    , _heightMapTexture(nullptr)
    , _shiftMeridianBy180("shiftMeiridian", "Shift Meridian by 180 deg", false)
    , _heightExaggeration("heightExaggeration", "Height Exaggeration", 1.f, 0.f, 100.f)
    , _debugProjectionTextureRotation("debug.projectionTextureRotation", "Projection Texture Rotation", 0.f, 0.f, 360.f)
    , _capture(false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlanetProjection"
    );

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

    _projectionComponent.initialize(dictionary.value<ghoul::Dictionary>(keyProjection));

    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
    success = dictionary.getValue("Textures.Color", texturePath);
    if (success) {
        _colorTexturePath = absPath(texturePath); 
    }

    std::string heightMapPath = "";
    success = dictionary.getValue("Textures.Height", heightMapPath);
    if (success) {
        _heightMapTexturePath = absPath(heightMapPath);
    }

    if (dictionary.hasKeyAndValue<bool>(keyMeridianShift)) {
        _shiftMeridianBy180 = dictionary.value<bool>(keyMeridianShift);
    }

    float radius = std::pow(10.0, 9.0);
    dictionary.getValue(keyRadius, radius);
    setBoundingSphere(radius);

    addPropertySubOwner(_geometry.get());
    addPropertySubOwner(_projectionComponent);

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTextures, this));

    addProperty(_heightMapTexturePath);
    _heightMapTexturePath.onChange(std::bind(&RenderablePlanetProjection::loadTextures, this));

    addProperty(_heightExaggeration);
    addProperty(_debugProjectionTextureRotation);

    addProperty(_shiftMeridianBy180);
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
    completeSuccess &= _projectionComponent.initializeGL();
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
    _projectionComponent.deinitialize();
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
    return _geometry && _programObject && _baseTexture && _projectionComponent.isReady();
}

void RenderablePlanetProjection::imageProjectGPU(
                                std::shared_ptr<ghoul::opengl::Texture> projectionTexture)
{
    _projectionComponent.imageProjectBegin();

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

    _projectionComponent.imageProjectEnd();
}

void RenderablePlanetProjection::attitudeParameters(double time) {
    // precomputations for shader
    _instrumentMatrix = SpiceManager::ref().positionTransformMatrix(
        _projectionComponent.instrumentId(), _mainFrame, time
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

    _transform = glm::mat4(_stateMatrix) * rot * roty * rotProp;

    glm::dvec3 bs;
    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_projectionComponent.instrumentId());
        bs = std::move(res.boresightVector);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERRORC(e.component, e.what());
        return;
    }

    double lightTime;
    glm::dvec3 p = SpiceManager::ref().targetPosition(
        _projectionComponent.projectorId(),
        _projectionComponent.projecteeId(),
        _mainFrame,
        _projectionComponent.aberration(),
        time,
        lightTime
    );
    psc position = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
   
    //change to KM and add psc camera scaling. 
    position[3] += (3 + _camScaling[1]);
    //position[3] += 3;
    glm::vec3 cpos = position.vec3();

    float distance = glm::length(cpos);
    float radius = boundingSphere();

    _projectorMatrix = _projectionComponent.computeProjectorMatrix(
        cpos,
        bs,
        _up,
        _instrumentMatrix,
        _projectionComponent.fieldOfViewY(),
        _projectionComponent.aspectRatio(),
        distance - radius,
        distance + radius,
        _boresight
    );
}

ghoul::opengl::Texture& RenderablePlanetProjection::baseTexture() const {
    return _projectionComponent.projectionTexture();
}

void RenderablePlanetProjection::render(const RenderData& data) {
    if (_projectionComponent.needsClearProjection())
        _projectionComponent.clearAllProjections();

    _camScaling = data.camera.scaling();
    _up = data.camera.lookUpVectorCameraSpace();

    if (_capture && _projectionComponent.doesPerformProjection()) {
        for (const Image& img : _imageTimes) {
            RenderablePlanetProjection::attitudeParameters(img.timeRange.start);
            imageProjectGPU(_projectionComponent.loadProjectionTexture(img.path));
        }
        _capture = false;
    }
    attitudeParameters(_time);
    _imageTimes.clear();

    double  lt;
    glm::dvec3 p =
        SpiceManager::ref().targetPosition("SUN", _projectionComponent.projecteeId(), "GALACTIC", {}, _time, lt);
    psc sun_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);

    // Main renderpass
    _programObject->activate();
    _programObject->setUniform("sun_pos", sun_pos.vec3());
    //_programObject->setUniform("ViewProjection" ,  data.camera.viewProjectionMatrix());
    //_programObject->setUniform("ModelTransform" , _transform);

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    // This is apparently the transform needed to get the model in the coordinate system
    // Used by SPICE. Don't ask me why, it was defined in the function attitudeParameters.
    // SPICE needs a planet to be defined with z in the north pole, x in the prime
    // meridian and y completes the right handed coordinate system.
    // Doing this is part of changing from using the transforms defined by the
    // scenegraph node (data.modelTransform) to achieve higher precision rendering. //KB
    glm::dmat4 rot = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(1, 0, 0));
    glm::dmat4 roty = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(0, -1, 0));
    modelTransform = modelTransform * rot * roty;

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _programObject->setUniform("modelTransform", glm::mat4(modelTransform));
    _programObject->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    _programObject->setUniform("_hasHeightMap", _heightMapTexture != nullptr);
    _programObject->setUniform("_heightExaggeration", _heightExaggeration);
    _programObject->setUniform("_projectionFading", _projectionComponent.projectionFading());

    //_programObject->setUniform("debug_projectionTextureRotation", glm::radians(_debugProjectionTextureRotation.value()));

    //setPscUniforms(*_programObject.get(), data.camera, data.position);

    _programObject->setUniform("shiftMeridian", _shiftMeridianBy180);

    ghoul::opengl::TextureUnit unit[3];
    unit[0].activate();
    _baseTexture->bind();
    _programObject->setUniform("baseTexture", unit[0]);

    unit[1].activate();
    _projectionComponent.projectionTexture().bind();
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

    if (_programObject->isDirty()) {
        _programObject->rebuildFromFile();
    }

    _projectionComponent.update();

    _time = Time::ref().j2000Seconds();
    _capture = false;

    if (openspace::ImageSequencer::ref().isReady()){
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
}

bool RenderablePlanetProjection::loadTextures() {
    using ghoul::opengl::Texture;
    _baseTexture = nullptr;
    if (_colorTexturePath.value() != "") {
        _baseTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(_colorTexturePath)
        );
        if (_baseTexture) {
            ghoul::opengl::convertTextureFormat(*_baseTexture, Texture::Format::RGB);
            _baseTexture->uploadTexture();
            _baseTexture->setFilter(Texture::FilterMode::Linear);
        }
    }

    _heightMapTexture = nullptr;
    if (_heightMapTexturePath.value() != "") {
        _heightMapTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(_heightMapTexturePath)
        );
        if (_heightMapTexture) {
            ghoul::opengl::convertTextureFormat(*_heightMapTexture, Texture::Format::RGB);
            _heightMapTexture->uploadTexture();
            _heightMapTexture->setFilter(Texture::FilterMode::Linear);
        }
    }

    return _baseTexture != nullptr;

}

}  // namespace openspace
