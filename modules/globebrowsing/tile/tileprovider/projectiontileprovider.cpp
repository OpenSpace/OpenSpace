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

#include <modules/globebrowsing/tile/tileprovider/projectiontileprovider.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/scene/scenegraphnode.h>

#include <modules/space/rendering/planetgeometry.h>
#include <modules/newhorizons/util/imagesequencer.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const char* _loggerCat = "ProjectionTileProvider";

    const char* keyGeometry = "Geometry";
    const char* keyProjection = "Projection";
    const char* keyMeridianShift = "Textures.MeridianShift";
    const char* keyColorTexture = "Textures.Color";
    const char* keyHeightTexture = "Textures.Height";

    const char* keyRadius = "Geometry.Radius";
    const char* keyShading = "PerformShading";
    const char* _mainFrame = "GALACTIC";
}

namespace openspace {
namespace globebrowsing {
namespace tileprovider {

/*
documentation::Documentation ProjectionTileProvider::Documentation() {
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
*/

ProjectionTileProvider::ProjectionTileProvider(const ghoul::Dictionary& dictionary) 
    : _fboProgramObject(nullptr)
    , _capture(false)
    , _defaultTile(Tile::TileUnavailable)
{

    ghoul::Dictionary geometryDictionary;
    bool success = dictionary.getValue(
        keyGeometry, geometryDictionary);
    if (success) {
        geometryDictionary.setValue(SceneGraphNode::KeyName, "TestGeometry");
        using planetgeometry::PlanetGeometry;
        _geometry = std::unique_ptr<PlanetGeometry>(
            PlanetGeometry::createFromDictionary(geometryDictionary)
        );
    }

    _projectionComponent.initialize(dictionary.value<ghoul::Dictionary>(keyProjection));


    addPropertySubOwner(_geometry.get());
    addPropertySubOwner(_projectionComponent);
}

ProjectionTileProvider::~ProjectionTileProvider(){
    
}

bool ProjectionTileProvider::initialize() {
    bool completeSuccess = true;
    completeSuccess &= TileProvider::initialize();

    _fboProgramObject = ghoul::opengl::ProgramObject::Build("fboPassProgram",
        "${MODULE_NEWHORIZONS}/shaders/renderablePlanetProjection_vs.glsl",
        "${MODULE_NEWHORIZONS}/shaders/renderablePlanetProjection_fs.glsl"
    );

    completeSuccess &= _projectionComponent.initializeGL();
    completeSuccess &= _geometry->initialize(nullptr);

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

bool ProjectionTileProvider::deinitialize() {
    _projectionComponent.deinitialize();
    _geometry = nullptr;

    glDeleteVertexArrays(1, &_quad);
    glDeleteBuffers(1, &_vertexPositionBuffer);

    _fboProgramObject = nullptr;

    return true;
}

void ProjectionTileProvider::update() {
    // Update
    if (_fboProgramObject->isDirty()) {
        _fboProgramObject->rebuildFromFile();
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

    _stateMatrix = glm::dmat3(1.0);














    // Projection

    if (_projectionComponent.needsClearProjection())
        _projectionComponent.clearAllProjections();

    _camScaling = glm::vec2(1.0);// data.camera.scaling();
    _up = glm::vec3(0,1,0);// data.camera.lookUpVectorCameraSpace();

    if (_capture && _projectionComponent.doesPerformProjection()) {
        for (const Image& img : _imageTimes) {
            attitudeParameters(img.timeRange.start);
            imageProjectGPU(_projectionComponent.loadProjectionTexture(img.path));
        }
        _capture = false;
    }
    attitudeParameters(_time);
    _imageTimes.clear();

}

void ProjectionTileProvider::reset() {
}

int ProjectionTileProvider::maxLevel() {
    return 3;
}

Tile ProjectionTileProvider::getTile(const TileIndex& tileIndex) {
    _projectionComponent.projectionTexture();
}

float ProjectionTileProvider::noDataValueAsFloat() {

}

Tile ProjectionTileProvider::getDefaultTile() {

}

Tile::Status ProjectionTileProvider::getTileStatus(const TileIndex& tileIndex) {
    return Tile::Status::OK;
}

TileDepthTransform ProjectionTileProvider::depthTransform() {
    
}

void ProjectionTileProvider::attitudeParameters(double time) {
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
  
    _transform = glm::mat4(_stateMatrix);

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
    float radius = 1185000.0f;
  
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

void ProjectionTileProvider::imageProjectGPU(
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


} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace
