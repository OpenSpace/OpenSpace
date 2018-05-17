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

#include <modules/spacecraftinstruments/rendering/renderableplanetprojection.h>

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <modules/space/rendering/planetgeometry.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* _loggerCat = "RenderablePlanetProjection";
    constexpr const char* ProjectiveProgramName = "ProjectiveProgram";
    constexpr const char* FBOPassProgramName = "FBOPassProgram";


    constexpr const char* KeyGeometry = "Geometry";
    constexpr const char* KeyProjection = "Projection";

    constexpr const char* KeyRadius = "Geometry.Radius";
//    const char* keyShading = "PerformShading";
    constexpr const char* _mainFrame = "GALACTIC";

    constexpr const char* NoImageText = "No Image";

    static const openspace::properties::Property::PropertyInfo ColorTexturePathsInfo = {
        "ColorTexturePaths",
        "Color Texture",
        "The texture path selected in this property is used as the base texture that is "
        "applied to the planet prior to any image projections. This menu always contains "
        "an empty option for not using a color map. If this value is specified in an "
        "asset, the last texture is used."
    };

    static const openspace::properties::Property::PropertyInfo AddColorTextureInfo = {
        "AddColorTexture",
        "Add Color Base Texture",
        "Adds a new base color texture to the list of selectable base maps used prior to "
        "any image projection."
    };

    static const openspace::properties::Property::PropertyInfo HeightTexturePathsInfo = {
        "HeightTexturePaths",
        "Heightmap Texture",
        "The texture path selected in this property is used as the height map on the "
        "planet. This menu always contains an empty option for not using a heightmap. If "
        "this value is specified in an asset, the last texture is used."
    };

    static const openspace::properties::Property::PropertyInfo AddHeightTextureInfo = {
        "AddHeightTexture",
        "Add Heightmap Texture",
        "Adds a new height map texture to the list of selectable height maps used."
    };

    static const openspace::properties::Property::PropertyInfo HeightExaggerationInfo = {
        "HeightExaggeration",
        "Height Exaggeration",
        "This value determines the level of height exaggeration that is applied to a "
        "potential height field. A value of '0' inhibits the height field, whereas a "
        "value of '1' uses the measured height field."
    };

    static const openspace::properties::Property::PropertyInfo MeridianShiftInfo = {
        "MeridianShift",
        "Meridian Shift",
        "If this value is enabled, a shift of the meridian by 180 degrees is performed. "
        "This is a fix especially for Pluto height maps, where the definition of the "
        "meridian has changed through the New Horizons mission and this requires this "
        "shift."
    };

    static const openspace::properties::Property::PropertyInfo AmbientBrightnessInfo = {
        "AmbientBrightness",
        "Ambient Brightness",
        "This value determines the ambient brightness of the dark side of the planet."
    };
} // namespace

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
                Optional::No,
                ""
            },
            {
                KeyGeometry,
                new ReferencingVerifier("space_geometry_planet"),
                Optional::No,
                "The geometry that is used for rendering this planet.",
            },
            {
                KeyProjection,
                new ReferencingVerifier("newhorizons_projectioncomponent"),
                Optional::No,
                "Contains information about projecting onto this planet.",
            },
            {
                ColorTexturePathsInfo.identifier,
                new StringListVerifier,
                Optional::No,
                ColorTexturePathsInfo.description
            },
            {
                HeightTexturePathsInfo.identifier,
                new StringListVerifier,
                Optional::Yes,
                HeightTexturePathsInfo.description
            },
            {
                HeightExaggerationInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                HeightExaggerationInfo.description
            },
            {
                MeridianShiftInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                MeridianShiftInfo.description
            },
            {
                AmbientBrightnessInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                AmbientBrightnessInfo.description
            }
        }
    };
}

RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dict)
    : Renderable(dict)
    , _colorTexturePaths(ColorTexturePathsInfo)
    , _addColorTexturePath(AddColorTextureInfo)
    , _colorTextureDirty(false)
    , _heightMapTexturePaths(HeightTexturePathsInfo)
    , _addHeightMapTexturePath(AddHeightTextureInfo)
    , _heightMapTextureDirty(false)
    , _programObject(nullptr)
    , _fboProgramObject(nullptr)
    , _baseTexture(nullptr)
    , _heightMapTexture(nullptr)
    , _heightExaggeration(HeightExaggerationInfo, 1.f, 0.f, 1e6f, 1.f, 3.f)
    , _meridianShift(MeridianShiftInfo, false)
    , _ambientBrightness(AmbientBrightnessInfo, 0.075f, 0.f, 1.f)
    , _capture(false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dict,
        "RenderablePlanetProjection"
    );

    ghoul::Dictionary geometryDictionary;
    bool success = dict.getValue(KeyGeometry, geometryDictionary);
    if (success) {
        _geometry = planetgeometry::PlanetGeometry::createFromDictionary(
            geometryDictionary
        );
    }

    _projectionComponent.initialize(
        identifier(),
        dict.value<ghoul::Dictionary>(KeyProjection)
    );

    _colorTexturePaths.addOption(0, NoImageText);
    _colorTexturePaths.onChange([this](){
        _colorTextureDirty = true;
    });
    addProperty(_colorTexturePaths);

    if (dict.hasKey(ColorTexturePathsInfo.identifier)) {
        ghoul::Dictionary value = dict.value<ghoul::Dictionary>(
            ColorTexturePathsInfo.identifier
        );

        for (size_t i = 1; i <= value.size(); ++i) {
            std::string texture = absPath(value.value<std::string>(std::to_string(i)));

            _colorTexturePaths.addOption(
                // as we started with 0, this works
                static_cast<int>(_colorTexturePaths.options().size()),
                texture
            );

            _colorTexturePaths = static_cast<int>(
                _colorTexturePaths.options().size() - 1
            );
        }
    }

    _addColorTexturePath.onChange([this]() {
        if (!_addColorTexturePath.value().empty()) {
            _colorTexturePaths.addOption(
                // as we started with 0, this works
                static_cast<int>(_colorTexturePaths.options().size()),
                _addColorTexturePath.value()
            );

            _colorTexturePaths = static_cast<int>(
                _colorTexturePaths.options().size() - 1
            );

            _addColorTexturePath = "";
        }
    });
    addProperty(_addColorTexturePath);



    _heightMapTexturePaths.addOption(0, NoImageText);
    _heightMapTexturePaths.onChange([this]() {
        _heightMapTextureDirty = true;
    });
    addProperty(_heightMapTexturePaths);


    if (dict.hasKey(HeightTexturePathsInfo.identifier)) {
        ghoul::Dictionary value = dict.value<ghoul::Dictionary>(
            HeightTexturePathsInfo.identifier
        );

        for (size_t i = 1; i <= value.size(); ++i) {
            std::string texture = absPath(value.value<std::string>(std::to_string(i)));

            _heightMapTexturePaths.addOption(
                // as we started with 0, this works
                static_cast<int>(_heightMapTexturePaths.options().size()),
                texture
            );

            _heightMapTexturePaths = static_cast<int>(
                _heightMapTexturePaths.options().size() - 1
            );
        }
    }

    _addHeightMapTexturePath.onChange([this]() {
        if (!_addHeightMapTexturePath.value().empty()) {
            _heightMapTexturePaths.addOption(
                // as we started with 0, this works
                static_cast<int>(_heightMapTexturePaths.options().size()),
                _addHeightMapTexturePath.value()
            );
            _heightMapTexturePaths = static_cast<int>(
                _heightMapTexturePaths.options().size() - 1
            );

            _addHeightMapTexturePath = "";
        }
    });
    addProperty(_addHeightMapTexturePath);


    if (dict.hasKeyAndValue<bool>(MeridianShiftInfo.identifier)) {
        _meridianShift = dict.value<bool>(MeridianShiftInfo.identifier);
    }

    float radius = std::pow(10.f, 9.f);
    dict.getValue(KeyRadius, radius);
    setBoundingSphere(radius);

    addPropertySubOwner(_geometry.get());
    addPropertySubOwner(_projectionComponent);

    if (dict.hasKey(HeightExaggerationInfo.identifier)) {
        _heightExaggeration = static_cast<float>(
            dict.value<double>(HeightExaggerationInfo.identifier)
        );
    }

    addProperty(_heightExaggeration);
    addProperty(_meridianShift);
    addProperty(_ambientBrightness);
}

RenderablePlanetProjection::~RenderablePlanetProjection() {}

void RenderablePlanetProjection::initializeGL() {
    _programObject =
        SpacecraftInstrumentsModule::ProgramObjectManager.requestProgramObject(
            ProjectiveProgramName,
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return OsEng.renderEngine().buildRenderProgram(
                    ProjectiveProgramName,
                    absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/"
                            "renderablePlanet_vs.glsl"
                    ),
                    absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/"
                            "renderablePlanet_fs.glsl"
                    )
                );
            }
        );

    _mainUniformCache.sunPos = _programObject->uniformLocation("sun_pos");
    _mainUniformCache.modelTransform = _programObject->uniformLocation("modelTransform");
    _mainUniformCache.modelViewProjectionTransform = _programObject->uniformLocation(
        "modelViewProjectionTransform"
    );
    _mainUniformCache.hasBaseMap = _programObject->uniformLocation("_hasBaseMap");
    _mainUniformCache.hasHeightMap = _programObject->uniformLocation("_hasHeightMap");
    _mainUniformCache.heightExaggeration = _programObject->uniformLocation(
        "_heightExaggeration"
    );
    _mainUniformCache.meridianShift = _programObject->uniformLocation("_meridianShift");
    _mainUniformCache.ambientBrightness = _programObject->uniformLocation(
        "_ambientBrightness"
    );
    _mainUniformCache.projectionFading = _programObject->uniformLocation(
        "_projectionFading"
    );
    _mainUniformCache.baseTexture = _programObject->uniformLocation("baseTexture");
    _mainUniformCache.projectionTexture = _programObject->uniformLocation(
        "projectionTexture"
    );
    _mainUniformCache.heightTexture = _programObject->uniformLocation("heightTexture");

    _fboProgramObject =
        SpacecraftInstrumentsModule::ProgramObjectManager.requestProgramObject(
            FBOPassProgramName,
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return ghoul::opengl::ProgramObject::Build(
                    FBOPassProgramName,
                        absPath(
                            "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/"
                            "renderablePlanetProjection_vs.glsl"
                        ),
                        absPath(
                            "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/"
                            "renderablePlanetProjection_fs.glsl"
                        )
                );
            }
        );

    _fboUniformCache.projectionTexture = _fboProgramObject->uniformLocation(
        "projectionTexture"
    );
    _fboUniformCache.projectorMatrix = _fboProgramObject->uniformLocation(
        "ProjectorMatrix"
    );
    _fboUniformCache.modelTransform = _fboProgramObject->uniformLocation(
        "ModelTransform"
    );
    _fboUniformCache.scaling = _fboProgramObject->uniformLocation("_scaling");
    _fboUniformCache.boresight = _fboProgramObject->uniformLocation("boresight");
    _fboUniformCache.radius = _fboProgramObject->uniformLocation("_radius");
    _fboUniformCache.segments = _fboProgramObject->uniformLocation("_segments");

    loadColorTexture();
    loadHeightTexture();
    _projectionComponent.initializeGL();
    _geometry->initialize(this);

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
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );

    glBindVertexArray(0);
}

void RenderablePlanetProjection::deinitializeGL() {
    _projectionComponent.deinitialize();
    _baseTexture = nullptr;
    _geometry = nullptr;

    glDeleteVertexArrays(1, &_quad);
    glDeleteBuffers(1, &_vertexPositionBuffer);

    SpacecraftInstrumentsModule::ProgramObjectManager.releaseProgramObject(
        ProjectiveProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            OsEng.renderEngine().removeRenderProgram(p);
        }
    );
    _programObject = nullptr;

    SpacecraftInstrumentsModule::ProgramObjectManager.releaseProgramObject(
        FBOPassProgramName
    );
    _fboProgramObject = nullptr;
}

bool RenderablePlanetProjection::isReady() const {
    return _geometry && _programObject && _projectionComponent.isReady();
}

void RenderablePlanetProjection::imageProjectGPU(
                                std::shared_ptr<ghoul::opengl::Texture> projectionTexture)
{
    _projectionComponent.imageProjectBegin();

    _fboProgramObject->activate();

    ghoul::opengl::TextureUnit unitFbo;
    unitFbo.activate();
    projectionTexture->bind();
    _fboProgramObject->setUniform(_fboUniformCache.projectionTexture, unitFbo);

    _fboProgramObject->setUniform(_fboUniformCache.projectorMatrix, _projectorMatrix);
    _fboProgramObject->setUniform(_fboUniformCache.modelTransform, _transform);
    _fboProgramObject->setUniform(_fboUniformCache.scaling, _camScaling);
    _fboProgramObject->setUniform(_fboUniformCache.boresight, _boresight);

    if (_geometry->hasProperty("Radius")) {
        ghoul::any r = _geometry->property("Radius")->get();
        if (glm::vec3* radius = ghoul::any_cast<glm::vec3>(&r)){
            _fboProgramObject->setUniform(_fboUniformCache.radius, radius);
        }
    } else {
        LERROR("Geometry object needs to provide radius");
    }
    if (_geometry->hasProperty("Segments")) {
        ghoul::any s = _geometry->property("Segments")->get();
        if (int* segments = ghoul::any_cast<int>(&s)) {
            _fboProgramObject->setUniform(_fboUniformCache.segments, segments[0]);
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
        glm::half_pi<float>(),
        glm::vec3(1, 0, 0)
    );
    glm::mat4 roty = glm::rotate(
        _transform,
        glm::half_pi<float>(),
        glm::vec3(0, -1, 0)
    );

    _transform = glm::mat4(_stateMatrix) * rot * roty;

    glm::dvec3 bs;
    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(
            _projectionComponent.instrumentId()
        );
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

void RenderablePlanetProjection::render(const RenderData& data, RendererTasks&) {
    if (_projectionComponent.needsClearProjection()) {
        _projectionComponent.clearAllProjections();
    }

    if (_projectionComponent.needsMipMapGeneration()) {
        _projectionComponent.generateMipMap();
    }

    _camScaling = glm::vec2(1.f, 0.f); // Unit scaling
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
    glm::dvec3 p = SpiceManager::ref().targetPosition(
        "SUN",
        _projectionComponent.projecteeId(),
        "GALACTIC",
        {},
        _time,
        lt
    );
    psc sun_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);

    // Main renderpass
    _programObject->activate();
    _programObject->setUniform(_mainUniformCache.sunPos, sun_pos.vec3());
    //_programObject->setUniform("ViewProjection" ,  data.camera.viewProjectionMatrix());
    //_programObject->setUniform("ModelTransform" , _transform);

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    // This is apparently the transform needed to get the model in the coordinate system
    // Used by SPICE. Don't ask me why, it was defined in the function attitudeParameters.
    // SPICE needs a planet to be defined with z in the north pole, x in the prime
    // meridian and y completes the right handed coordinate system.
    // Doing this is part of changing from using the transforms defined by the
    // scenegraph node (data.modelTransform) to achieve higher precision rendering. //KB
    glm::dmat4 rot = glm::rotate(
        glm::dmat4(1.0),
        glm::half_pi<double>(),
        glm::dvec3(1.0, 0.0, 0.0)
    );
    glm::dmat4 roty = glm::rotate(
        glm::dmat4(1.0),
        glm::half_pi<double>(),
        glm::dvec3(0.0, -1.0, 0.0)
    );
    modelTransform = modelTransform * rot * roty;

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _programObject->setUniform(
        _mainUniformCache.modelTransform,
        glm::mat4(modelTransform)
    );
    _programObject->setUniform(_mainUniformCache
        .modelViewProjectionTransform,
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );

    _programObject->setUniform(_mainUniformCache.hasBaseMap, _baseTexture != nullptr);
    _programObject->setUniform(
        _mainUniformCache.hasHeightMap,
        _heightMapTexture != nullptr
    );
    _programObject->setUniform(_mainUniformCache.heightExaggeration, _heightExaggeration);
    _programObject->setUniform(_mainUniformCache.meridianShift, _meridianShift);
    _programObject->setUniform(_mainUniformCache.ambientBrightness, _ambientBrightness);
    _programObject->setUniform(
        _mainUniformCache.projectionFading,
        _projectionComponent.projectionFading()
    );

    ghoul::opengl::TextureUnit unit[3];
    if (_baseTexture) {
        unit[0].activate();
        _baseTexture->bind();
        _programObject->setUniform(_mainUniformCache.baseTexture, unit[0]);
    }

    unit[1].activate();
    _projectionComponent.projectionTexture().bind();
    _programObject->setUniform(_mainUniformCache.projectionTexture, unit[1]);

    if (_heightMapTexture) {
        unit[2].activate();
        _heightMapTexture->bind();
        _programObject->setUniform(_mainUniformCache.heightTexture, unit[2]);
    }

    _geometry->render();
    _programObject->deactivate();
}

void RenderablePlanetProjection::update(const UpdateData& data) {
    if (_programObject->isDirty()) {
        _programObject->rebuildFromFile();

        _mainUniformCache.sunPos = _programObject->uniformLocation("sun_pos");
        _mainUniformCache.modelTransform = _programObject->uniformLocation(
            "modelTransform"
        );
        _mainUniformCache.modelViewProjectionTransform = _programObject->uniformLocation(
            "modelViewProjectionTransform"
        );
        _mainUniformCache.hasBaseMap = _programObject->uniformLocation("_hasBaseMap");
        _mainUniformCache.hasHeightMap = _programObject->uniformLocation("_hasHeightMap");
        _mainUniformCache.heightExaggeration = _programObject->uniformLocation(
            "_heightExaggeration"
        );
        _mainUniformCache.meridianShift = _programObject->uniformLocation(
            "_meridianShift"
        );
        _mainUniformCache.ambientBrightness = _programObject->uniformLocation(
            "_ambientBrightness"
        );
        _mainUniformCache.projectionFading = _programObject->uniformLocation(
            "_projectionFading"
        );
        _mainUniformCache.baseTexture = _programObject->uniformLocation("baseTexture");
        _mainUniformCache.projectionTexture = _programObject->uniformLocation(
            "projectionTexture"
        );
        _mainUniformCache.heightTexture = _programObject->uniformLocation(
            "heightTexture"
        );
    }

    if (_fboProgramObject->isDirty()) {
        _fboProgramObject->rebuildFromFile();

        _fboUniformCache.projectionTexture = _fboProgramObject->uniformLocation(
            "projectionTexture"
        );
        _fboUniformCache.projectorMatrix = _fboProgramObject->uniformLocation(
            "ProjectorMatrix"
        );
        _fboUniformCache.modelTransform = _fboProgramObject->uniformLocation(
            "ModelTransform"
        );
        _fboUniformCache.scaling = _fboProgramObject->uniformLocation("_scaling");
        _fboUniformCache.boresight = _fboProgramObject->uniformLocation("boresight");
        _fboUniformCache.radius = _fboProgramObject->uniformLocation("_radius");
        _fboUniformCache.segments = _fboProgramObject->uniformLocation("_segments");
    }

    if (_colorTextureDirty) {
        loadColorTexture();
        _colorTextureDirty = false;
    }

    if (_heightMapTextureDirty) {
        loadHeightTexture();
        _heightMapTextureDirty = false;
    }

    _projectionComponent.update();

    const double time = data.time.j2000Seconds();

    // Only project new images if time changed since last update.
    if (time != _time) {
        if (openspace::ImageSequencer::ref().isReady()) {
            openspace::ImageSequencer::ref().updateSequencer(time);
            if (_projectionComponent.doesPerformProjection()) {
                _capture = openspace::ImageSequencer::ref().getImagePaths(
                    _imageTimes,
                    _projectionComponent.projecteeId(),
                    _projectionComponent.instrumentId(),
                    _time
                );
            }
        }
        _time = time;
    }

    _stateMatrix = data.modelTransform.rotation;
}

void RenderablePlanetProjection::loadColorTexture() {
    using ghoul::opengl::Texture;
    std::string selectedPath = _colorTexturePaths.option().description;

    // We delete the texture first in order to free up the memory, which could otherwise
    // run out in the case of two large textures
    _baseTexture = nullptr;
    if (selectedPath != NoImageText) {
        _baseTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(selectedPath)
        );
        if (_baseTexture) {
            ghoul::opengl::convertTextureFormat(*_baseTexture, Texture::Format::RGB);
            _baseTexture->uploadTexture();
            _baseTexture->setWrapping(
                { Texture::WrappingMode::Repeat, Texture::WrappingMode::MirroredRepeat}
            );
            _baseTexture->setFilter(Texture::FilterMode::LinearMipMap);
        }
    }
}

void RenderablePlanetProjection::loadHeightTexture() {
    using ghoul::opengl::Texture;
    std::string selectedPath = _heightMapTexturePaths.option().description;

    // We delete the texture first in order to free up the memory, which could otherwise
    // run out in the case of two large textures
    _heightMapTexture = nullptr;
    if (selectedPath != NoImageText) {
        _heightMapTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(selectedPath)
        );
        if (_heightMapTexture) {
            ghoul::opengl::convertTextureFormat(*_heightMapTexture, Texture::Format::RGB);
            _heightMapTexture->uploadTexture();
            _heightMapTexture->setFilter(Texture::FilterMode::Linear);
        }
    }
}

}  // namespace openspace
