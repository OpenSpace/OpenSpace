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

#include <modules/space/rendering/planetgeometry.h>
#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* _loggerCat = "RenderablePlanetProjection";
    constexpr const char* ProjectiveProgramName = "ProjectiveProgram";
    constexpr const char* FBOPassProgramName = "FBOPassProgram";

    constexpr const std::array<const char*, 12> MainUniformNames = {
        "sun_pos", "modelTransform", "modelViewProjectionTransform", "_hasBaseMap",
        "_hasHeightMap", "_heightExaggeration", "_meridianShift", "_ambientBrightness",
        "_projectionFading", "baseTexture", "projectionTexture", "heightTexture"
    };

    constexpr const std::array<const char*, 7> FboUniformNames = {
        "projectionTexture", "ProjectorMatrix", "ModelTransform", "_scaling",
        "boresight", "_radius", "_segments"
    };

    constexpr const char* KeyGeometry = "Geometry";
    constexpr const char* KeyProjection = "Projection";
    constexpr const char* KeyRadius = "Geometry.Radius";
    constexpr const char* _mainFrame = "GALACTIC";

    constexpr const char* NoImageText = "No Image";

    constexpr openspace::properties::Property::PropertyInfo ColorTexturePathsInfo = {
        "ColorTexturePaths",
        "Color Texture",
        "The texture path selected in this property is used as the base texture that is "
        "applied to the planet prior to any image projections. This menu always contains "
        "an empty option for not using a color map. If this value is specified in an "
        "asset, the last texture is used."
    };

    constexpr openspace::properties::Property::PropertyInfo AddColorTextureInfo = {
        "AddColorTexture",
        "Add Color Base Texture",
        "Adds a new base color texture to the list of selectable base maps used prior to "
        "any image projection."
    };

    constexpr openspace::properties::Property::PropertyInfo HeightTexturePathsInfo = {
        "HeightTexturePaths",
        "Heightmap Texture",
        "The texture path selected in this property is used as the height map on the "
        "planet. This menu always contains an empty option for not using a heightmap. If "
        "this value is specified in an asset, the last texture is used."
    };

    constexpr openspace::properties::Property::PropertyInfo AddHeightTextureInfo = {
        "AddHeightTexture",
        "Add Heightmap Texture",
        "Adds a new height map texture to the list of selectable height maps used."
    };

    constexpr openspace::properties::Property::PropertyInfo HeightExaggerationInfo = {
        "HeightExaggeration",
        "Height Exaggeration",
        "This value determines the level of height exaggeration that is applied to a "
        "potential height field. A value of '0' inhibits the height field, whereas a "
        "value of '1' uses the measured height field."
    };

    constexpr openspace::properties::Property::PropertyInfo MeridianShiftInfo = {
        "MeridianShift",
        "Meridian Shift",
        "If this value is enabled, a shift of the meridian by 180 degrees is performed. "
        "This is a fix especially for Pluto height maps, where the definition of the "
        "meridian has changed through the New Horizons mission and this requires this "
        "shift."
    };

    constexpr openspace::properties::Property::PropertyInfo AmbientBrightnessInfo = {
        "AmbientBrightness",
        "Ambient Brightness",
        "This value determines the ambient brightness of the dark side of the planet."
    };

    constexpr openspace::properties::Property::PropertyInfo MaxProjectionsPerFrameInfo = {
        "MaxProjectionsPerFrame",
        "Max Projections Per Frame",
        "The maximum number of image projections to perform per frame. "
        "Useful to avoid freezing the system for large delta times."
    };

    constexpr openspace::properties::Property::PropertyInfo ProjectionsInBufferInfo = {
        "ProjectionsInBuffer",
        "Projections In Buffer",
        "(Read only) The number of images that are currently waiting to be projected"
    };

    constexpr openspace::properties::Property::PropertyInfo ClearProjectionBufferInfo = {
        "ClearProjectionBuffer",
        "Clear Projection Buffer",
        "Remove all pending projections from the buffer"
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
            },
            {
                MaxProjectionsPerFrameInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                MaxProjectionsPerFrameInfo.description
            }
        }
    };
}

RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dict)
    : Renderable(dict)
    , _colorTexturePaths(ColorTexturePathsInfo)
    , _addColorTexturePath(AddColorTextureInfo)
    , _heightMapTexturePaths(HeightTexturePathsInfo)
    , _addHeightMapTexturePath(AddHeightTextureInfo)
    , _heightExaggeration(HeightExaggerationInfo, 1.f, 0.f, 1e6f, 1.f, 3.f)
    , _meridianShift(MeridianShiftInfo, false)
    , _ambientBrightness(AmbientBrightnessInfo, 0.075f, 0.f, 1.f)
    , _maxProjectionsPerFrame(MaxProjectionsPerFrameInfo, 1, 1, 64)
    , _projectionsInBuffer(ProjectionsInBufferInfo, 0, 1, 32)
    , _clearProjectionBuffer(ClearProjectionBufferInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dict,
        "RenderablePlanetProjection"
    );

    ghoul::Dictionary geometryDictionary = dict.value<ghoul::Dictionary>(KeyGeometry);
    _geometry = planetgeometry::PlanetGeometry::createFromDictionary(geometryDictionary);

    _projectionComponent.initialize(
        identifier(),
        dict.value<ghoul::Dictionary>(KeyProjection)
    );

    _colorTexturePaths.addOption(0, NoImageText);
    _colorTexturePaths.onChange([this](){ _colorTextureDirty = true; });
    addProperty(_colorTexturePaths);

    if (dict.hasKeyAndValue<ghoul::Dictionary>(ColorTexturePathsInfo.identifier)) {
        const ghoul::Dictionary& value = dict.value<ghoul::Dictionary>(
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
    _heightMapTexturePaths.onChange([this]() { _heightMapTextureDirty = true; });
    addProperty(_heightMapTexturePaths);


    if (dict.hasKeyAndValue<ghoul::Dictionary>(HeightTexturePathsInfo.identifier)) {
        const ghoul::Dictionary& value = dict.value<ghoul::Dictionary>(
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

    if (dict.hasKey(MaxProjectionsPerFrameInfo.identifier)) {
        _maxProjectionsPerFrame = static_cast<int>(
            dict.value<double>(MaxProjectionsPerFrameInfo.identifier)
        );
    }

    addProperty(_heightExaggeration);
    addProperty(_meridianShift);
    addProperty(_ambientBrightness);

    addProperty(_maxProjectionsPerFrame);
    addProperty(_projectionsInBuffer);

    _clearProjectionBuffer.onChange([this]() {
        _imageTimes.clear();
        _projectionsInBuffer = static_cast<int>(_imageTimes.size());
    });

    addProperty(_clearProjectionBuffer);
}

RenderablePlanetProjection::~RenderablePlanetProjection() {} // NOLINT

void RenderablePlanetProjection::initializeGL() {
    _programObject =
        SpacecraftInstrumentsModule::ProgramObjectManager.request(
            ProjectiveProgramName,
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine.buildRenderProgram(
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

    ghoul::opengl::updateUniformLocations(
        *_programObject,
        _mainUniformCache,
        MainUniformNames
    );

    _fboProgramObject =
        SpacecraftInstrumentsModule::ProgramObjectManager.request(
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

    ghoul::opengl::updateUniformLocations(
        *_fboProgramObject,
        _fboUniformCache,
        FboUniformNames
    );

    loadColorTexture();
    loadHeightTexture();
    _projectionComponent.initializeGL();
    _geometry->initialize();
    setBoundingSphere(_geometry->boundingSphere());

    //completeSuccess &= auxiliaryRendertarget();
    // SCREEN-QUAD
    const GLfloat vertexData[] = {
        -1.f, -1.f,
         1.f,  1.f,
        -1.f,  1.f,
        -1.f, -1.f,
         1.f, -1.f,
         1.f,  1.f,
    };

    glGenVertexArrays(1, &_quad);
    glBindVertexArray(_quad);
    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 2, nullptr);
    // glEnableVertexAttribArray(1);
    // glVertexAttribPointer(
    //     1,
    //     2,
    //     GL_FLOAT,
    //     GL_FALSE,
    //     sizeof(GLfloat) * 2,
    //     reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    // );

    glBindVertexArray(0);
}

void RenderablePlanetProjection::deinitializeGL() {
    _projectionComponent.deinitialize();
    _baseTexture = nullptr;
    _geometry = nullptr;

    glDeleteVertexArrays(1, &_quad);
    glDeleteBuffers(1, &_vertexPositionBuffer);

    SpacecraftInstrumentsModule::ProgramObjectManager.release(
        ProjectiveProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _programObject = nullptr;

    SpacecraftInstrumentsModule::ProgramObjectManager.release(
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
        _imageTimes.clear();
        _projectionsInBuffer = static_cast<int>(_imageTimes.size());
    }

    if (_projectionComponent.needsMipMapGeneration()) {
        _projectionComponent.generateMipMap();
    }

    _camScaling = glm::vec2(1.f, 0.f); // Unit scaling
    _up = data.camera.lookUpVectorCameraSpace();

    if (_projectionComponent.doesPerformProjection()) {
        int nPerformedProjections = 0;
        for (const Image& img : _imageTimes) {
            if (nPerformedProjections >= _maxProjectionsPerFrame) {
                break;
            }
            RenderablePlanetProjection::attitudeParameters(img.timeRange.start);
            imageProjectGPU(_projectionComponent.loadProjectionTexture(img.path));
            ++nPerformedProjections;
        }
        _imageTimes.erase(
            _imageTimes.begin(),
            _imageTimes.begin() + nPerformedProjections
        );
        _projectionsInBuffer = static_cast<int>(_imageTimes.size());

    }
    attitudeParameters(data.time.j2000Seconds());

    double  lt;
    glm::dvec3 p = SpiceManager::ref().targetPosition(
        "SUN",
        _projectionComponent.projecteeId(),
        "GALACTIC",
        {},
        data.time.j2000Seconds(),
        lt
    );
    psc sun_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);

    // Main renderpass
    _programObject->activate();
    _programObject->setUniform(_mainUniformCache.sunPos, sun_pos.vec3());
    //_program->setUniform("ViewProjection" ,  data.camera.viewProjectionMatrix());
    //_program->setUniform("ModelTransform" , _transform);

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
        
        ghoul::opengl::updateUniformLocations(
            *_programObject,
            _mainUniformCache,
            MainUniformNames
        );
    }

    if (_fboProgramObject->isDirty()) {
        _fboProgramObject->rebuildFromFile();

        ghoul::opengl::updateUniformLocations(
            *_fboProgramObject,
            _fboUniformCache,
            FboUniformNames
        );
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
    const double integrateFromTime = data.previousFrameTime.j2000Seconds();

    // Only project new images if time changed since last update.
    if (time > integrateFromTime) {
        if (openspace::ImageSequencer::ref().isReady()) {
            if (_projectionComponent.doesPerformProjection()) {
                std::vector<Image> newImageTimes;
                openspace::ImageSequencer::ref().imagePaths(
                    newImageTimes,
                    _projectionComponent.projecteeId(),
                    _projectionComponent.instrumentId(),
                    time,
                    integrateFromTime
                );

                if (!newImageTimes.empty()) {
                    double firstNewImage = newImageTimes[0].timeRange.end;
                    // Make sure images are always projected in the correct order
                    // (Remove buffered images with a later timestamp)
                    clearProjectionBufferAfterTime(firstNewImage);

                    // Now, insert the new images to the buffer
                    insertImageProjections(newImageTimes);
                }
            }
        }
    }

    _stateMatrix = data.modelTransform.rotation;
}

void RenderablePlanetProjection::clearProjectionBufferAfterTime(double time) {
    const auto& it = std::find_if(
        _imageTimes.begin(),
        _imageTimes.end(),
        [time](const Image& image) {
            return image.timeRange.end > time;
        }
    );
    if (it != _imageTimes.end()) {
        _imageTimes.erase(it, _imageTimes.end());
    }
}

void RenderablePlanetProjection::insertImageProjections(const std::vector<Image>& images)
{
    _imageTimes.insert(_imageTimes.end(),
        images.begin(),
        images.end()
    );
    _projectionsInBuffer = static_cast<int>(_imageTimes.size());
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
