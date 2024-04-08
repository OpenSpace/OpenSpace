/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <modules/spacecraftinstruments/util/image.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/sphere.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr std::array<const char*, 12> MainUniformNames = {
        "sun_pos", "modelTransform", "modelViewProjectionTransform", "hasBaseMap",
        "hasHeightMap", "heightExaggeration", "meridianShift", "ambientBrightness",
        "projectionFading", "baseTexture", "projectionTexture", "heightTexture"
    };

    constexpr std::array<const char*, 6> FboUniformNames = {
        "projectionTexture", "ProjectorMatrix", "ModelTransform",
        "boresight", "radius", "segments"
    };

    constexpr std::string_view NoImageText = "No Image";

    constexpr openspace::properties::Property::PropertyInfo ColorTexturePathsInfo = {
        "ColorTexturePaths",
        "Color Texture",
        "The texture path selected in this property is used as the base texture that is "
        "applied to the planet prior to any image projections. This menu always contains "
        "an empty option for not using a color map. If this value is specified in an "
        "asset, the last texture is used",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AddColorTextureInfo = {
        "AddColorTexture",
        "Add Color Base Texture",
        "Adds a new base color texture to the list of selectable base maps used prior to "
        "any image projection",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HeightTexturePathsInfo = {
        "HeightTexturePaths",
        "Heightmap Texture",
        "The texture path selected in this property is used as the height map on the "
        "planet. This menu always contains an empty option for not using a heightmap. If "
        "this value is specified in an asset, the last texture is used",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AddHeightTextureInfo = {
        "AddHeightTexture",
        "Add Heightmap Texture",
        "Adds a new height map texture to the list of selectable height maps used",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HeightExaggerationInfo = {
        "HeightExaggeration",
        "Height Exaggeration",
        "This value determines the level of height exaggeration that is applied to a "
        "potential height field. A value of '0' inhibits the height field, whereas a "
        "value of '1' uses the measured height field",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MeridianShiftInfo = {
        "MeridianShift",
        "Meridian Shift",
        "If this value is enabled, a shift of the meridian by 180 degrees is performed. "
        "This is a fix especially for Pluto height maps, where the definition of the "
        "meridian has changed through the New Horizons mission and this requires this "
        "shift",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AmbientBrightnessInfo = {
        "AmbientBrightness",
        "Ambient Brightness",
        "This value determines the ambient brightness of the dark side of the planet",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MaxProjectionsPerFrameInfo = {
        "MaxProjectionsPerFrame",
        "Max Projections Per Frame",
        "The maximum number of image projections to perform per frame. "
        "Useful to avoid freezing the system for large delta times",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ProjectionsInBufferInfo = {
        "ProjectionsInBuffer",
        "Projections In Buffer",
        "(Read only) The number of images that are currently waiting to be projected",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ClearProjectionBufferInfo = {
        "ClearProjectionBuffer",
        "Clear Projection Buffer",
        "Remove all pending projections from the buffer",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Radius",
        "Radius",
        "This value specifies the radius of this sphere in meters",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "This value specifies the number of segments that this sphere is split into",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderablePlanetProjection)]] Parameters {
        // Contains information about projecting onto this planet
        ghoul::Dictionary projection
            [[codegen::reference("spacecraftinstruments_projectioncomponent")]];

        // [[codegen::verbatim(ColorTexturePathsInfo.description)]]
        std::vector<std::string> colorTexturePaths;

        // [[codegen::verbatim(HeightTexturePathsInfo.description)]]
        std::vector<std::string> heightTexturePaths;

        // [[codegen::verbatim(HeightExaggerationInfo.description)]]
        std::optional<float> heightExaggeration;

        // [[codegen::verbatim(MeridianShiftInfo.description)]]
        std::optional<bool> meridianShift;

        // [[codegen::verbatim(AmbientBrightnessInfo.description)]]
        std::optional<double> ambientBrightness;

        // [[codegen::verbatim(MaxProjectionsPerFrameInfo.description)]]
        std::optional<int> maxProjectionsPerFrame;

        // [[codegen::verbatim(RadiusInfo.description)]]
        std::variant<float, glm::vec3> radius;

        // [[codegen::verbatim(SegmentsInfo.description)]]
        int segments;
    };
#include "renderableplanetprojection_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePlanetProjection::Documentation() {
    return codegen::doc<Parameters>("spacecraftinstruments_renderableplanetprojection");
}

RenderablePlanetProjection::RenderablePlanetProjection(const ghoul::Dictionary& dict)
    : Renderable(dict)
    , _colorTexturePaths(ColorTexturePathsInfo)
    , _addColorTexturePath(AddColorTextureInfo)
    , _heightMapTexturePaths(HeightTexturePathsInfo)
    , _addHeightMapTexturePath(AddHeightTextureInfo)
    , _heightExaggeration(HeightExaggerationInfo, 1.f, 0.f, 1e6f, 1.f)
    , _meridianShift(MeridianShiftInfo, false)
    , _ambientBrightness(AmbientBrightnessInfo, 0.075f, 0.f, 1.f)
    , _maxProjectionsPerFrame(MaxProjectionsPerFrameInfo, 3, 1, 64)
    , _projectionsInBuffer(ProjectionsInBufferInfo, 0, 1, 32)
    , _clearProjectionBuffer(ClearProjectionBufferInfo)
    , _radius(RadiusInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(std::pow(10.f, 20.f)))
    , _segments(SegmentsInfo, 20, 1, 5000)
    , _sphere(nullptr)
{
    const Parameters p = codegen::bake<Parameters>(dict);

    _projectionComponent.initialize(identifier(), p.projection);

    _colorTexturePaths.addOption(0, std::string(NoImageText));
    _colorTexturePaths.onChange([this](){ _colorTextureDirty = true; });
    addProperty(_colorTexturePaths);

    for (const std::string& t : p.colorTexturePaths) {
        _colorTexturePaths.addOption(
            static_cast<int>(_colorTexturePaths.options().size()),
            t
        );
    }
    _colorTexturePaths = static_cast<int>(_colorTexturePaths.options().size() - 1);

    _addColorTexturePath.onChange([this]() {
        if (_addColorTexturePath.value().empty()) {
            return;
        }
        _colorTexturePaths.addOption(
            // as we started with 0, this works
            static_cast<int>(_colorTexturePaths.options().size()),
            _addColorTexturePath.value()
        );

        _colorTexturePaths = static_cast<int>(_colorTexturePaths.options().size() - 1);
        _addColorTexturePath = "";
    });
    addProperty(_addColorTexturePath);

    _heightMapTexturePaths.addOption(0, std::string(NoImageText));
    _heightMapTexturePaths.onChange([this]() { _heightMapTextureDirty = true; });
    addProperty(_heightMapTexturePaths);

    for (const std::string& t : p.heightTexturePaths) {
        _heightMapTexturePaths.addOption(
            static_cast<int>(_heightMapTexturePaths.options().size()),
            t
        );
    }
    _heightMapTexturePaths = static_cast<int>(
        _heightMapTexturePaths.options().size() - 1
    );
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

    _meridianShift = p.meridianShift.value_or(_meridianShift);
    addProperty(_meridianShift);

    addPropertySubOwner(_projectionComponent);

    _heightExaggeration.setExponent(3.f);
    _heightExaggeration = p.heightExaggeration.value_or(_heightExaggeration);
    addProperty(_heightExaggeration);

    _maxProjectionsPerFrame = p.maxProjectionsPerFrame.value_or(_maxProjectionsPerFrame);
    addProperty(_maxProjectionsPerFrame);

    addProperty(_ambientBrightness);
    addProperty(_projectionsInBuffer);

    _clearProjectionBuffer.onChange([this]() {
        _imageTimes.clear();
        _projectionsInBuffer = static_cast<int>(_imageTimes.size());
    });

    addProperty(_clearProjectionBuffer);

    if (std::holds_alternative<float>(p.radius)) {
        const float r = std::get<float>(p.radius);
        _radius = glm::dvec3(r, r, r);
    }
    else {
        _radius = std::get<glm::vec3>(p.radius);
    }
    setBoundingSphere(glm::compMax(_radius.value()));
    _radius.onChange([this]() { createSphere(); });
    addProperty(_radius);

    _segments = p.segments;
    _segments.onChange([this]() { createSphere(); });
    addProperty(_segments);
}

RenderablePlanetProjection::~RenderablePlanetProjection() {}

void RenderablePlanetProjection::initializeGL() {
    _programObject = SpacecraftInstrumentsModule::ProgramObjectManager.request(
        "ProjectiveProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "ProjectiveProgram",
                absPath(
                    "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderablePlanet_vs.glsl"
                ),
                absPath(
                    "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderablePlanet_fs.glsl"
                )
            );
        }
    );

    ghoul::opengl::updateUniformLocations(
        *_programObject,
        _mainUniformCache,
        MainUniformNames
    );

    _fboProgramObject = SpacecraftInstrumentsModule::ProgramObjectManager.request(
        "FBOPassProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return ghoul::opengl::ProgramObject::Build(
                "FBOPassProgram",
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
    createSphere();
    const glm::vec3 radius = _radius;
    setBoundingSphere(std::max(std::max(radius[0], radius[1]), radius[2]));

    // SCREEN-QUAD
    constexpr std::array<GLfloat, 12> VertexData = {
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
    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nullptr);
    glBindVertexArray(0);
}

void RenderablePlanetProjection::deinitializeGL() {
    _sphere = nullptr;

    _projectionComponent.deinitialize();
    _baseTexture = nullptr;

    glDeleteVertexArrays(1, &_quad);
    glDeleteBuffers(1, &_vertexPositionBuffer);

    SpacecraftInstrumentsModule::ProgramObjectManager.release(
        "ProjectiveProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _programObject = nullptr;

    SpacecraftInstrumentsModule::ProgramObjectManager.release("FBOPassProgram");
    _fboProgramObject = nullptr;
}

bool RenderablePlanetProjection::isReady() const {
    return _programObject && _projectionComponent.isReady();
}

void RenderablePlanetProjection::imageProjectGPU(
                                          const ghoul::opengl::Texture& projectionTexture,
                                                         const glm::mat4& projectorMatrix)
{
    _projectionComponent.imageProjectBegin();

    _fboProgramObject->activate();

    ghoul::opengl::TextureUnit unitFbo;
    unitFbo.activate();
    projectionTexture.bind();
    _fboProgramObject->setUniform(_fboUniformCache.projectionTexture, unitFbo);

    _fboProgramObject->setUniform(_fboUniformCache.projectorMatrix, projectorMatrix);
    _fboProgramObject->setUniform(_fboUniformCache.modelTransform, _transform);
    _fboProgramObject->setUniform(_fboUniformCache.boresight, _boresight);
    _fboProgramObject->setUniform(_fboUniformCache.radius, _radius);
    _fboProgramObject->setUniform(_fboUniformCache.segments, _segments);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    _fboProgramObject->deactivate();

    _projectionComponent.imageProjectEnd();
    glBindVertexArray(0);
}

glm::mat4 RenderablePlanetProjection::attitudeParameters(double time, const glm::vec3& up)
{
    // precomputations for shader
    const glm::dmat3 instrumentMatrix = SpiceManager::ref().positionTransformMatrix(
        _projectionComponent.instrumentId(),
        "GALACTIC",
        time
    );

    const SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(
        _projectionComponent.instrumentId()
    );

    double lightTime = 0.0;
    const glm::dvec3 p = SpiceManager::ref().targetPosition(
        _projectionComponent.projectorId(),
        _projectionComponent.projecteeId(),
        "GALACTIC",
        _projectionComponent.aberration(),
        time,
        lightTime
    ) * 1000.0;

    const double distance = glm::length(p);
    const double radius = boundingSphere();
    return _projectionComponent.computeProjectorMatrix(
        p,
        res.boresightVector,
        up,
        instrumentMatrix,
        _projectionComponent.fieldOfViewY(),
        _projectionComponent.aspectRatio(),
        static_cast<float>(distance - radius),
        static_cast<float>(distance + radius),
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

    const glm::vec3 up = data.camera.lookUpVectorCameraSpace();
    if (_projectionComponent.doesPerformProjection()) {
        int nProjections = 0;
        for (const Image& img : _imageTimes) {
            if (nProjections >= _maxProjectionsPerFrame) {
                break;
            }
            try {
                const glm::mat4 projMatrix = attitudeParameters(img.timeRange.start, up);
                const std::shared_ptr<ghoul::opengl::Texture> t =
                    _projectionComponent.loadProjectionTexture(img.path);
                imageProjectGPU(*t, projMatrix);
                ++nProjections;
            }
            catch (const SpiceManager::SpiceException& e) {
                LERRORC(e.component, e.what());
            }
        }
        _imageTimes.erase(_imageTimes.begin(), _imageTimes.begin() + nProjections);
        _projectionsInBuffer = static_cast<int>(_imageTimes.size());
    }
    try {
        attitudeParameters(data.time.j2000Seconds(), up);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERRORC(e.component, e.what());
    }

    double lt = 0.0;
    const glm::dvec3 sunPos = SpiceManager::ref().targetPosition(
        "SUN",
        _projectionComponent.projecteeId(),
        "GALACTIC",
        {},
        data.time.j2000Seconds(),
        lt
    );

    // Main renderpass
    _programObject->activate();
    _programObject->setUniform(_mainUniformCache.sunPos, static_cast<glm::vec3>(sunPos));

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelTransform = calcModelTransform(data);

    _programObject->setUniform(
        _mainUniformCache.modelTransform,
        glm::mat4(modelTransform)
    );
    _programObject->setUniform(
        _mainUniformCache.modelViewProjectionTransform,
        glm::mat4(calcModelViewProjectionTransform(data, modelTransform))
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

    ghoul::opengl::TextureUnit baseUnit;
    if (_baseTexture) {
        baseUnit.activate();
        _baseTexture->bind();
        _programObject->setUniform(_mainUniformCache.baseTexture, baseUnit);
    }

    ghoul::opengl::TextureUnit projectionUnit;
    projectionUnit.activate();
    _projectionComponent.projectionTexture().bind();
    _programObject->setUniform(_mainUniformCache.projectionTexture, projectionUnit);

    ghoul::opengl::TextureUnit heightUnit;
    if (_heightMapTexture) {
        heightUnit.activate();
        _heightMapTexture->bind();
        _programObject->setUniform(_mainUniformCache.heightTexture, heightUnit);
    }

    _sphere->render();
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
    if (time > integrateFromTime && ImageSequencer::ref().isReady() &&
        _projectionComponent.doesPerformProjection())
    {
        std::vector<Image> newImageTimes= ImageSequencer::ref().imagePaths(
            _projectionComponent.projecteeId(),
            _projectionComponent.instrumentId(),
            time,
            integrateFromTime
        );

        if (!newImageTimes.empty()) {
            const double firstNewImage = newImageTimes[0].timeRange.end;
            // Make sure images are always projected in the correct order
            // (Remove buffered images with a later timestamp)
            const auto& it = std::find_if(
                _imageTimes.begin(),
                _imageTimes.end(),
                [firstNewImage](const Image& image) {
                    return image.timeRange.end > firstNewImage;
                }
            );
            if (it != _imageTimes.end()) {
                _imageTimes.erase(it, _imageTimes.end());
            }

            // Now, insert the new images to the buffer
            _imageTimes.insert(
                _imageTimes.end(),
                newImageTimes.begin(),
                newImageTimes.end()
            );
            _projectionsInBuffer = static_cast<int>(_imageTimes.size());
        }
    }

    _transform = glm::mat4(data.modelTransform.rotation);
}

void RenderablePlanetProjection::loadColorTexture() {
    using ghoul::opengl::Texture;
    const std::string selectedPath = _colorTexturePaths.option().description;

    // We delete the texture first in order to free up the memory, which could otherwise
    // run out in the case of two large textures
    _baseTexture = nullptr;
    if (selectedPath != NoImageText) {
        _baseTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(selectedPath),
            2
        );
        if (_baseTexture) {
            ghoul::opengl::convertTextureFormat(*_baseTexture, Texture::Format::RGB);
            _baseTexture->uploadTexture();
            _baseTexture->setWrapping(
                { Texture::WrappingMode::Repeat, Texture::WrappingMode::MirroredRepeat }
            );
            _baseTexture->setFilter(Texture::FilterMode::LinearMipMap);
        }
    }
}

void RenderablePlanetProjection::loadHeightTexture() {
    using ghoul::opengl::Texture;
    const std::string selectedPath = _heightMapTexturePaths.option().description;

    // We delete the texture first in order to free up the memory, which could otherwise
    // run out in the case of two large textures
    _heightMapTexture = nullptr;
    if (selectedPath != NoImageText) {
        _heightMapTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(selectedPath),
            2
        );
        if (_heightMapTexture) {
            ghoul::opengl::convertTextureFormat(*_heightMapTexture, Texture::Format::RGB);
            _heightMapTexture->uploadTexture();
            _heightMapTexture->setFilter(Texture::FilterMode::Linear);
        }
    }
}

void RenderablePlanetProjection::createSphere() {
    _sphere = std::make_unique<Sphere>(_radius, _segments);
    _sphere->initialize();
}

}  // namespace openspace
