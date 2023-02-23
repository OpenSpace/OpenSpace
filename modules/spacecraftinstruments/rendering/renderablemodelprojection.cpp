/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/spacecraftinstruments/rendering/renderablemodelprojection.h>

#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/model/modelgeometry.h>
#include <ghoul/io/model/modelreader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr std::array<const char*, 7> MainUniformNames = {
        "performShading", "directionToSunViewSpace", "modelViewTransform",
        "projectionTransform", "projectionFading", "baseTexture", "projectionTexture"
    };

    constexpr std::array<const char*, 6> FboUniformNames = {
        "projectionTexture", "depthTexture", "needShadowMap", "ProjectorMatrix",
        "ModelTransform", "boresight"
    };

    constexpr std::array<const char*, 2> DepthFboUniformNames = {
        "ProjectorMatrix", "ModelTransform"
    };

    constexpr openspace::properties::Property::PropertyInfo PerformShadingInfo = {
        "PerformShading",
        "Perform Shading",
        "If this value is enabled, the model will be shaded based on the relative "
        "location to the Sun. If this value is disabled, shading is disabled and the "
        "entire model is rendered brightly"
    };

    struct [[codegen::Dictionary(RenderableModelProjection)]] Parameters {
        // The file or files that should be loaded in this RenderableModel. The file can
        // contain filesystem tokens or can be specified relatively to the
        // location of the .asset file.
        // This specifies the model that is rendered by the Renderable.
        std::filesystem::path geometryFile;

        // Contains information about projecting onto this planet.
        ghoul::Dictionary projection
            [[codegen::reference("spacecraftinstruments_projectioncomponent")]];

        // [[codegen::verbatim(PerformShadingInfo.description)]]
        std::optional<bool> performShading;

        // The radius of the bounding sphere of this object. This has to be a
        // radius that is larger than anything that is rendered by it. It has to
        // be at least as big as the convex hull of the object. The default value
        // is 10e9 meters.
        std::optional<double> boundingSphereRadius;
    };
#include "renderablemodelprojection_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableModelProjection::Documentation() {
    return codegen::doc<Parameters>("spacecraftinstruments_renderablemodelprojection");
}

RenderableModelProjection::RenderableModelProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _performShading(PerformShadingInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::filesystem::path file = absPath(p.geometryFile.string());
    _geometry = ghoul::io::ModelReader::ref().loadModel(
        file.string(),
        ghoul::io::ModelReader::ForceRenderInvisible::No,
        ghoul::io::ModelReader::NotifyInvisibleDropped::Yes
    );

    addPropertySubOwner(_projectionComponent);
    _projectionComponent.initialize(identifier(), p.projection);

    double boundingSphereRadius = p.boundingSphereRadius.value_or(1.0e9);
    setBoundingSphere(boundingSphereRadius);

    _performShading = p.performShading.value_or(_performShading);
    addProperty(_performShading);
}

RenderableModelProjection::~RenderableModelProjection() {}

bool RenderableModelProjection::isReady() const {
    return (_programObject != nullptr) && _projectionComponent.isReady();
}

void RenderableModelProjection::initializeGL() {
    _programObject = global::renderEngine->buildRenderProgram(
        "ModelShader",
        absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderableModel_vs.glsl"),
        absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderableModel_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(
        *_programObject,
        _mainUniformCache,
        MainUniformNames
    );

    _fboProgramObject = ghoul::opengl::ProgramObject::Build(
        "ProjectionPass",
        absPath(
            "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderableModelProjection_vs.glsl"
        ),
        absPath(
            "${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderableModelProjection_fs.glsl"
        )
    );

    ghoul::opengl::updateUniformLocations(
        *_fboProgramObject,
        _fboUniformCache,
        FboUniformNames
    );

    _depthFboProgramObject = ghoul::opengl::ProgramObject::Build(
        "DepthPass",
        absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderableModelDepth_vs.glsl"),
        absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/renderableModelDepth_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(
        *_depthFboProgramObject,
        _depthFboUniformCache,
        DepthFboUniformNames
    );

    _projectionComponent.initializeGL();

    double bs = boundingSphere();
    _geometry->initialize();
    setBoundingSphere(bs); // ignore bounding sphere set by geometry.
}

void RenderableModelProjection::deinitializeGL() {
    if (_geometry) {
        _geometry->deinitialize();
    }

    _geometry = nullptr;

    _projectionComponent.deinitialize();

    global::renderEngine->removeRenderProgram(_programObject.get());
    _programObject = nullptr;
}

ghoul::opengl::Texture& RenderableModelProjection::baseTexture() const {
    return _projectionComponent.projectionTexture();
}

void RenderableModelProjection::render(const RenderData& data, RendererTasks&) {
    if (_projectionComponent.needsClearProjection()) {
        _projectionComponent.clearAllProjections();
    }

    glm::vec3 up = data.camera.lookUpVectorCameraSpace();

    if (_shouldCapture && _projectionComponent.doesPerformProjection()) {
        for (const Image& i : _imageTimes) {
            try {
                glm::mat4 projectorMatrix = attitudeParameters(i.timeRange.start, up);
                std::shared_ptr<ghoul::opengl::Texture> t =
                    _projectionComponent.loadProjectionTexture(i.path, i.isPlaceholder);
                imageProjectGPU(*t, projectorMatrix);
            }
            catch (const SpiceManager::SpiceException& e) {
                LERRORC(e.component, e.what());
            }
        }
        _shouldCapture = false;
    }

    _programObject->activate();

    try {
        attitudeParameters(data.time.j2000Seconds(), up);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERRORC(e.component, e.what());
    }
    _imageTimes.clear();

    // Calculate variables to be used as uniform variables in shader
    const glm::vec3 bodyPos = data.modelTransform.translation;

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 transform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));
    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * transform;
    const glm::vec3 directionToSun = glm::normalize(_sunPosition - bodyPos);
    const glm::vec3 directionToSunViewSpace = glm::normalize(
        glm::mat3(data.camera.combinedViewMatrix()) * directionToSun
    );

    _programObject->setUniform(_mainUniformCache.performShading, _performShading);
    _programObject->setUniform(
        _mainUniformCache.directionToSunViewSpace,
        directionToSunViewSpace
    );
    _programObject->setUniform(
        _mainUniformCache.modelViewTransform,
        glm::mat4(modelViewTransform)
    );
    _programObject->setUniform(
        _mainUniformCache.projectionTransform,
        data.camera.projectionMatrix()
    );
    _programObject->setUniform(
        _mainUniformCache.projectionFading,
        _projectionComponent.projectionFading()
    );

    ghoul::opengl::TextureUnit baseUnit;
    baseUnit.activate();
    _programObject->setUniform(_mainUniformCache.baseTexture, baseUnit);

    ghoul::opengl::TextureUnit projectionUnit;
    projectionUnit.activate();
    _projectionComponent.projectionTexture().bind();
    _programObject->setUniform(_mainUniformCache.projectionTexture, projectionUnit);

    _geometry->render(*_programObject, false);

    _programObject->deactivate();
}

void RenderableModelProjection::update(const UpdateData& data) {
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

    _projectionComponent.update();

    if (_depthFboProgramObject->isDirty()) {
        _depthFboProgramObject->rebuildFromFile();

        ghoul::opengl::updateUniformLocations(
            *_depthFboProgramObject,
            _depthFboUniformCache,
            DepthFboUniformNames
        );
    }

    const double time = data.time.j2000Seconds();
    const double integrateFromTime = data.previousFrameTime.j2000Seconds();

    // Only project new images if time changed since last update.
    if (time > integrateFromTime && ImageSequencer::ref().isReady() &&
        _projectionComponent.doesPerformProjection())
    {
        _imageTimes = ImageSequencer::ref().imagePaths(
            _projectionComponent.projecteeId(),
            _projectionComponent.instrumentId(),
            time,
            integrateFromTime
        );
        _shouldCapture = !_imageTimes.empty();
    }

    glm::dmat3 stateMatrix = data.modelTransform.rotation;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            _transform[i][j] = static_cast<float>(stateMatrix[i][j]);
        }
    }

    SceneGraphNode* sun = global::renderEngine->scene()->sceneGraphNode("Sun");
    if (sun) {
        _sunPosition = sun->worldPosition() - data.modelTransform.translation;
    }
    else {
        // If the Sun node doesn't exist, we assume that the light source is in the origin
        _sunPosition = -data.modelTransform.translation;
    }
}

void RenderableModelProjection::imageProjectGPU(
                                          const ghoul::opengl::Texture& projectionTexture,
                                                         const glm::mat4& projectorMatrix)
{
    if (_projectionComponent.needsShadowMap()) {
        _projectionComponent.depthMapRenderBegin();
        _depthFboProgramObject->activate();
        _depthFboProgramObject->setUniform(
            _depthFboUniformCache.ProjectorMatrix,
            projectorMatrix
        );
        _depthFboProgramObject->setUniform(
            _depthFboUniformCache.ModelTransform,
            _transform
        );

        _geometry->render(*_fboProgramObject, false, true);

        _depthFboProgramObject->deactivate();
        _projectionComponent.depthMapRenderEnd();
    }

    _projectionComponent.imageProjectBegin();
    _fboProgramObject->activate();

    ghoul::opengl::TextureUnit unitFbo;
    unitFbo.activate();
    projectionTexture.bind();
    _fboProgramObject->setUniform(_fboUniformCache.projectionTexture, unitFbo);

    _fboProgramObject->setUniform(
        _fboUniformCache.needShadowMap,
        _projectionComponent.needsShadowMap()
    );

    ghoul::opengl::TextureUnit unitDepthFbo;
    if (_projectionComponent.needsShadowMap()) {
        unitDepthFbo.activate();
        _projectionComponent.depthTexture().bind();
        _fboProgramObject->setUniform(_fboUniformCache.depthTexture, unitDepthFbo);
    }

    _fboProgramObject->setUniform(_fboUniformCache.ProjectorMatrix, projectorMatrix);
    _fboProgramObject->setUniform(_fboUniformCache.ModelTransform, _transform);
    _fboProgramObject->setUniform(_fboUniformCache.boresight, _boresight);

    _geometry->render(*_fboProgramObject, false, true);

    _fboProgramObject->deactivate();
    _projectionComponent.imageProjectEnd();
}

glm::mat4 RenderableModelProjection::attitudeParameters(double time, const glm::vec3& up)
{
    _instrumentMatrix = SpiceManager::ref().positionTransformMatrix(
        _projectionComponent.instrumentId(),
        "GALACTIC",
        time
    );

    SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(
        _projectionComponent.instrumentId()
    );
    _boresight = std::move(res.boresightVector);

    double lightTime;
    const glm::dvec3 p = SpiceManager::ref().targetPosition(
        _projectionComponent.projectorId(),
        _projectionComponent.projecteeId(),
        "GALACTIC",
        _projectionComponent.aberration(),
        time,
        lightTime
    );

    const glm::vec3 cpos = p * 10000.0;

    const float distance = glm::length(cpos);
    const double radius = boundingSphere();
    return _projectionComponent.computeProjectorMatrix(
        cpos,
        _boresight,
        up,
        _instrumentMatrix,
        _projectionComponent.fieldOfViewY(),
        _projectionComponent.aspectRatio(),
        static_cast<float>(distance - radius),
        static_cast<float>(distance + radius),
        _boresight
    );
}

}  // namespace openspace
