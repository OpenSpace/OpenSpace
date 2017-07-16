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

#include <modules/globebrowsing/globes/renderableglobe.h>
 
#include <modules/debugging/rendering/debugrenderer.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/pointglobe.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>

namespace {
    const char* keyFrame = "Frame";
    const char* keyRadii = "Radii";
    const char* keySegmentsPerPatch = "SegmentsPerPatch";
    const char* keyLayers = "Layers";
} // namespace

using namespace openspace::properties;

namespace openspace::globebrowsing {

RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
    : _debugProperties({
        BoolProperty("saveOrThrowCamera", "save or throw camera", false),
        BoolProperty("showChunkEdges", "show chunk edges", false),
        BoolProperty("showChunkBounds", "show chunk bounds", false),
        BoolProperty("showChunkAABB", "show chunk AABB", false),
        BoolProperty("showHeightResolution", "show height resolution", false),
        BoolProperty("showHeightIntensities", "show height intensities", false),
        BoolProperty("performFrustumCulling", "perform frustum culling", true),
        BoolProperty("performHorizonCulling", "perform horizon culling", true),
        BoolProperty("levelByProjectedAreaElseDistance", "level by projected area (else distance)", true),
        BoolProperty("resetTileProviders", "reset tile providers", false),
        BoolProperty("toggleEnabledEveryFrame", "toggle enabled every frame", false),
        BoolProperty("collectStats", "collect stats", false),
        BoolProperty("limitLevelByAvailableData", "Limit level by available data", true),
        IntProperty("modelSpaceRenderingCutoffLevel", "Model Space Rendering Cutoff Level", 10, 1, 22)
    })
    , _generalProperties({
        BoolProperty("enabled", "Enabled", true),
        BoolProperty("performShading", "perform shading", true),
        BoolProperty("atmosphere", "atmosphere", false),
        BoolProperty("useAccurateNormals", "useAccurateNormals", false),
        FloatProperty("lodScaleFactor", "lodScaleFactor",10.0f, 1.0f, 50.0f),
        FloatProperty("cameraMinHeight", "cameraMinHeight", 100.0f, 0.0f, 1000.0f),
        FloatProperty("orenNayarRoughness", "orenNayarRoughness", 0.0f, 0.0f, 1.0f)
    })
    , _debugPropertyOwner("Debug")
{
    setName("RenderableGlobe");
        
    dictionary.getValue(keyFrame, _frame);

    // Read the radii in to its own dictionary
    glm::dvec3 radii;
    dictionary.getValue(keyRadii, radii);
    _ellipsoid = Ellipsoid(radii);
    setBoundingSphere(_ellipsoid.maximumRadius());

    // Ghoul can't read ints from lua dictionaries...
    double patchSegmentsd;
    dictionary.getValue(keySegmentsPerPatch, patchSegmentsd);
    int patchSegments = patchSegmentsd;

    // Init layer manager
    ghoul::Dictionary layersDictionary;
    if (!dictionary.getValue(keyLayers, layersDictionary)) {
        throw ghoul::RuntimeError(
            std::string(keyLayers) + " must be specified specified!");
    }

    _layerManager = std::make_shared<LayerManager>(layersDictionary);

    _chunkedLodGlobe = std::make_shared<ChunkedLodGlobe>(
        *this, patchSegments, _layerManager);
    //_pointGlobe = std::make_shared<PointGlobe>(*this);
        
    _distanceSwitch.addSwitchValue(_chunkedLodGlobe);
    //_distanceSwitch.addSwitchValue(_pointGlobe);
        
    addProperty(_generalProperties.isEnabled);
    addProperty(_generalProperties.atmosphereEnabled);
    addProperty(_generalProperties.performShading);
    addProperty(_generalProperties.useAccurateNormals);
    addProperty(_generalProperties.lodScaleFactor);
    addProperty(_generalProperties.cameraMinHeight);
    addProperty(_generalProperties.orenNayarRoughness);
        
    _debugPropertyOwner.addProperty(_debugProperties.saveOrThrowCamera);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkEdges);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkBounds);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkAABB);
    _debugPropertyOwner.addProperty(_debugProperties.showHeightResolution);
    _debugPropertyOwner.addProperty(_debugProperties.showHeightIntensities);
    _debugPropertyOwner.addProperty(_debugProperties.performFrustumCulling);
    _debugPropertyOwner.addProperty(_debugProperties.performHorizonCulling);
    _debugPropertyOwner.addProperty(
        _debugProperties.levelByProjectedAreaElseDistance
    );
    _debugPropertyOwner.addProperty(_debugProperties.resetTileProviders);
    _debugPropertyOwner.addProperty(_debugProperties.toggleEnabledEveryFrame);
    _debugPropertyOwner.addProperty(_debugProperties.collectStats);
    _debugPropertyOwner.addProperty(_debugProperties.limitLevelByAvailableData);
    _debugPropertyOwner.addProperty(_debugProperties.modelSpaceRenderingCutoffLevel);
  
    auto notifyShaderRecompilation = [&](){
        _chunkedLodGlobe->notifyShaderRecompilation();
    };
    _generalProperties.atmosphereEnabled.onChange(notifyShaderRecompilation);
    _generalProperties.useAccurateNormals.onChange(notifyShaderRecompilation);
    _generalProperties.performShading.onChange(notifyShaderRecompilation);
    _debugProperties.showChunkEdges.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightResolution.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightIntensities.onChange(notifyShaderRecompilation);

    _layerManager->onChange(notifyShaderRecompilation);

    addPropertySubOwner(_debugPropertyOwner);
    addPropertySubOwner(_layerManager.get());
    //addPropertySubOwner(_pointGlobe.get());
}

bool RenderableGlobe::initialize() {
    return _distanceSwitch.initialize();
}

bool RenderableGlobe::deinitialize() {
    return _distanceSwitch.deinitialize();
}

bool RenderableGlobe::isReady() const {
    return true;
}

void RenderableGlobe::render(const RenderData& data) {
    bool statsEnabled = _debugProperties.collectStats.value();
    _chunkedLodGlobe->stats.setEnabled(statsEnabled);

    if (_debugProperties.toggleEnabledEveryFrame.value()) {
        _generalProperties.isEnabled.setValue(
            !_generalProperties.isEnabled.value()
        );
    }
    if (_generalProperties.isEnabled.value()) {
        if (_debugProperties.saveOrThrowCamera.value()) {
            _debugProperties.saveOrThrowCamera.setValue(false);

            if (savedCamera() == nullptr) { // save camera
                setSaveCamera(std::make_shared<Camera>(data.camera));
            }
            else { // throw camera
                setSaveCamera(nullptr);
            }
        }
        _distanceSwitch.render(data);
    }
    if (_savedCamera != nullptr) {
        DebugRenderer::ref().renderCameraFrustum(data, *_savedCamera);
    }
}

void RenderableGlobe::update(const UpdateData& data) {
    _time = data.time.j2000Seconds();
    _distanceSwitch.update(data);

    glm::dmat4 translation =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation);
    glm::dmat4 rotation = glm::dmat4(data.modelTransform.rotation);
    glm::dmat4 scaling =
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale,
            data.modelTransform.scale, data.modelTransform.scale));

    _cachedModelTransform = translation * rotation * scaling;
    _cachedInverseModelTransform = glm::inverse(_cachedModelTransform);

    if (_debugProperties.resetTileProviders) {
        _layerManager->reset();
        _debugProperties.resetTileProviders = false;
    }
    _layerManager->update();
    _chunkedLodGlobe->update(data);
}

glm::dvec3 RenderableGlobe::projectOnEllipsoid(glm::dvec3 position) {
    return _ellipsoid.geodeticSurfaceProjection(position);
}

float RenderableGlobe::getHeight(glm::dvec3 position) {
    if (_chunkedLodGlobe) {
        return _chunkedLodGlobe->getHeight(position);
    }
    else {
        return 0;
    }
}

std::shared_ptr<ChunkedLodGlobe> RenderableGlobe::chunkedLodGlobe() const{
    return _chunkedLodGlobe;
}

LayerManager* RenderableGlobe::layerManager() const {
    return _layerManager.get();
}

const Ellipsoid& RenderableGlobe::ellipsoid() const{
    return _ellipsoid;
}

const glm::dmat4& RenderableGlobe::modelTransform() const{
    return _cachedModelTransform;
}

const glm::dmat4& RenderableGlobe::inverseModelTransform() const{
    return _cachedInverseModelTransform;
}

const RenderableGlobe::DebugProperties&
    RenderableGlobe::debugProperties() const{
    return _debugProperties;
}
    
const RenderableGlobe::GeneralProperties&
    RenderableGlobe::generalProperties() const{
    return _generalProperties;
}

const std::shared_ptr<const Camera> RenderableGlobe::savedCamera() const {
    return _savedCamera;
}

SurfacePositionHandle RenderableGlobe::calculateSurfacePositionHandle(
                                                       const glm::dvec3& targetModelSpace) 
{
    glm::dvec3 centerToEllipsoidSurface =
        _ellipsoid.geodeticSurfaceProjection(targetModelSpace);
    glm::dvec3 ellipsoidSurfaceToTarget = targetModelSpace - centerToEllipsoidSurface;
    // ellipsoidSurfaceOutDirection will point towards the target, we want the outward
    // direction. Therefore it must be flipped in case the target is under the reference
    // ellipsoid so that it always points outwards
    glm::dvec3 ellipsoidSurfaceOutDirection = glm::normalize(ellipsoidSurfaceToTarget);
    if (glm::dot(ellipsoidSurfaceOutDirection, centerToEllipsoidSurface) < 0) {
        ellipsoidSurfaceOutDirection *= -1.0;
    }

    double heightToSurface = getHeight(targetModelSpace);
    heightToSurface = glm::isnan(heightToSurface) ? 0.0 : heightToSurface;
    centerToEllipsoidSurface = glm::isnan(glm::length(centerToEllipsoidSurface)) ?
        (glm::dvec3(0.0, 1.0, 0.0) * static_cast<double>(boundingSphere())) :
        centerToEllipsoidSurface;
    ellipsoidSurfaceOutDirection = glm::isnan(glm::length(ellipsoidSurfaceOutDirection)) ?
        glm::dvec3(0.0, 1.0, 0.0) : ellipsoidSurfaceOutDirection;

    return {
        centerToEllipsoidSurface,
        ellipsoidSurfaceOutDirection,
        heightToSurface
    };
}

void RenderableGlobe::setSaveCamera(std::shared_ptr<Camera> camera) { 
    _savedCamera = camera;
}

} // namespace openspace::globebrowsing
