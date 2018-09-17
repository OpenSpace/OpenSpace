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

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <modules/debugging/rendering/debugrenderer.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <ghoul/logging/logmanager.h>
#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/chunklevelevaluator.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/availabletiledataevaluator.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/distanceevaluator.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/projectedareaevaluator.h>
#include <modules/globebrowsing/chunk/chunknode.h>
#include <modules/globebrowsing/chunk/culling/chunkculler.h>
#include <modules/globebrowsing/chunk/culling/frustumculler.h>
#include <modules/globebrowsing/chunk/culling/horizonculler.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/meshes/skirtedgrid.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/rendering/chunkrenderer.h>
#include <modules/globebrowsing/rendering/layer/layer.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/debugging/rendering/debugrenderer.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* keyFrame = "Frame";
    constexpr const char* keyRadii = "Radii";
    constexpr const char* keySegmentsPerPatch = "SegmentsPerPatch";
    constexpr const char* keyLayers = "Layers";
    constexpr const char* keyShadowGroup = "ShadowGroup";
    constexpr const char* keyShadowSource = "Source";
    constexpr const char* keyShadowCaster = "Caster";

    const openspace::globebrowsing::GeodeticPatch Coverage =
        openspace::globebrowsing::GeodeticPatch(0, 0, 90, 180);

    const openspace::globebrowsing::TileIndex LeftHemisphereIndex =
        openspace::globebrowsing::TileIndex(0, 0, 1);

    const openspace::globebrowsing::TileIndex RightHemisphereIndex =
        openspace::globebrowsing::TileIndex(1, 0, 1);

    constexpr openspace::properties::Property::PropertyInfo SaveOrThrowInfo = {
        "SaveOrThrowCamera",
        "Save or throw camera",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ShowChunkEdgeInfo = {
        "ShowChunkEdges",
        "Show chunk edges",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ShowChunkBoundsInfo = {
        "ShowChunkBounds",
        "Show chunk bounds",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ShowChunkAABBInfo = {
        "ShowChunkAABB",
        "Show chunk AABB",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo HeightResolutionInfo = {
        "ShowHeightResolution",
        "Show height resolution",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo HeightIntensityInfo = {
        "ShowHeightIntensities",
        "Show height intensities",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo FrustumCullingInfo = {
        "PerformFrustumCulling",
        "Perform frustum culling",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo HorizonCullingInfo = {
        "PerformHorizonCulling",
        "Perform horizon culling",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo LevelProjectedAreaInfo = {
        "LevelByProjectedAreaElseDistance",
        "Level by projected area (else distance)",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ResetTileProviderInfo = {
        "ResetTileProviders",
        "Reset tile providers",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo CollectStatsInfo = {
        "CollectStats",
        "Collect stats",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo LimitLevelInfo = {
        "LimitLevelByAvailableData",
        "Limit level by available data",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ModelSpaceRenderingInfo = {
        "ModelSpaceRenderingCutoffLevel",
        "Model Space Rendering Cutoff Level",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo PerformShadingInfo = {
        "PerformShading",
        "Perform shading",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "Atmosphere",
        "Atmosphere",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo AccurateNormalsInfo = {
        "UseAccurateNormals",
        "Use Accurate Normals",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo EclipseInfo = {
        "Eclipse",
        "Eclipse",
        "Enables/Disable Eclipse shadows"
    };

    constexpr openspace::properties::Property::PropertyInfo EclipseHardShadowsInfo = {
        "EclipseHardShadows",
        "Eclipse Hard Shadows",
        "Enables the rendering of eclipse shadows using hard shadows"
    };

    constexpr openspace::properties::Property::PropertyInfo LodScaleFactorInfo = {
        "LodScaleFactor",
        "Level of Detail Scale Factor",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo CameraMinHeightInfo = {
        "CameraMinHeight",
        "Camera Minimum Height",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo OrenNayarRoughnessInfo = {
        "OrenNayarRoughness",
        "orenNayarRoughness",
        "" // @TODO Missing documentation
    };
} // namespace

using namespace openspace::properties;

namespace openspace::globebrowsing {

RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _debugProperties({
        BoolProperty(SaveOrThrowInfo, false),
        BoolProperty(ShowChunkEdgeInfo, false),
        BoolProperty(ShowChunkBoundsInfo, false),
        BoolProperty(ShowChunkAABBInfo, false),
        BoolProperty(HeightResolutionInfo, false),
        BoolProperty(HeightIntensityInfo, false),
        BoolProperty(FrustumCullingInfo, true),
        BoolProperty(HorizonCullingInfo, true),
        BoolProperty(LevelProjectedAreaInfo, false),
        BoolProperty(ResetTileProviderInfo, false),
        BoolProperty(CollectStatsInfo, false),
        BoolProperty(LimitLevelInfo, true),
        IntProperty(ModelSpaceRenderingInfo, 10, 1, 22)
    })
    , _generalProperties({
        BoolProperty(PerformShadingInfo, true),
        BoolProperty(AtmosphereInfo, false),
        BoolProperty(AccurateNormalsInfo, false),
        BoolProperty(EclipseInfo, false),
        BoolProperty(EclipseHardShadowsInfo, false),
        FloatProperty(LodScaleFactorInfo, 10.f, 1.f, 50.f),
        FloatProperty(CameraMinHeightInfo, 100.f, 0.f, 1000.f),
        FloatProperty(OrenNayarRoughnessInfo, 0.f, 0.f, 1.f)
    })
    , _debugPropertyOwner({ "Debug" })
    , _leftRoot(std::make_unique<ChunkNode>(Chunk(*this, LeftHemisphereIndex)))
    , _rightRoot(std::make_unique<ChunkNode>(Chunk(*this, RightHemisphereIndex)))
{
    setIdentifier("RenderableGlobe");

    dictionary.getValue(keyFrame, _frame);

    // Read the radii in to its own dictionary
    if (dictionary.hasKeyAndValue<glm::dvec3>(keyRadii)) {
        const glm::dvec3 radii = dictionary.value<glm::vec3>(keyRadii);
        _ellipsoid = Ellipsoid(radii);
        setBoundingSphere(static_cast<float>(_ellipsoid.maximumRadius()));
    }
    else if (dictionary.hasKeyAndValue<double>(keyRadii)) {
        const double radius = dictionary.value<double>(keyRadii);
        _ellipsoid = Ellipsoid({ radius, radius, radius });
        setBoundingSphere(static_cast<float>(_ellipsoid.maximumRadius()));
    }

    // Ghoul can't read ints from lua dictionaries...
    double patchSegmentsd;
    dictionary.getValue(keySegmentsPerPatch, patchSegmentsd);
    int patchSegments = static_cast<int>(patchSegmentsd);

    if (dictionary.hasValue<bool>("PerformShading")) {
        _generalProperties.performShading = dictionary.value<bool>("PerformShading");
    }

    // Init layer manager
    ghoul::Dictionary layersDictionary;
    if (!dictionary.getValue(keyLayers, layersDictionary)) {
        throw ghoul::RuntimeError(std::string(keyLayers) + " must be specified");
    }

    _layerManager = std::make_shared<LayerManager>(layersDictionary);


    std::shared_ptr<SkirtedGrid> geometry = std::make_shared<SkirtedGrid>(
        static_cast<unsigned int>(patchSegments),
        static_cast<unsigned int>(patchSegments),
        TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates::Yes,
        TriangleSoup::Normals::No
        );

    _chunkCullers.push_back(std::make_unique<culling::HorizonCuller>());
    _chunkCullers.push_back(std::make_unique<culling::FrustumCuller>(
        AABB3(
            glm::vec3(-1, -1, 0),
            glm::vec3(1, 1, 1e35)
        )
        ));

    _chunkEvaluatorByAvailableTiles =
        std::make_unique<chunklevelevaluator::AvailableTileData>();
    _chunkEvaluatorByProjectedArea =
        std::make_unique<chunklevelevaluator::ProjectedArea>();
    _chunkEvaluatorByDistance = std::make_unique<chunklevelevaluator::Distance>();

    _renderer = std::make_unique<ChunkRenderer>(geometry, _layerManager, _ellipsoid);


    addProperty(_generalProperties.atmosphereEnabled);
    addProperty(_generalProperties.performShading);
    addProperty(_generalProperties.useAccurateNormals);
    addProperty(_generalProperties.eclipseShadowsEnabled);
    addProperty(_generalProperties.eclipseHardShadows);
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
    _debugPropertyOwner.addProperty(_debugProperties.collectStats);
    _debugPropertyOwner.addProperty(_debugProperties.limitLevelByAvailableData);
    _debugPropertyOwner.addProperty(_debugProperties.modelSpaceRenderingCutoffLevel);

    auto notifyShaderRecompilation = [&](){
        this->notifyShaderRecompilation();
    };
    _generalProperties.atmosphereEnabled.onChange(notifyShaderRecompilation);
    _generalProperties.useAccurateNormals.onChange(notifyShaderRecompilation);
    _generalProperties.eclipseShadowsEnabled.onChange(notifyShaderRecompilation);
    _generalProperties.eclipseHardShadows.onChange(notifyShaderRecompilation);
    _generalProperties.performShading.onChange(notifyShaderRecompilation);
    _debugProperties.showChunkEdges.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightResolution.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightIntensities.onChange(notifyShaderRecompilation);

    _layerManager->onChange(notifyShaderRecompilation);

    addPropertySubOwner(_debugPropertyOwner);
    addPropertySubOwner(_layerManager.get());
    //addPropertySubOwner(_pointGlobe.get());

    //================================================================
    //======== Reads Shadow (Eclipses) Entries in mod file ===========
    //================================================================
    ghoul::Dictionary shadowDictionary;
    bool success = dictionary.getValue(keyShadowGroup, shadowDictionary);
    bool disableShadows = false;
    if (success) {
        std::vector<std::pair<std::string, double>> sourceArray;
        unsigned int sourceCounter = 1;
        while (success) {
            std::string sourceName;
            success = shadowDictionary.getValue(keyShadowSource +
                std::to_string(sourceCounter) + ".Name", sourceName);
            if (success) {
                double sourceRadius;
                success = shadowDictionary.getValue(keyShadowSource +
                    std::to_string(sourceCounter) + ".Radius", sourceRadius);
                if (success) {
                    sourceArray.emplace_back(sourceName, sourceRadius);
                }
                else {
                    //LWARNING("No Radius value expecified for Shadow Source Name "
                    //    << sourceName << " from " << name
                    //    << " planet.\nDisabling shadows for this planet.");
                    disableShadows = true;
                    break;
                }
            }
            sourceCounter++;
        }

        if (!disableShadows && !sourceArray.empty()) {
            success = true;
            std::vector<std::pair<std::string, double>> casterArray;
            unsigned int casterCounter = 1;
            while (success) {
                std::string casterName;
                success = shadowDictionary.getValue(keyShadowCaster +
                    std::to_string(casterCounter) + ".Name", casterName);
                if (success) {
                    double casterRadius;
                    success = shadowDictionary.getValue(keyShadowCaster +
                        std::to_string(casterCounter) + ".Radius", casterRadius);
                    if (success) {
                        casterArray.emplace_back(casterName, casterRadius);
                    }
                    else {
                        //LWARNING("No Radius value expecified for Shadow Caster Name "
                        //    << casterName << " from " << name
                        //    << " planet.\nDisabling shadows for this planet.");
                        disableShadows = true;
                        break;
                    }
                }

                casterCounter++;
            }

            std::vector<Ellipsoid::ShadowConfiguration> shadowConfArray;
            if (!disableShadows && (!sourceArray.empty() && !casterArray.empty())) {
                for (const auto & source : sourceArray) {
                    for (const auto & caster : casterArray) {
                        Ellipsoid::ShadowConfiguration sc;
                        sc.source = source;
                        sc.caster = caster;
                        shadowConfArray.push_back(sc);
                    }
                }
                _ellipsoid.setShadowConfigurationArray(shadowConfArray);
            }
        }
    }
}

void RenderableGlobe::initializeGL() {
    _layerManager->initialize();

    _layerManager->update();

    // Recompile the shaders directly so that it is not done the first time the render
    // function is called.
    recompileShaders();
}

void RenderableGlobe::deinitializeGL() {
    _layerManager->deinitialize();
}

bool RenderableGlobe::isReady() const {
    return true;
}

void RenderableGlobe::render(const RenderData& data, RendererTasks& rendererTask) {
    bool statsEnabled = _debugProperties.collectStats;
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    _chunkedLodGlobe->stats.setEnabled(statsEnabled);
#else // DEBUG_GLOBEBROWSING_STATSRECORD
    if (statsEnabled) {
        LWARNINGC(
            "RenderableGlobe",
            "Stats collection was enabled, but ChunkedLodGlobe compiled without support"
        );
        _debugProperties.collectStats = false;
    }
#endif // DEBUG_GLOBEBROWSING_STATSRECORD

    if (_enabled) {
        if (_debugProperties.saveOrThrowCamera) {
            _debugProperties.saveOrThrowCamera = false;

            if (!savedCamera()) {
                // save camera
                setSaveCamera(std::make_shared<Camera>(data.camera));
            }
            else { // throw camera
                setSaveCamera(nullptr);
            }
        }

        const double distanceToCamera = distance(
            data.camera.positionVec3(),
            data.modelTransform.translation
        );

        // This distance will be enough to render the globe as one pixel if the field of
        // view is 'fov' radians and the screen resolution is 'res' pixels.
        const double fov = 2 * glm::pi<double>() / 6; // 60 degrees
        const int res = 2880;

        const double distance = res * boundingSphere() / tan(fov / 2);

        if (distanceToCamera < distance) {
            renderChunks(data, rendererTask);
        }
    }
    if (_savedCamera != nullptr) {
        DebugRenderer::ref().renderCameraFrustum(data, *_savedCamera);
    }
}

void RenderableGlobe::update(const UpdateData& data) {
    _time = data.time.j2000Seconds();

    setBoundingSphere(static_cast<float>(
        ellipsoid().maximumRadius() * data.modelTransform.scale
        ));
    _renderer->update();


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
    setBoundingSphere(static_cast<float>(
        ellipsoid().maximumRadius() * data.modelTransform.scale
        ));
    _renderer->update();
}

glm::dvec3 RenderableGlobe::projectOnEllipsoid(glm::dvec3 position) {
    return _ellipsoid.geodeticSurfaceProjection(position);
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
                                                 const glm::dvec3& targetModelSpace) const
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
    _savedCamera = std::move(camera);
}








bool RenderableGlobe::testIfCullable(const Chunk& chunk,
    const RenderData& renderData) const
{
    if (debugProperties().performHorizonCulling &&
        _chunkCullers[0]->isCullable(chunk, renderData)) {
        return true;
    }
    if (debugProperties().performFrustumCulling &&
        _chunkCullers[1]->isCullable(chunk, renderData)) {
        return true;
    }
    return false;
}

const ChunkNode& RenderableGlobe::findChunkNode(const Geodetic2& location) const {
    ghoul_assert(
        Coverage.contains(location),
        "Point must be in lat [-90, 90] and lon [-180, 180]"
    );

    return location.lon < Coverage.center().lon ?
        _leftRoot->find(location) :
        _rightRoot->find(location);
}

int RenderableGlobe::desiredLevel(const Chunk& chunk,
    const RenderData& renderData) const
{
    int desiredLevel = 0;
    if (debugProperties().levelByProjectedAreaElseDistance) {
        desiredLevel = _chunkEvaluatorByProjectedArea->desiredLevel(chunk, renderData);
    }
    else {
        desiredLevel = _chunkEvaluatorByDistance->desiredLevel(chunk, renderData);
    }

    int levelByAvailableData = _chunkEvaluatorByAvailableTiles->desiredLevel(
        chunk,
        renderData
    );
    if (levelByAvailableData != chunklevelevaluator::Evaluator::UnknownDesiredLevel &&
        debugProperties().limitLevelByAvailableData)
    {
        desiredLevel = glm::min(desiredLevel, levelByAvailableData);
    }

    desiredLevel = glm::clamp(desiredLevel, MinSplitDepth, MaxSplitDepth);
    return desiredLevel;
}

float RenderableGlobe::getHeight(const glm::dvec3& position) const {
    float height = 0;

    // Get the uv coordinates to sample from
    const Geodetic2 geodeticPosition = ellipsoid().cartesianToGeodetic2(position);
    const int chunkLevel = findChunkNode(geodeticPosition).chunk().tileIndex().level;

    const TileIndex tileIndex = TileIndex(geodeticPosition, chunkLevel);
    const GeodeticPatch patch = GeodeticPatch(tileIndex);

    const Geodetic2 geoDiffPatch = patch.corner(Quad::NORTH_EAST) -
        patch.corner(Quad::SOUTH_WEST);

    const Geodetic2 geoDiffPoint = geodeticPosition - patch.corner(Quad::SOUTH_WEST);
    const glm::vec2 patchUV = glm::vec2(
        geoDiffPoint.lon / geoDiffPatch.lon,
        geoDiffPoint.lat / geoDiffPatch.lat
    );

    // Get the tile providers for the height maps
    const std::vector<std::shared_ptr<Layer>>& heightMapLayers =
        _layerManager->layerGroup(layergroupid::GroupID::HeightLayers).activeLayers();

    for (const std::shared_ptr<Layer>& layer : heightMapLayers) {
        tileprovider::TileProvider* tileProvider = layer->tileProvider();
        if (!tileProvider) {
            continue;
        }
        // Transform the uv coordinates to the current tile texture
        const ChunkTile chunkTile = tileProvider->chunkTile(tileIndex);
        const Tile& tile = chunkTile.tile;
        const TileUvTransform& uvTransform = chunkTile.uvTransform;
        const TileDepthTransform& depthTransform = tileProvider->depthTransform();
        if (tile.status() != Tile::Status::OK) {
            return 0;
        }

        ghoul::opengl::Texture* tileTexture = tile.texture();
        if (!tileTexture) {
            return 0;
        }

        glm::vec2 transformedUv = layer->TileUvToTextureSamplePosition(
            uvTransform,
            patchUV,
            glm::uvec2(tileTexture->dimensions())
        );

        // Sample and do linear interpolation
        // (could possibly be moved as a function in ghoul texture)
        // Suggestion: a function in ghoul::opengl::Texture that takes uv coordinates
        // in range [0,1] and uses the set interpolation method and clamping.

        const glm::uvec3 dimensions = tileTexture->dimensions();

        const glm::vec2 samplePos = transformedUv * glm::vec2(dimensions);
        glm::uvec2 samplePos00 = samplePos;
        samplePos00 = glm::clamp(
            samplePos00,
            glm::uvec2(0, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::vec2 samplePosFract = samplePos - glm::vec2(samplePos00);

        const glm::uvec2 samplePos10 = glm::min(
            samplePos00 + glm::uvec2(1, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::uvec2 samplePos01 = glm::min(
            samplePos00 + glm::uvec2(0, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::uvec2 samplePos11 = glm::min(
            samplePos00 + glm::uvec2(1, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );

        const float sample00 = tileTexture->texelAsFloat(samplePos00).x;
        const float sample10 = tileTexture->texelAsFloat(samplePos10).x;
        const float sample01 = tileTexture->texelAsFloat(samplePos01).x;
        const float sample11 = tileTexture->texelAsFloat(samplePos11).x;

        // In case the texture has NaN or no data values don't use this height map.
        const bool anySampleIsNaN =
            std::isnan(sample00) ||
            std::isnan(sample01) ||
            std::isnan(sample10) ||
            std::isnan(sample11);

        const bool anySampleIsNoData =
            sample00 == tileProvider->noDataValueAsFloat() ||
            sample01 == tileProvider->noDataValueAsFloat() ||
            sample10 == tileProvider->noDataValueAsFloat() ||
            sample11 == tileProvider->noDataValueAsFloat();

        if (anySampleIsNaN || anySampleIsNoData) {
            continue;
        }

        const float sample0 = sample00 * (1.f - samplePosFract.x) +
            sample10 * samplePosFract.x;
        const float sample1 = sample01 * (1.f - samplePosFract.x) +
            sample11 * samplePosFract.x;

        const float sample = sample0 * (1.f - samplePosFract.y) +
            sample1 * samplePosFract.y;

        // Same as is used in the shader. This is not a perfect solution but
        // if the sample is actually a no-data-value (min_float) the interpolated
        // value might not be. Therefore we have a cut-off. Assuming no data value
        // is smaller than -100000
        if (sample > -100000) {
            // Perform depth transform to get the value in meters
            height = depthTransform.depthOffset + depthTransform.depthScale * sample;
            // Make sure that the height value follows the layer settings.
            // For example if the multiplier is set to a value bigger than one,
            // the sampled height should be modified as well.
            height = layer->renderSettings().performLayerSettings(height);
        }
    }
    // Return the result
    return height;
}

void RenderableGlobe::notifyShaderRecompilation() {
    _shadersNeedRecompilation = true;
}

void RenderableGlobe::recompileShaders() {
    _renderer->recompileShaders(*this);
    _shadersNeedRecompilation = false;
}

void RenderableGlobe::renderChunks(const RenderData& data, RendererTasks&) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    stats.startNewRecord();
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
    if (_shadersNeedRecompilation) {
        _renderer->recompileShaders(*this);
        _shadersNeedRecompilation = false;
    }

#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    auto duration = std::chrono::system_clock::now().time_since_epoch();
    auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
    stats.i["time"] = millis;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD

    _leftRoot->updateChunkTree(data);
    _rightRoot->updateChunkTree(data);

    // Calculate the MVP matrix
    const glm::dmat4 viewTransform = glm::dmat4(data.camera.combinedViewMatrix());
    const glm::dmat4 vp = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
        viewTransform;
    const glm::dmat4 mvp = vp * modelTransform();

    // Render function
    auto renderJob = [this, &data, &mvp](const ChunkNode& chunkNode) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
        stats.i["chunks nodes"]++;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
        const Chunk& chunk = chunkNode.chunk();
        if (chunkNode.isLeaf()) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
            stats.i["leafs chunk nodes"]++;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD

            if (chunk.isVisible()) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
                stats.i["rendered chunks"]++;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
                _renderer->renderChunk(chunkNode.chunk(), data);
                debugRenderChunk(chunk, mvp);
            }
        }
    };

    _leftRoot->breadthFirst(renderJob);
    _rightRoot->breadthFirst(renderJob);

    //_leftRoot->reverseBreadthFirst(renderJob);
    //_rightRoot->reverseBreadthFirst(renderJob);

#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    auto duration2 = std::chrono::system_clock::now().time_since_epoch();
    auto ms2 = std::chrono::duration_cast<std::chrono::milliseconds>(duration2).count();
    stats.i["chunk globe render time"] = ms2 - millis;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
}

void RenderableGlobe::debugRenderChunk(const Chunk& chunk, const glm::dmat4& mvp) const {
    if (debugProperties().showChunkBounds ||
        debugProperties().showChunkAABB)
    {
        const std::vector<glm::dvec4>& modelSpaceCorners =
            chunk.boundingPolyhedronCorners();

        std::vector<glm::vec4> clippingSpaceCorners(8);
        AABB3 screenSpaceBounds;

        for (size_t i = 0; i < 8; ++i) {
            const glm::vec4& clippingSpaceCorner = mvp * modelSpaceCorners[i];
            clippingSpaceCorners[i] = clippingSpaceCorner;

            glm::vec3 screenSpaceCorner =
                glm::vec3((1.f / clippingSpaceCorner.w) * clippingSpaceCorner);
            screenSpaceBounds.expand(std::move(screenSpaceCorner));
        }

        const unsigned int colorBits = 1 + chunk.tileIndex().level % 6;
        const glm::vec4 color = glm::vec4(
            colorBits & 1,
            colorBits & 2,
            colorBits & 4,
            0.3f
        );

        if (debugProperties().showChunkBounds) {
            DebugRenderer::ref().renderNiceBox(clippingSpaceCorners, color);
        }

        if (debugProperties().showChunkAABB) {
            const std::vector<glm::vec4>& screenSpacePoints =
                DebugRenderer::ref().verticesFor(screenSpaceBounds);
            DebugRenderer::ref().renderNiceBox(screenSpacePoints, color);
        }
    }
}


} // namespace openspace::globebrowsing
