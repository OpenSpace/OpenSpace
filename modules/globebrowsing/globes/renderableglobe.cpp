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

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <ghoul/misc/threadpool.h>

#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/tileprovidermanager.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <modules/debugging/rendering/debugrenderer.h>

// ghoul includes
#include <ghoul/misc/assert.h>

namespace {
    const std::string _loggerCat = "RenderableGlobe";

    // Keys for the dictionary
    const std::string keyFrame = "Frame";
    const std::string keyRadii = "Radii";
    const std::string keyInteractionDepthBelowEllipsoid =
        "InteractionDepthBelowEllipsoid";
    const std::string keyCameraMinHeight = "CameraMinHeight";
    const std::string keySegmentsPerPatch = "SegmentsPerPatch";
    const std::string keyTextures = "Textures";
    const std::string keyColorTextures = "ColorTextures";
    const std::string keyHeightMaps = "HeightMaps";
}

namespace openspace {
namespace globebrowsing {

    SingleTexturePropertyOwner::SingleTexturePropertyOwner(std::string name)
    : isEnabled("isEnabled", "isEnabled", true) {
        setName(name);
        addProperty(isEnabled);
    }
    
    SingleTexturePropertyOwner::~SingleTexturePropertyOwner() {
        
    }
    
    LayeredCategoryPropertyOwner::LayeredCategoryPropertyOwner(
        LayeredTextures::TextureCategory category,
        TileProviderManager& tileProviderManager)
    : _tileProviderManager(tileProviderManager)
    , _levelBlendingEnabled("blendTileLevels", "blend tile levels", true){
        setName(LayeredTextures::TEXTURE_CATEGORY_NAMES[category]);
        
        // Create the property owners
        auto& layerGroup = _tileProviderManager.getTileProviderGroup(category);
        for (NamedTileProvider& tileProvider : layerGroup.tileProviders) {
            _texturePropertyOwners.push_back(
                std::make_unique<SingleTexturePropertyOwner>(tileProvider.name));
        }
        
        // Specify and add the property owners
        for (int i = 0; i < layerGroup.tileProviders.size(); i++) {
            NamedTileProvider &tileProvider = layerGroup.tileProviders[i];
            SingleTexturePropertyOwner &prop = *_texturePropertyOwners[i].get();
            prop.isEnabled.set(tileProvider.isActive);
            prop.isEnabled.onChange([&]{
                tileProvider.isActive = prop.isEnabled;
            });
            
            for (auto setting : tileProvider.settings.array()) {
                prop.addProperty(setting->property());
            }
  
            addPropertySubOwner(prop);
        }

        _levelBlendingEnabled.set(layerGroup.levelBlendingEnabled);
        _levelBlendingEnabled.onChange([&]{
            layerGroup.levelBlendingEnabled = _levelBlendingEnabled;
        });
        addProperty(_levelBlendingEnabled);
    }
    
    LayeredCategoryPropertyOwner::~LayeredCategoryPropertyOwner() {
        
    }
    
    RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
    : _generalProperties({
        properties::BoolProperty("enabled", "Enabled", true),
        properties::BoolProperty("performShading", "perform shading", true),
        properties::BoolProperty("atmosphere", "atmosphere", false),
        properties::FloatProperty("lodScaleFactor", "lodScaleFactor",10.0f, 1.0f, 50.0f),
        properties::FloatProperty(
            "cameraMinHeight", "cameraMinHeight", 100.0f, 0.0f, 1000.0f)
    })
    , _debugProperties({
        properties::BoolProperty("saveOrThrowCamera", "save or throw camera", false),
        properties::BoolProperty("showChunkEdges", "show chunk edges", false),
        properties::BoolProperty("showChunkBounds", "show chunk bounds", false),
        properties::BoolProperty("showChunkAABB", "show chunk AABB", false),
    	properties::BoolProperty("showHeightResolution", "show height resolution", false),
        properties::BoolProperty(
            "showHeightIntensities", "show height intensities", false),
        properties::BoolProperty(
            "performFrustumCulling", "perform frustum culling", true),
        properties::BoolProperty(
            "performHorizonCulling", "perform horizon culling", true),
        properties::BoolProperty(
            "levelByProjectedAreaElseDistance", "level by projected area (else distance)",
            false),
        properties::BoolProperty("resetTileProviders", "reset tile providers", false),
        properties::BoolProperty(
            "toggleEnabledEveryFrame", "toggle enabled every frame", false)})
    {
        setName("RenderableGlobe");
        
        dictionary.getValue(keyFrame, _frame);

        // Read the radii in to its own dictionary
        Vec3 radii;
        dictionary.getValue(keyRadii, radii);
        _ellipsoid = Ellipsoid(radii);
        setBoundingSphere(pss(_ellipsoid.averageRadius(), 0.0));

        // Ghoul can't read ints from lua dictionaries...
        double patchSegmentsd;
        dictionary.getValue(keySegmentsPerPatch, patchSegmentsd);
        int patchSegments = patchSegmentsd;
        
        dictionary.getValue(keyInteractionDepthBelowEllipsoid,
            _interactionDepthBelowEllipsoid);
        float cameraMinHeight;
        dictionary.getValue(keyCameraMinHeight, cameraMinHeight);
        _generalProperties.cameraMinHeight.set(cameraMinHeight);

        // Init tile provider manager
        ghoul::Dictionary texturesDictionary;
        dictionary.getValue(keyTextures, texturesDictionary);

        _tileProviderManager = std::make_shared<TileProviderManager>(texturesDictionary);

        _chunkedLodGlobe = std::make_shared<ChunkedLodGlobe>(
            *this, patchSegments, _tileProviderManager);
        _pointGlobe = std::make_shared<PointGlobe>(*this);
        
        _distanceSwitch.addSwitchValue(_chunkedLodGlobe, 1e9);
        _distanceSwitch.addSwitchValue(_pointGlobe, 1e12);

        _debugPropertyOwner.setName("Debug");
        _texturePropertyOwner.setName("Textures");

        addProperty(_generalProperties.isEnabled);
        addProperty(_generalProperties.atmosphereEnabled);
        addProperty(_generalProperties.performShading);
        addProperty(_generalProperties.lodScaleFactor);
        addProperty(_generalProperties.cameraMinHeight);
        
        _debugPropertyOwner.addProperty(_debugProperties.saveOrThrowCamera);
        _debugPropertyOwner.addProperty(_debugProperties.showChunkEdges);
        _debugPropertyOwner.addProperty(_debugProperties.showChunkBounds);
        _debugPropertyOwner.addProperty(_debugProperties.showChunkAABB);
        _debugPropertyOwner.addProperty(_debugProperties.showHeightResolution);
        _debugPropertyOwner.addProperty(_debugProperties.showHeightIntensities);
        _debugPropertyOwner.addProperty(_debugProperties.performFrustumCulling);
        _debugPropertyOwner.addProperty(_debugProperties.performHorizonCulling);
        _debugPropertyOwner.addProperty(
            _debugProperties.levelByProjectedAreaElseDistance);
        _debugPropertyOwner.addProperty(_debugProperties.resetTileProviders);
        //_debugPropertyOwner.addProperty(_debugProperties.toggleEnabledEveryFrame);
              
        for (int i = 0; i < LayeredTextures::NUM_TEXTURE_CATEGORIES; i++) {
            _textureProperties.push_back(std::make_unique<LayeredCategoryPropertyOwner>
                (LayeredTextures::TextureCategory(i), *_tileProviderManager));
            _texturePropertyOwner.addPropertySubOwner(*_textureProperties[i]);
        }
        
        addPropertySubOwner(_debugPropertyOwner);
        addPropertySubOwner(_texturePropertyOwner);
    }

    RenderableGlobe::~RenderableGlobe() {

    }

    bool RenderableGlobe::initialize() {
        return _distanceSwitch.initialize();
    }

    bool RenderableGlobe::deinitialize() {
        return _distanceSwitch.deinitialize();
    }

    bool RenderableGlobe::isReady() const {
        return _distanceSwitch.isReady();
    }

    void RenderableGlobe::render(const RenderData& data) {
        if (_debugProperties.toggleEnabledEveryFrame.value()) {
            _generalProperties.isEnabled.setValue(
                !_generalProperties.isEnabled.value());
        }
        if (_generalProperties.isEnabled.value()) {
            if (_debugProperties.saveOrThrowCamera.value()) {
                _debugProperties.saveOrThrowCamera.setValue(false);

                if (savedCamera() == nullptr) { // save camera
                    LDEBUG("Saving snapshot of camera!");
                    setSaveCamera(std::make_shared<Camera>(data.camera));
                }
                else { // throw camera
                    LDEBUG("Throwing away saved camera!");
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
        _time = data.time;
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
            _tileProviderManager->reset();
            _debugProperties.resetTileProviders = false;
        }
        _tileProviderManager->update();
        _chunkedLodGlobe->update(data);
    }

    glm::dvec3 RenderableGlobe::projectOnEllipsoid(glm::dvec3 position) {
        return _ellipsoid.geodeticSurfaceProjection(position);
    }

    float RenderableGlobe::getHeight(glm::dvec3 position) {
        // Get the tile provider for the height map
        const auto& heightMapProviders = _tileProviderManager->getTileProviderGroup(
            LayeredTextures::HeightMaps).getActiveTileProviders();
        if (heightMapProviders.size() == 0)
            return 0;
        const auto& tileProvider = heightMapProviders[0];

        // Get the uv coordinates to sample from
        Geodetic2 geodeticPosition = _ellipsoid.cartesianToGeodetic2(position);
        int chunkLevel = _chunkedLodGlobe->findChunkNode(
            geodeticPosition).getChunk().tileIndex().level;
        
        TileIndex chunkIdx = TileIndex(geodeticPosition, chunkLevel);
        GeodeticPatch patch = GeodeticPatch(chunkIdx);
        Geodetic2 geoDiffPatch =
            patch.getCorner(Quad::NORTH_EAST) -
            patch.getCorner(Quad::SOUTH_WEST);
        Geodetic2 geoDiffPoint = geodeticPosition - patch.getCorner(Quad::SOUTH_WEST);
        glm::vec2 patchUV = glm::vec2(
            geoDiffPoint.lon / geoDiffPatch.lon, geoDiffPoint.lat / geoDiffPatch.lat);

        // Transform the uv coordinates to the current tile texture
        TileAndTransform tileAndTransform = TileSelector::getHighestResolutionTile(
            tileProvider.get(), chunkIdx);
        const auto& tile = tileAndTransform.tile;
        const auto& uvTransform = tileAndTransform.uvTransform;
        const auto& depthTransform = tileProvider->depthTransform();
        if (tile.status != Tile::Status::OK) {
            return 0;
        }
        //glm::vec2 transformedUv = uvTransform.uvOffset + uvTransform.uvScale * patchUV;

        glm::vec2 transformedUv = Tile::TileUvToTextureSamplePosition(
            uvTransform,
            patchUV,
            glm::uvec2(tile.texture->dimensions()));

        // Sample and do linear interpolation
        // (could possibly be moved as a function in ghoul texture)
        glm::uvec3 dimensions = tile.texture->dimensions();
        
        glm::vec2 samplePos = transformedUv * glm::vec2(dimensions);
        glm::uvec2 samplePos00 = samplePos;
        samplePos00 = glm::clamp(
            samplePos00, glm::uvec2(0, 0), glm::uvec2(dimensions) - glm::uvec2(1));
        glm::vec2 samplePosFract = samplePos - glm::vec2(samplePos00);

        glm::uvec2 samplePos10 = glm::min(
            samplePos00 + glm::uvec2(1, 0), glm::uvec2(dimensions) - glm::uvec2(1));
        glm::uvec2 samplePos01 = glm::min(
            samplePos00 + glm::uvec2(0, 1), glm::uvec2(dimensions) - glm::uvec2(1));
        glm::uvec2 samplePos11 = glm::min(
            samplePos00 + glm::uvec2(1, 1), glm::uvec2(dimensions) - glm::uvec2(1));

        float sample00 = tile.texture->texelAsFloat(samplePos00).x;
        float sample10 = tile.texture->texelAsFloat(samplePos10).x;
        float sample01 = tile.texture->texelAsFloat(samplePos01).x;
        float sample11 = tile.texture->texelAsFloat(samplePos11).x;

        float sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
        float sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

        float sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

        // Perform depth transform to get the value in meters
        float height = depthTransform.depthOffset + depthTransform.depthScale * sample;
        
        // Return the result
        return height;
    }

    std::shared_ptr<ChunkedLodGlobe> RenderableGlobe::chunkedLodGlobe() const{
        return _chunkedLodGlobe;
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

    double RenderableGlobe::interactionDepthBelowEllipsoid() {
        return _interactionDepthBelowEllipsoid;
    }

    void RenderableGlobe::setSaveCamera(std::shared_ptr<Camera> camera) { 
        _savedCamera = camera;
    }
} // namespace globebrowsing
} // namespace openspace

