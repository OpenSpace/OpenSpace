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

#include <modules/globebrowsing/other/threadpool.h>
#include <modules/globebrowsing/tile/temporaltileprovider.h>
#include <modules/globebrowsing/tile/tileselector.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

namespace {
    const std::string _loggerCat = "RenderableGlobe";

    // Keys for the dictionary
    const std::string keyFrame = "Frame";
    const std::string keyRadii = "Radii";
    const std::string keySegmentsPerPatch = "SegmentsPerPatch";
    const std::string keyTextureInitData = "TextureInitData";
    const std::string keyTextures = "Textures";
    const std::string keyColorTextures = "ColorTextures";
    const std::string keyHeightMaps = "HeightMaps";
}



namespace openspace {


    RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
        : _saveOrThrowCamera(properties::BoolProperty("saveOrThrowCamera", "saveOrThrowCamera"))
        , lodScaleFactor(properties::FloatProperty("lodScaleFactor", "lodScaleFactor", 5.0f, 1.0f, 50.0f))
        , debugSelection(ReferencedBoolSelection("Debug", "Debug"))
        , atmosphereEnabled(properties::BoolProperty(" Atmosphere", " Atmosphere", false))
    {
        setName("RenderableGlobe");
        
        

        dictionary.getValue(keyFrame, _frame);

        // Read the radii in to its own dictionary
        Vec3 radii;
        dictionary.getValue(keyRadii, radii);
        _ellipsoid = Ellipsoid(radii);
        setBoundingSphere(pss(_ellipsoid.averageRadius(), 0.0));

        // Ghoul can't read ints from lua dictionaries
        double patchSegmentsd;
        dictionary.getValue(keySegmentsPerPatch, patchSegmentsd);
        int patchSegments = patchSegmentsd;
        
        // Init tile provider manager
        ghoul::Dictionary textureInitDataDictionary;
        ghoul::Dictionary texturesDictionary;
        dictionary.getValue(keyTextureInitData, textureInitDataDictionary);
        dictionary.getValue(keyTextures, texturesDictionary);

        _tileProviderManager = std::make_shared<TileProviderManager>(
            texturesDictionary, textureInitDataDictionary);

        _chunkedLodGlobe = std::make_shared<ChunkedLodGlobe>(
            _ellipsoid, patchSegments, _tileProviderManager);
        _distanceSwitch.addSwitchValue(_chunkedLodGlobe, 1e12);

        // Add debug options - must be after chunkedLodGlobe has been created as it 
        // references its members
        addProperty(debugSelection);
        debugSelection.addOption("Show chunk edges", &_chunkedLodGlobe->debugOptions.showChunkEdges);
        debugSelection.addOption("Show chunk bounds", &_chunkedLodGlobe->debugOptions.showChunkBounds);
        debugSelection.addOption("Show chunk AABB", &_chunkedLodGlobe->debugOptions.showChunkAABB);

        debugSelection.addOption("Culling: Frustum", &_chunkedLodGlobe->debugOptions.doFrustumCulling);
        debugSelection.addOption("Culling: Horizon", &_chunkedLodGlobe->debugOptions.doHorizonCulling);

        debugSelection.addOption("Level by proj area (else distance)", &_chunkedLodGlobe->debugOptions.levelByProjAreaElseDistance);
        debugSelection.addOption("Level limited by available data", &_chunkedLodGlobe->debugOptions.limitLevelByAvailableHeightData);

        // Add all tile layers as being toggleable for each category
        for (int i = 0; i < LayeredTextures::NUM_TEXTURE_CATEGORIES;  i++){
            LayeredTextures::TextureCategory category = (LayeredTextures::TextureCategory) i;
            std::string categoryName = std::to_string(i+1) + ". " + LayeredTextures::TEXTURE_CATEGORY_NAMES[i];
            auto selection = std::make_unique<ReferencedBoolSelection>(categoryName, categoryName);
            
            auto& categoryProviders = _tileProviderManager->getLayerCategory(category);
            for (auto& provider : categoryProviders) {
                selection->addOption(provider.name, &provider.isActive);
            }
            selection->addOption(" - Blend tile levels - ", &_chunkedLodGlobe->blendProperties[category]);

            addProperty(selection.get());
            _categorySelections.push_back(std::move(selection));
        }

        addProperty(atmosphereEnabled);
        addProperty(_saveOrThrowCamera);
        addProperty(lodScaleFactor);
    }

    RenderableGlobe::~RenderableGlobe() {

    }

    bool RenderableGlobe::initialize() {
        for (auto& selection : _categorySelections) {
            selection->initialize();
        }
        debugSelection.initialize();

        return _distanceSwitch.initialize();
    }

    bool RenderableGlobe::deinitialize() {
        return _distanceSwitch.deinitialize();
    }

    bool RenderableGlobe::isReady() const {
        return _distanceSwitch.isReady();
    }

    void RenderableGlobe::render(const RenderData& data) {
        if (_saveOrThrowCamera.value()) {
            _saveOrThrowCamera.setValue(false);

            if (_chunkedLodGlobe->getSavedCamera() == nullptr) { // save camera
                LDEBUG("Saving snapshot of camera!");
                _chunkedLodGlobe->setSaveCamera(std::make_shared<Camera>(data.camera));
            }
            else { // throw camera
                LDEBUG("Throwing away saved camera!");
                _chunkedLodGlobe->setSaveCamera(nullptr);
            }
        }

        _distanceSwitch.render(data);
    }

    void RenderableGlobe::update(const UpdateData& data) {

        // set spice-orientation in accordance to timestamp
        //_chunkedLodGlobe->setStateMatrix(
        //    SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time));
        // We currently do not consider rotation anywhere in the rendering.
        // @TODO Consider rotation everywhere in the rendering (culling, splitting, camera, etc)
        _chunkedLodGlobe->setStateMatrix(glm::dmat3(1.0));
        _time = data.time;
        _distanceSwitch.update(data);

        _chunkedLodGlobe->lodScaleFactor = lodScaleFactor.value();
        _chunkedLodGlobe->atmosphereEnabled = atmosphereEnabled.value();

        _tileProviderManager->prerender();
    }

    glm::dvec3 RenderableGlobe::projectOnEllipsoid(glm::dvec3 position) {
        return _ellipsoid.geodeticSurfaceProjection(position);
    }

    glm::dvec3 RenderableGlobe::projectOnGlobeSurface(glm::dvec3 position) {
        
        Geodetic2 geodeticPosition = _ellipsoid.cartesianToGeodetic2(position);
        glm::dvec3 ellipsoidNormal = _ellipsoid.geodeticSurfaceNormal(geodeticPosition);
        int chunkLevel = 20;
        ChunkIndex chunkIdx = ChunkIndex(geodeticPosition, chunkLevel);
        GeodeticPatch patch = GeodeticPatch(chunkIdx);
        Geodetic2 geoDiffPatch = patch.getCorner(Quad::NORTH_EAST) - patch.getCorner(Quad::SOUTH_WEST);
        Geodetic2 geoDiffPoint = geodeticPosition - patch.getCorner(Quad::SOUTH_WEST);
        glm::vec2 uvPatch = glm::vec2(geoDiffPoint.lon / geoDiffPatch.lon, geoDiffPoint.lat / geoDiffPatch.lat);

        const auto& heightMapProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::HeightMaps);

        if (heightMapProviders.size() == 0)
            return _ellipsoid.geodeticSurfaceProjection(position);

        TileAndTransform tileAndTransform = TileSelector::getHighestResolutionTile(heightMapProviders[0].get(), chunkIdx);
        
        glm::vec2 transformedUv = tileAndTransform.uvTransform.uvOffset + tileAndTransform.uvTransform.uvScale * uvPatch;
        double sampledHeight = tileAndTransform.tile.sampleValueAsDouble(transformedUv);

        return _ellipsoid.geodeticSurfaceProjection(position) + ellipsoidNormal * (sampledHeight + 100);
    }

    double RenderableGlobe::getHeight(glm::dvec3 position) {

        Geodetic2 geodeticPosition = _ellipsoid.cartesianToGeodetic2(position);
        glm::dvec3 ellipsoidNormal = _ellipsoid.geodeticSurfaceNormal(geodeticPosition);
        int chunkLevel = 16;
        ChunkIndex chunkIdx = ChunkIndex(geodeticPosition, chunkLevel);
        GeodeticPatch patch = GeodeticPatch(chunkIdx);
        Geodetic2 geoDiffPatch = patch.getCorner(Quad::NORTH_EAST) - patch.getCorner(Quad::SOUTH_WEST);
        Geodetic2 geoDiffPoint = geodeticPosition - patch.getCorner(Quad::SOUTH_WEST);
        glm::vec2 uvPatch = glm::vec2(geoDiffPoint.lon / geoDiffPatch.lon, geoDiffPoint.lat / geoDiffPatch.lat);

        const auto& heightMapProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::HeightMaps);

        if (heightMapProviders.size() == 0)
            return 0;

        TileAndTransform tileAndTransform = TileSelector::getHighestResolutionTile(heightMapProviders[0].get(), chunkIdx);

        glm::vec2 transformedUv = tileAndTransform.uvTransform.uvOffset + tileAndTransform.uvTransform.uvScale * uvPatch;
        double sampledHeight = tileAndTransform.tile.sampleValueAsDouble(transformedUv);

        return sampledHeight;
    }

    std::shared_ptr<ChunkedLodGlobe> RenderableGlobe::chunkedLodGlobe() {
        return _chunkedLodGlobe;
    }

}  // namespace openspace

