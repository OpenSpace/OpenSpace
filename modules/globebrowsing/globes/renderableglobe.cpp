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

#include <modules/globebrowsing/globes/globemesh.h>

#include <modules/globebrowsing/other/threadpool.h>

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
    const std::string keyRadii = "Radii";
    const std::string keySegmentsPerPatch = "SegmentsPerPatch";
    const std::string keyTextures = "Textures";
    const std::string keyColorTextures = "ColorTextures";
    const std::string keyHeightMaps = "HeightMaps";
}



namespace openspace {


    RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
        : _tileProviderManager(std::shared_ptr<TileProviderManager>(new TileProviderManager))
        , _saveOrThrowCamera(properties::BoolProperty("saveOrThrowCamera", "saveOrThrowCamera"))
        , doFrustumCulling(properties::BoolProperty("doFrustumCulling", "doFrustumCulling"))
        , doHorizonCulling(properties::BoolProperty("doHorizonCulling", "doHorizonCulling"))
        , mergeInvisible(properties::BoolProperty("mergeInvisible", "mergeInvisible", true))
        , lodScaleFactor(properties::FloatProperty("lodScaleFactor", "lodScaleFactor", 10.0f, 0.0f, 100.0f))
        , initChunkVisible(properties::BoolProperty("initChunkVisible", "initChunkVisible", true))
        , renderSmallChunksFirst(properties::BoolProperty("renderSmallChunksFirst", "renderSmallChunksFirst", true))
    {
        
        setName("RenderableGlobe");
        
        addProperty(_saveOrThrowCamera);
        addProperty(doFrustumCulling);
        addProperty(doHorizonCulling);
        addProperty(mergeInvisible);
        addProperty(lodScaleFactor);
        addProperty(initChunkVisible);
        addProperty(renderSmallChunksFirst);
                
        doFrustumCulling.setValue(true);
        doHorizonCulling.setValue(true);
        renderSmallChunksFirst.setValue(true);

        


        // Read the radii in to its own dictionary
        Vec3 radii;
        double patchSegmentsd;

        dictionary.getValue(keyRadii, radii);
        // Ghoul can't read ints from lua dictionaries
        dictionary.getValue(keySegmentsPerPatch, patchSegmentsd);
        int patchSegments = patchSegmentsd;

        _ellipsoid = Ellipsoid(radii);

        
        setBoundingSphere(pss(_ellipsoid.averageRadius(), 0.0));
        

        ghoul::Dictionary texturesDictionary;
        dictionary.getValue(keyTextures, texturesDictionary);

        ghoul::Dictionary colorTexturesDictionary;
        texturesDictionary.getValue(keyColorTextures, colorTexturesDictionary);

        int minimumTextureSide = 1024;
        int minimumHeightmapSize = 64;
        int frameUntilFlushRequestQueue = 60;
        int cacheSize = 5000;

        
        // Create TileProviders for all color textures
        for (size_t i = 1; i < colorTexturesDictionary.size() + 1; i++)
        {
            std::string name, path;
            ghoul::Dictionary colorTextureDictionary =
                colorTexturesDictionary.value<ghoul::Dictionary>(std::to_string(i));
            colorTextureDictionary.getValue("Name", name);
            colorTextureDictionary.getValue("FilePath", path);
            
            std::shared_ptr<TileDataset> tileDataset = std::shared_ptr<TileDataset>(
                new TileDataset(path, minimumTextureSide));
                
            std::shared_ptr<ThreadPool> threadPool = std::shared_ptr<ThreadPool>(
                new ThreadPool(1));

            std::shared_ptr<AsyncTileDataProvider> tileReader = std::shared_ptr<AsyncTileDataProvider>(
                new AsyncTileDataProvider(tileDataset, threadPool));

            std::shared_ptr<TileProvider> colorTextureProvider = std::shared_ptr<TileProvider>(
                new TileProvider(tileReader, cacheSize, frameUntilFlushRequestQueue));

            _tileProviderManager->addColorTexture(name, colorTextureProvider);
        }

        ghoul::Dictionary heightMapsDictionary;
        texturesDictionary.getValue(keyHeightMaps, heightMapsDictionary);

        // Create TileProviders for all height maps
        for (size_t i = 1; i < heightMapsDictionary.size() + 1; i++)
        {
            std::string name, path;
            ghoul::Dictionary heightMapDictionary =
                heightMapsDictionary.value<ghoul::Dictionary>(std::to_string(i));
            heightMapDictionary.getValue("Name", name);
            heightMapDictionary.getValue("FilePath", path);

            std::shared_ptr<TileDataset> tileDataset = std::shared_ptr<TileDataset>(
                new TileDataset(path, minimumHeightmapSize));

            std::shared_ptr<ThreadPool> threadPool = std::shared_ptr<ThreadPool>(
                new ThreadPool(1));

            std::shared_ptr<AsyncTileDataProvider> tileReader = std::shared_ptr<AsyncTileDataProvider>(
                new AsyncTileDataProvider(tileDataset, threadPool));

            std::shared_ptr<TileProvider> heightMapProvider = std::shared_ptr<TileProvider>(
                new TileProvider(tileReader, cacheSize, frameUntilFlushRequestQueue));


            _tileProviderManager->addHeightMap(name, heightMapProvider);
        }
        
        _chunkedLodGlobe = std::shared_ptr<ChunkedLodGlobe>(
            new ChunkedLodGlobe(_ellipsoid, patchSegments, _tileProviderManager));

        _distanceSwitch.addSwitchValue(_chunkedLodGlobe, 1e12);
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
        if (_saveOrThrowCamera.value()) {
            _saveOrThrowCamera.setValue(false);

            if (_chunkedLodGlobe->getSavedCamera() == nullptr) { // save camera
                LDEBUG("Saving snapshot of camera!");
                _chunkedLodGlobe->setSaveCamera(new Camera(data.camera));
            }
            else { // throw camera
                LDEBUG("Throwing away saved camera!");
                _chunkedLodGlobe->setSaveCamera(nullptr);
            }
        }
        _chunkedLodGlobe->doFrustumCulling = doFrustumCulling.value();
        _chunkedLodGlobe->doHorizonCulling = doHorizonCulling.value();
        _chunkedLodGlobe->mergeInvisible = mergeInvisible.value();
        _chunkedLodGlobe->lodScaleFactor= lodScaleFactor.value();
        _chunkedLodGlobe->initChunkVisible = initChunkVisible.value();
        _distanceSwitch.render(data);
    }

    void RenderableGlobe::update(const UpdateData& data) {
        _time = data.time;
        _distanceSwitch.update(data);
    }

    glm::dvec3 RenderableGlobe::geodeticSurfaceProjection(glm::dvec3 position) {
        return _ellipsoid.geodeticSurfaceProjection(position);
    }


}  // namespace openspace
