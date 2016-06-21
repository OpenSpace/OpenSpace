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
        , doFrustumCulling(properties::BoolProperty("doFrustumCulling", "doFrustumCulling"))
        , doHorizonCulling(properties::BoolProperty("doHorizonCulling", "doHorizonCulling"))
        , mergeInvisible(properties::BoolProperty("mergeInvisible", "mergeInvisible", true))
        , lodScaleFactor(properties::FloatProperty("lodScaleFactor", "lodScaleFactor", 5.0f, 1.0f, 50.0f))
        , initChunkVisible(properties::BoolProperty("initChunkVisible", "initChunkVisible", true))
        , renderSmallChunksFirst(properties::BoolProperty("renderSmallChunksFirst", "renderSmallChunksFirst", true))
        , chunkHeight(properties::FloatProperty("chunkHeight", "chunkHeight", 8700.0f, 0.0f, 8700.0f))

        , _baseLayersSelection(properties::SelectionProperty("Base Layers", "Base Layers"))
        , _nightLayersSelection(properties::SelectionProperty("Night Textures", "Night Textures"))
        , _heightMapsSelection(properties::SelectionProperty("Height Maps", "Height Maps"))
        , _waterMasksSelection(properties::SelectionProperty("Water Masks", "Water Masks"))
        , _overlaysSelection(properties::SelectionProperty("Overlays", "Overlays"))

        , debugSelection(properties::SelectionProperty("Debug", "Debug"))

        , blendHeightMap(properties::BoolProperty("blendHeightMap", "blendHeightMap", true))
        , blendColorMap(properties::BoolProperty("blendColorMap", "blendColorMap", true))
        , blendNightTexture(properties::BoolProperty("blendNightTexture", "blendNightTexture", true))
        , blendOverlay(properties::BoolProperty("blendOverlay", "blendOverlay", true))
        , blendWaterMask(properties::BoolProperty("blendWaterMask", "blendWaterMask", true))
        , atmosphereEnabled(properties::BoolProperty("atmosphereEnabled", "atmosphereEnabled", false))
        , levelByProjArea(properties::BoolProperty("levelByProjArea", "levelByProjArea", true))
        , limitLevelByAvailableHeightData(properties::BoolProperty("limitLevelByAvailableHeightData", "limitLevelByAvailableHeightData", true))
    {
        setName("RenderableGlobe");
        
        addProperty(_saveOrThrowCamera);
        addProperty(doFrustumCulling);
        addProperty(doHorizonCulling);
        addProperty(mergeInvisible);
        addProperty(lodScaleFactor);
        addProperty(initChunkVisible);
        addProperty(renderSmallChunksFirst);
        addProperty(chunkHeight);

        addProperty(_baseLayersSelection);
        addProperty(_nightLayersSelection);
        addProperty(_heightMapsSelection);
        addProperty(_waterMasksSelection);
        addProperty(_overlaysSelection);

        addDebugOptions();

        addProperty(blendHeightMap);
        addProperty(blendColorMap);
        addProperty(blendNightTexture);
        addProperty(blendOverlay);
        addProperty(blendWaterMask);
        addProperty(atmosphereEnabled);


        addProperty(levelByProjArea);
        addProperty(limitLevelByAvailableHeightData);

        doFrustumCulling.setValue(true);
        doHorizonCulling.setValue(true);
        renderSmallChunksFirst.setValue(true);

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
        _tileProviderManager = std::shared_ptr<TileProviderManager>(
            new TileProviderManager(texturesDictionary, textureInitDataDictionary));

        addToggleLayerProperties(LayeredTextures::ColorTextures, _baseLayersSelection);
        addToggleLayerProperties(LayeredTextures::NightTextures, _nightLayersSelection);
        addToggleLayerProperties(LayeredTextures::HeightMaps, _heightMapsSelection);
        addToggleLayerProperties(LayeredTextures::WaterMasks, _waterMasksSelection);
        addToggleLayerProperties(LayeredTextures::Overlays, _overlaysSelection);

        _baseLayersSelection.onChange(std::bind(&RenderableGlobe::baseLayerSelectionChanged, this));
        _nightLayersSelection.onChange(std::bind(&RenderableGlobe::nightLayersSelectionChanged, this));
        _heightMapsSelection.onChange(std::bind(&RenderableGlobe::heightMapsSelectionChanged, this));
        _waterMasksSelection.onChange(std::bind(&RenderableGlobe::waterMasksSelectionChanged, this));
        _overlaysSelection.onChange(std::bind(&RenderableGlobe::overlaysSelectionChanged, this));

        _chunkedLodGlobe = std::shared_ptr<ChunkedLodGlobe>(
            new ChunkedLodGlobe(_ellipsoid, patchSegments, _tileProviderManager));        

        _distanceSwitch.addSwitchValue(_chunkedLodGlobe, 1e12);
    }

    RenderableGlobe::~RenderableGlobe() {

    }

    void RenderableGlobe::addToggleLayerProperties(
        LayeredTextures::TextureCategory category,
        properties::SelectionProperty& dest)
    {
        auto& categoryProviders = _tileProviderManager->getLayerCategory(category);
        for (size_t i = 0; i < categoryProviders.size(); i++) {
            std::string name = categoryProviders[i].name;
            dest.addOption( { static_cast<int>(i), name });
        }
    }

    void RenderableGlobe::initializeToggleLayerProperties(
        LayeredTextures::TextureCategory category,
        properties::SelectionProperty& selectionProperty)
    {
        std::vector<int> enabledIndices;
        auto& categoryProviders = _tileProviderManager->getLayerCategory(category);
        for (size_t i = 0; i < categoryProviders.size(); i++) {
            if (categoryProviders[i].isActive)
                enabledIndices.push_back(i);
        }
        selectionProperty.setValue(enabledIndices);
    }


    bool RenderableGlobe::initialize() {
        initializeToggleLayerProperties(LayeredTextures::ColorTextures, _baseLayersSelection);
        initializeToggleLayerProperties(LayeredTextures::NightTextures, _nightLayersSelection);
        initializeToggleLayerProperties(LayeredTextures::HeightMaps, _heightMapsSelection);
        initializeToggleLayerProperties(LayeredTextures::WaterMasks, _waterMasksSelection);
        initializeToggleLayerProperties(LayeredTextures::Overlays, _overlaysSelection);

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

        _chunkedLodGlobe->doFrustumCulling = doFrustumCulling.value();
        _chunkedLodGlobe->doHorizonCulling = doHorizonCulling.value();
        _chunkedLodGlobe->mergeInvisible = mergeInvisible.value();
        _chunkedLodGlobe->lodScaleFactor = lodScaleFactor.value();
        _chunkedLodGlobe->initChunkVisible = initChunkVisible.value();
        _chunkedLodGlobe->chunkHeight = chunkHeight.value();

        _chunkedLodGlobe->blendProperties[LayeredTextures::HeightMaps] = blendHeightMap.value();
        _chunkedLodGlobe->blendProperties[LayeredTextures::ColorTextures] = blendColorMap.value();
        _chunkedLodGlobe->blendProperties[LayeredTextures::NightTextures] = blendNightTexture.value();
        _chunkedLodGlobe->blendProperties[LayeredTextures::Overlays] = blendOverlay.value();
        _chunkedLodGlobe->blendProperties[LayeredTextures::WaterMasks] = blendWaterMask.value();
        _chunkedLodGlobe->atmosphereEnabled = atmosphereEnabled.value();

        _chunkedLodGlobe->levelByProjArea = levelByProjArea.value();
        _chunkedLodGlobe->limitLevelByAvailableHeightData = limitLevelByAvailableHeightData.value();

        _tileProviderManager->prerender();
    }

    glm::dvec3 RenderableGlobe::geodeticSurfaceProjection(glm::dvec3 position) {
        return _ellipsoid.geodeticSurfaceProjection(position);
    }

    std::shared_ptr<ChunkedLodGlobe> RenderableGlobe::chunkedLodGlobe() {
        return _chunkedLodGlobe;
    }

    void RenderableGlobe::selectionChanged(
        properties::SelectionProperty selectionProperty,
        LayeredTextures::TextureCategory textureCategory)
    {
        const std::vector<int>& selectedIndices = selectionProperty;
        auto& category = _tileProviderManager->getLayerCategory(textureCategory);
        // First inactivate all of them
        for (size_t i = 0; i < category.size(); i++) {
            category[i].isActive = false;
        }
        // Activate the selected ones
        for (size_t i = 0; i < selectedIndices.size(); i++){
            category[selectedIndices[i]].isActive = true;
        }
    }

    void RenderableGlobe::baseLayerSelectionChanged()
    {
        selectionChanged(_baseLayersSelection, LayeredTextures::ColorTextures);
    }

    void RenderableGlobe::nightLayersSelectionChanged()
    {
        selectionChanged(_nightLayersSelection, LayeredTextures::NightTextures);
    }

    void RenderableGlobe::heightMapsSelectionChanged()
    {
        selectionChanged(_heightMapsSelection, LayeredTextures::HeightMaps);
    }

    void RenderableGlobe::waterMasksSelectionChanged()
    {
        selectionChanged(_waterMasksSelection, LayeredTextures::WaterMasks);
    }

    void RenderableGlobe::overlaysSelectionChanged()
    {
        selectionChanged(_overlaysSelection, LayeredTextures::Overlays);
    }

    void RenderableGlobe::addDebugOptions() {
        addProperty(debugSelection);

        // Add options (GUI value initialized to false)
        debugSelection.addOption({ 0, "Show chunk edges" });
        debugSelection.addOption({ 1, "Show chunk bounds" });
        debugSelection.addOption({ 2, "Show chunk AABB" });
        

        // Add callback
        debugSelection.onChange([this]() {
            int nOptions = this->debugSelection.options().size();
            for (int i = 0; i < nOptions; ++i) {
                this->setDebugOption(i, false);
            }

            const std::vector<int>& selectedIndices = this->debugSelection;
            for (auto selectedIndex : selectedIndices) {
                this->setDebugOption(selectedIndex, true);
            }
        });
    }

    void RenderableGlobe::setDebugOption(size_t index, bool value) {
        switch (index) {
        case 0: _chunkedLodGlobe->showChunkEdges = value; break;
        case 1: _chunkedLodGlobe->showChunkBounds = value; break;
        case 2: _chunkedLodGlobe->showChunkAABB = value; break;
        }
    }
}  // namespace openspace
