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
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/pointglobe.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/globebrowsingmodule.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <fstream>
#include <cstdlib>

namespace {
    constexpr const char* _loggerCat = "RenderableGlobe";
    constexpr const char* keyFrame = "Frame";
    constexpr const char* keyRadii = "Radii";
    constexpr const char* keySegmentsPerPatch = "SegmentsPerPatch";
    constexpr const char* keyLayers = "Layers";
    constexpr const char* keyShadowGroup = "ShadowGroup";
    constexpr const char* keyShadowSource = "Source";
    constexpr const char* keyShadowCaster = "Caster";
    constexpr const char* keyLabels = "Labels";
    constexpr const char* keyLabelsFileName = "FileName";

    constexpr int8_t CurrentCacheVersion = 1;

    static const openspace::properties::Property::PropertyInfo SaveOrThrowInfo = {
        "SaveOrThrowCamera",
        "Save or throw camera",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ShowChunkEdgeInfo = {
        "ShowChunkEdges",
        "Show chunk edges",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ShowChunkBoundsInfo = {
        "ShowChunkBounds",
        "Show chunk bounds",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ShowChunkAABBInfo = {
        "ShowChunkAABB",
        "Show chunk AABB",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo HeightResolutionInfo = {
        "ShowHeightResolution",
        "Show height resolution",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo HeightIntensityInfo = {
        "ShowHeightIntensities",
        "Show height intensities",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo FrustumCullingInfo = {
        "PerformFrustumCulling",
        "Perform frustum culling",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo HorizonCullingInfo = {
        "PerformHorizonCulling",
        "Perform horizon culling",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo LevelProjectedAreaInfo = {
        "LevelByProjectedAreaElseDistance",
        "Level by projected area (else distance)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ResetTileProviderInfo = {
        "ResetTileProviders",
        "Reset tile providers",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo CollectStatsInfo = {
        "CollectStats",
        "Collect stats",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo LimitLevelInfo = {
        "LimitLevelByAvailableData",
        "Limit level by available data",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ModelSpaceRenderingInfo = {
        "ModelSpaceRenderingCutoffLevel",
        "Model Space Rendering Cutoff Level",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo PerformShadingInfo = {
        "PerformShading",
        "Perform shading",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo AtmosphereInfo = {
        "Atmosphere",
        "Atmosphere",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo AccurateNormalsInfo = {
        "UseAccurateNormals",
        "Use Accurate Normals",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EclipseInfo = {
        "Eclipse",
        "Eclipse",
        "Enables/Disable Eclipse shadows"
    };

    static const openspace::properties::Property::PropertyInfo EclipseHardShadowsInfo = {
        "EclipseHardShadows",
        "Eclipse Hard Shadows",
        "Enables the rendering of eclipse shadows using hard shadows"
    };

    static const openspace::properties::Property::PropertyInfo LodScaleFactorInfo = {
        "LodScaleFactor",
        "Level of Detail Scale Factor",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo CameraMinHeightInfo = {
        "CameraMinHeight",
        "Camera Minimum Height",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo OrenNayarRoughnessInfo = {
        "OrenNayarRoughness",
        "orenNayarRoughness",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo LabelsInfo = {
        "Labels",
        "Labels Enabled",
        "Enables and disables the rendering of labels on the globe surface from "
        "the csv label file"
    };

    static const openspace::properties::Property::PropertyInfo LabelsFontSizeInfo = {
        "LabelsFontSize",
        "Labels Font Size",
        "Font size for the rendering labels. This is different fromt text size."
    };

    static const openspace::properties::Property::PropertyInfo LabelsMaxSizeInfo = {
        "LabelsMaxSize",
        "Labels Maximum Text Size",
        "Maximum label size"
    };

    static const openspace::properties::Property::PropertyInfo LabelsMinSizeInfo = {
        "LabelsMinSize",
        "Labels Minimum Text Size",
        "Minimum label size"
    };

    static const openspace::properties::Property::PropertyInfo LabelsSizeInfo = {
        "LabelsSize",
        "Labels Size",
        "Labels Size"
    };

    static const openspace::properties::Property::PropertyInfo LabelsMinHeightInfo = {
        "LabelsMinHeight",
        "Labels Minimum Height",
        "Labels Minimum Height"
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
        FloatProperty(OrenNayarRoughnessInfo, 0.f, 0.f, 1.f),
        BoolProperty(LabelsInfo, false),
        IntProperty(LabelsFontSizeInfo, 30, 1, 50),
        IntProperty(LabelsMaxSizeInfo, 300, 10, 1000),
        IntProperty(LabelsMinSizeInfo, 30, 1, 100),
        FloatProperty(LabelsSizeInfo, 2.5, 0, 30),
        FloatProperty(LabelsMinHeightInfo, 100.0, 0.0, 10000.0)
    })
    , _debugPropertyOwner({ "Debug" })
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

    _chunkedLodGlobe = std::make_shared<ChunkedLodGlobe>(
        *this,
        patchSegments,
        _layerManager,
        _ellipsoid
    );
    //_pointGlobe = std::make_shared<PointGlobe>(*this);

    _distanceSwitch.addSwitchValue(_chunkedLodGlobe);
    //_distanceSwitch.addSwitchValue(_pointGlobe);

    addProperty(_generalProperties.atmosphereEnabled);
    addProperty(_generalProperties.performShading);
    addProperty(_generalProperties.useAccurateNormals);
    addProperty(_generalProperties.eclipseShadowsEnabled);
    addProperty(_generalProperties.eclipseHardShadows);
    addProperty(_generalProperties.lodScaleFactor);
    addProperty(_generalProperties.cameraMinHeight);
    addProperty(_generalProperties.orenNayarRoughness);
    addProperty(_generalProperties.labelsEnabled);
    addProperty(_generalProperties.labelsFontSize);
    addProperty(_generalProperties.labelsSize);
    addProperty(_generalProperties.labelsMinHeight);

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
        _chunkedLodGlobe->notifyShaderRecompilation();
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

    // Reads labels' file and build cache file if necessary
    _labelsDataPresent = false;
    ghoul::Dictionary labelsDictionary;
    bool successLabels = dictionary.getValue(keyLabels, labelsDictionary);
    if (successLabels) {
        std::string labelsFile;
        successLabels = labelsDictionary.getValue(keyLabelsFileName, labelsFile);
        // DEBUG:
        //std::cout << "========== File Name: " << absPath(labelsFile) << " ===========" << std::endl;
        if (successLabels) {
            _labelsDataPresent = true;
            bool loadSuccess = loadLabelsData(absPath(labelsFile));
            if (loadSuccess) {
                _generalProperties.labelsEnabled.set(true);
                _chunkedLodGlobe->setLabels(_labels);
                _chunkedLodGlobe->enableLabelsRendering(true);
                _generalProperties.labelsEnabled.onChange([&]() {
                    _chunkedLodGlobe->enableLabelsRendering(_generalProperties.labelsEnabled);
                });
                _generalProperties.labelsFontSize.onChange([&]() {
                    _chunkedLodGlobe->setFontSize(_generalProperties.labelsFontSize);
                });
                _generalProperties.labelsSize.onChange([&]() {
                    _chunkedLodGlobe->setLabelsSize(_generalProperties.labelsSize);
                });
                _generalProperties.labelsMinHeight.onChange([&]() {
                    _chunkedLodGlobe->setLabelsMinHeight(_generalProperties.labelsMinHeight);
                });
            }
        }
    }
}

void RenderableGlobe::initializeGL() {
    _layerManager->initialize();

    _layerManager->update();

    _distanceSwitch.initializeGL();

    // Recompile the shaders directly so that it is not done the first time the render
    // function is called.
    _chunkedLodGlobe->recompileShaders();

    if (_labelsDataPresent) {
        _chunkedLodGlobe->initializeFonts();
    }
}

void RenderableGlobe::deinitializeGL() {
    _distanceSwitch.deinitializeGL();

    _layerManager->deinitialize();
}

bool RenderableGlobe::isReady() const {
    return true;
}

void RenderableGlobe::render(const RenderData& data, RendererTasks& renderTask) {
    bool statsEnabled = _debugProperties.collectStats.value();
    _chunkedLodGlobe->stats.setEnabled(statsEnabled);

    if (_enabled) {
        if (_debugProperties.saveOrThrowCamera.value()) {
            _debugProperties.saveOrThrowCamera.setValue(false);

            if (savedCamera() == nullptr) { // save camera
                setSaveCamera(std::make_shared<Camera>(data.camera));
            }
            else { // throw camera
                setSaveCamera(nullptr);
            }
        }
        _distanceSwitch.render(data, renderTask);
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

void RenderableGlobe::setSaveCamera(std::shared_ptr<Camera> camera) {
    _savedCamera = camera;
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

bool RenderableGlobe::loadLabelsData(const std::string& file) {
    bool success = true;
    if (_labelsDataPresent) {
        std::string cachedFile = FileSys.cacheManager()->cachedFilename(
            ghoul::filesystem::File(file),
            "RenderableGlobe|" + identifier(),
            ghoul::filesystem::CacheManager::Persistent::Yes
        );

        bool hasCachedFile = FileSys.fileExists(cachedFile);
        if (hasCachedFile) {
            LINFO(fmt::format(
                "Cached file '{}' used for labels file '{}'",
                cachedFile,
                file
            ));

            success = loadCachedFile(cachedFile);
            if (success) {
                return true;
            }
            else {
                FileSys.cacheManager()->removeCacheFile(file);
                // Intentional fall-through to the 'else' to generate the cache
                // file for the next run
            }
        }
        else {
            LINFO(fmt::format("Cache for labels file '{}' not found", file));
        }
        LINFO(fmt::format("Loading labels file '{}'", file));

        success = readLabelsFile(file);
        if (!success) {
            return false;
        }

        success &= saveCachedFile(cachedFile);
    }
    return success;
}

bool RenderableGlobe::readLabelsFile(const std::string& file) {
    try {
        std::fstream csvLabelFile(file);
        if (!csvLabelFile.good()) {
            LERROR(fmt::format("Failed to open labels file '{}'", file));
            return false;
        }
        if (csvLabelFile.is_open()) {
            char line[4096];
            _labels.labelsArray.clear();
            while (!csvLabelFile.eof()) {
                csvLabelFile.getline(line, 4090);
                if (strnlen(line, 4090) > 10) {
                    LabelEntry lEntry;
                    char *token = strtok(line, ",");
                    // First line is just the Header
                    if (strcmp(token, "Feature_Name") == 0) {
                        continue;
                    }
                    strncpy(lEntry.feature, token, 256);
                    strtok(NULL, ","); // Target is not used
                    lEntry.diameter = static_cast<float>(atof(strtok(NULL, ",")));
                    lEntry.latitude = static_cast<float>(atof(strtok(NULL, ",")));
                    lEntry.longitude = static_cast<float>(atof(strtok(NULL, ",")));
                    char * coordinateSystem = strtok(NULL, ",");

                    if (strstr(coordinateSystem, "West") != NULL) {
                        lEntry.longitude = 360.0f - lEntry.longitude;
                    }

                    GlobeBrowsingModule* _globeBrowsingModule =
                        OsEng.moduleEngine().module<openspace::GlobeBrowsingModule>();
                    lEntry.geoPosition = _globeBrowsingModule->cartesianCoordinatesFromGeo(
                        *this,
                        lEntry.latitude,
                        lEntry.longitude,
                        lEntry.diameter
                    );

                    _labels.labelsArray.push_back(lEntry);
                }
            }
            return true;
        }
        else {
            return false;
        }
    }
    catch (const std::fstream::failure& e) {
        LERROR(fmt::format("Failed reading labels file '{}'", file));
        LERROR(e.what());
        return false;
    }
}

bool RenderableGlobe::loadCachedFile(const std::string& file) {
    std::ifstream fileStream(file, std::ifstream::binary);
    if (fileStream.good()) {
        int8_t version = 0;
        fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
        if (version != CurrentCacheVersion) {
            LINFO("The format of the cached file has changed: deleting old cache");
            fileStream.close();
            FileSys.deleteFile(file);
            return false;
        }

        int32_t nValues = 0;
        fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        _labels.labelsArray.resize(nValues);
        
        fileStream.read(reinterpret_cast<char*>(&_labels.labelsArray[0]),
            nValues * sizeof(_labels.labelsArray[0]));
      
        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
        return false;
    }
}

bool RenderableGlobe::saveCachedFile(const std::string& file) const {
    
    std::ofstream fileStream(file, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t));

        int32_t nValues = static_cast<int32_t>(_labels.labelsArray.size());
        if (nValues == 0) {
            LERROR("Error writing cache: No values were loaded");
            return false;
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

        size_t nBytes = nValues * sizeof(_labels.labelsArray[0]);
        fileStream.write(reinterpret_cast<const char*>(&_labels.labelsArray[0]), nBytes);

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return false;
    }
}

} // namespace openspace::globebrowsing
