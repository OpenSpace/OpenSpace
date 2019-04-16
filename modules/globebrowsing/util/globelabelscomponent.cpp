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

#include <modules/globebrowsing/util/globelabelscomponent.h>
#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/programobject.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <fstream>
#include <cstdlib>
#include <locale>

namespace {
    constexpr const char* keyLabels = "Labels";
    constexpr const char* keyLabelsFileName = "FileName";

    constexpr const char* _loggerCat = "GlobeLabels";

    constexpr int8_t CurrentCacheVersion = 1;

    constexpr openspace::properties::Property::PropertyInfo LabelsInfo = {
        "Labels",
        "Labels Enabled",
        "Enables and disables the rendering of labels on the globe surface from "
        "the csv label file"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsEnableInfo = {
        "Enable",
        "Enable",
        "Enables and disables labels' rendering from the asset file."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsFontSizeInfo = {
        "LabelsFontSize",
        "Labels Font Size",
        "Font size for the rendering labels. This is different fromt text size."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsMaxSizeInfo = {
        "LabelsMaxSize",
        "Labels Maximum Text Size",
        "Maximum label size"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsMinSizeInfo = {
        "LabelsMinSize",
        "Labels Minimum Text Size",
        "Minimum label size"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsSizeInfo = {
        "LabelsSize",
        "Labels Size",
        "Labels Size"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsMinHeightInfo = {
        "LabelsMinHeight",
        "Labels Minimum Height",
        "Labels Minimum Height"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsColorInfo = {
        "LabelsColor",
        "Labels Color",
        "Labels Color"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsFadeInStartingDistanceInfo = {
        "FadeInStartingDistance",
        "Fade In Starting Distance for Labels",
        "Fade In Starting Distance for Labels"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsFadeOutStartingDistanceInfo = {
        "FadeOutStartingDistance",
        "Fade Out Starting Distance for Labels",
        "Fade Out Starting Distance for Labels"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsFadeInEnabledInfo = {
        "LabelsFadeInEnabled",
        "Labels fade In enabled",
        "Labels fade In enabled"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsFadeOutEnabledInfo = {
        "LabelsFadeOutEnabled",
        "Labels fade Out enabled",
        "Labels fade Out enabled"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsDisableCullingEnabledInfo = {
        "LabelsDisableCullingEnabled",
        "Labels culling disabled",
        "Labels culling disabled"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelsDistanceEPSInfo = {
        "LabelsDistanceEPS",
        "Labels culling distance from globe's center",
        "Labels culling distance from globe's center"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelAlignmentOptionInfo = {
        "LabelAlignmentOption",
        "Label Alignment Option",
        "Labels are aligned horizontally or circularly related to the planet."
    };
} // namespace

namespace openspace {

documentation::Documentation GlobeLabelsComponent::Documentation() {
    using namespace documentation;
    return {
        "GlobeLabels Component",
        "globebrowsing_globelabelscomponent",
    {
        {
            LabelsInfo.identifier,
            new BoolVerifier,
            Optional::Yes,
            LabelsInfo.description
        },
        {
            LabelsEnableInfo.identifier,
            new BoolVerifier,
            Optional::Yes,
            LabelsEnableInfo.description
        },
        {
            LabelsFontSizeInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsFontSizeInfo.description
        },
        {
            LabelsMaxSizeInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsMaxSizeInfo.description
        },
        {
            LabelsMinSizeInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsMinSizeInfo.description
        },
        {
            LabelsSizeInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsSizeInfo.description
        },
        {
            LabelsMinHeightInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsMinHeightInfo.description
        },
        {
            LabelsColorInfo.identifier,
            new Vector4Verifier<float>(),
            Optional::Yes,
            LabelsColorInfo.description
        },
        {
            LabelsFadeInStartingDistanceInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsFadeInStartingDistanceInfo.description
        },
        {
            LabelsFadeOutStartingDistanceInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsFadeOutStartingDistanceInfo.description
        },
        {
            LabelsFadeInEnabledInfo.identifier,
            new BoolVerifier,
            Optional::Yes,
            LabelsFadeInEnabledInfo.description
        },
        {
            LabelsFadeOutEnabledInfo.identifier,
            new BoolVerifier,
            Optional::Yes,
            LabelsFadeOutEnabledInfo.description
        },
        {
            LabelsDisableCullingEnabledInfo.identifier,
            new BoolVerifier,
            Optional::Yes,
            LabelsDisableCullingEnabledInfo.description
        },
        {
            LabelsDistanceEPSInfo.identifier,
            new DoubleVerifier,
            Optional::Yes,
            LabelsDistanceEPSInfo.description
        },
        {
            LabelAlignmentOptionInfo.identifier,
            new StringVerifier,
            Optional::Yes,
            LabelAlignmentOptionInfo.description
        },
    }
    };
}

GlobeLabelsComponent::GlobeLabelsComponent()
    : properties::PropertyOwner({ "Labels" })
    , _labelsEnabled(LabelsInfo, false)
    , _labelsFontSize(LabelsFontSizeInfo, 30, 1, 120)
    , _labelsMaxSize(LabelsMaxSizeInfo, 300, 10, 1000)
    , _labelsMinSize(LabelsMinSizeInfo, 4, 1, 100)
    , _labelsSize(LabelsSizeInfo, 2.5, 0, 30)
    , _labelsMinHeight(LabelsMinHeightInfo, 100.0, 0.0, 10000.0)
    , _labelsColor(LabelsColorInfo, glm::vec4(1.f, 1.f, 0.f, 1.f),
        glm::vec4(0.f), glm::vec4(1.f))
    , _labelsFadeInDist(LabelsFadeInStartingDistanceInfo, 1E6, 1E3, 1E8)
    , _labelsFadeOutDist(LabelsFadeOutStartingDistanceInfo, 1E4, 1, 1E7)
    , _labelsFadeInEnabled(LabelsFadeInEnabledInfo, true)
    , _labelsFadeOutEnabled(LabelsFadeOutEnabledInfo, true)
    , _labelsDisableCullingEnabled(LabelsDisableCullingEnabledInfo, false)
    , _labelsDistaneEPS(LabelsDistanceEPSInfo, 100000.f, 1000.f, 10000000.f)
    , _labelAlignmentOption(LabelAlignmentOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    addProperty(_labelsEnabled);
    addProperty(_labelsFontSize);
    addProperty(_labelsSize);
    addProperty(_labelsMinHeight);
    _labelsColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_labelsColor);
    addProperty(_labelsFadeInDist);
    addProperty(_labelsFadeOutDist);
    addProperty(_labelsMinSize);
    addProperty(_labelsFadeInEnabled);
    addProperty(_labelsFadeOutEnabled);
    addProperty(_labelsDisableCullingEnabled);
    addProperty(_labelsDistaneEPS);

    _labelAlignmentOption.addOption(0, "Horizontally");
    _labelAlignmentOption.addOption(1, "Circularly");
    _labelAlignmentOption = Horizontally;
    addProperty(_labelAlignmentOption);
}

void GlobeLabelsComponent::initialize(const ghoul::Dictionary& dictionary, 
    globebrowsing::RenderableGlobe* globe, std::shared_ptr<ghoul::fontrendering::Font> font)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "GlobeLabelsComponent"
    );
        
    _globe = globe;

    // Reads labels' file and build cache file if necessary
    _labelsDataPresent = false;
    if (!dictionary.empty()) {
        std::string labelsFile;
        bool successLabels = dictionary.getValue(keyLabelsFileName, labelsFile);
        if (successLabels) {
            _labelsDataPresent = true;
            bool loadSuccess = loadLabelsData(absPath(labelsFile));
            if (loadSuccess) {
                if (dictionary.hasKey(LabelsEnableInfo.identifier)) {
                    // In case of the label's dic is present but is disabled
                    _labelsEnabled = dictionary.value<bool>(
                        LabelsEnableInfo.identifier
                        );
                }
                else {
                    // Is the labels dic is enable in the configuration file,
                    // enables the label automatically.
                    _labelsEnabled.set(true);
                }
         
                if (dictionary.hasKey(LabelsFontSizeInfo.identifier)) {
                    float fontSize = dictionary.value<float>(
                        LabelsFontSizeInfo.identifier
                    );
                    _labelsFontSize.set(fontSize);
                }

                if (dictionary.hasKey(LabelsSizeInfo.identifier)) {
                    float size = static_cast<float>(
                        dictionary.value<double>(LabelsSizeInfo.identifier)
                    );
                    _labelsSize.set(size);
                }

                if (dictionary.hasKey(LabelsMinHeightInfo.identifier)) {
                    float height = dictionary.value<float>(LabelsMinHeightInfo.identifier);
                    _labelsMinHeight.set(height);
                }

                if (dictionary.hasKey(LabelsColorInfo.identifier)) {
                    _labelsColor = dictionary.value<glm::vec4>(
                        LabelsColorInfo.identifier
                    );
                }

                if (dictionary.hasKey(LabelsFadeInEnabledInfo.identifier)) {
                    bool enabled = dictionary.value<bool>(
                        LabelsFadeInEnabledInfo.identifier
                    );
                    _labelsFadeInEnabled.set(enabled);
                }

                if (dictionary.hasKey(LabelsFadeInStartingDistanceInfo.identifier)) {
                    float dist = dictionary.value<float>(
                        LabelsFadeInStartingDistanceInfo.identifier
                    );
                    _labelsFadeInDist.set(dist);
                }

                if (dictionary.hasKey(LabelsFadeOutEnabledInfo.identifier)) {
                    _labelsFadeInEnabled = dictionary.value<bool>(
                        LabelsFadeOutEnabledInfo.identifier
                    );
                }

                if (dictionary.hasKey(LabelsFadeOutStartingDistanceInfo.identifier)) {
                    float dist = dictionary.value<float>(
                        LabelsFadeOutStartingDistanceInfo.identifier
                    );
                    _labelsFadeOutDist.set(dist);
                }

                if (dictionary.hasKey(LabelsMinSizeInfo.identifier)) {
                    int size = static_cast<int>(
                        dictionary.value<float>(LabelsMinSizeInfo.identifier)
                    );
                    _labelsMinSize.set(size);
                }

                if (dictionary.hasKey(LabelsMaxSizeInfo.identifier)) {
                    int size = static_cast<int>(
                        dictionary.value<float>(LabelsMaxSizeInfo.identifier)
                    );
                    _labelsMaxSize.set(size);
                }

                if (dictionary.hasKey(LabelsDisableCullingEnabledInfo.identifier)) {
                    bool disabled = dictionary.value<bool>(
                        LabelsDisableCullingEnabledInfo.identifier
                    );
                    _labelsDisableCullingEnabled.set(disabled);
                }

                if (dictionary.hasKey(LabelsDistanceEPSInfo.identifier)) {
                    float dist = static_cast<float>(
                        dictionary.value<double>(LabelsDistanceEPSInfo.identifier)
                    );
                    _labelsDistaneEPS.set(dist);
                }

                if (dictionary.hasKey(LabelAlignmentOptionInfo.identifier)) {
                    std::string labelAlignment = 
                        dictionary.value<std::string>(LabelAlignmentOptionInfo.identifier);
                    if (labelAlignment == "Horizontally") {
                        _labelAlignmentOption = Horizontally;
                    }
                    else {
                        _labelAlignmentOption = Circularly;
                    }
                }

                _font = font;
                initializeFonts();
            }
        }
    }
}

bool GlobeLabelsComponent::initializeGL() {
    return true;
}

void GlobeLabelsComponent::initializeFonts() {
    if (_font == nullptr) {
        _font = openspace::global::fontManager.font(
            "Mono",
            static_cast<float>(_labelsFontSize),
            ghoul::fontrendering::FontManager::Outline::Yes,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }
}


bool GlobeLabelsComponent::deinitialize() {
    return true;
}

bool GlobeLabelsComponent::isReady() const {
    return true;
}

void GlobeLabelsComponent::update() {
        
}

bool GlobeLabelsComponent::loadLabelsData(const std::string& file) {
    bool success = true;
    if (_labelsDataPresent) {
        std::string cachedFile = FileSys.cacheManager()->cachedFilename(
            ghoul::filesystem::File(file),
            "GlobeLabelsComponent|" + identifier(),
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

bool GlobeLabelsComponent::readLabelsFile(const std::string& file) {
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
                    // Non-ascii characters aren't displayed correctly by the text rendering 
                    // (We don't have the non-ascii character in the texture atlas)
                    // Once this limitation is fixed, we can remove the next piece of code
                    // Removing non ASCII characters:
                    int tokenChar = 0;
                    while (tokenChar < 256) {
                        if ((lEntry.feature[tokenChar] < 0 ||
                            lEntry.feature[tokenChar] > 127) &&
                            lEntry.feature[tokenChar] != '\0') {
                            lEntry.feature[tokenChar] = '*';
                        }
                        else if (lEntry.feature[tokenChar] == '\"') {
                            lEntry.feature[tokenChar] = '=';
                        }
                        tokenChar++;
                    }

                    strtok(NULL, ","); // Target is not used
                    lEntry.diameter = static_cast<float>(atof(strtok(NULL, ",")));
                    lEntry.latitude = static_cast<float>(atof(strtok(NULL, ",")));
                    lEntry.longitude = static_cast<float>(atof(strtok(NULL, ",")));
                    char * coordinateSystem = strtok(NULL, ",");

                    if (strstr(coordinateSystem, "West") != NULL) {
                        lEntry.longitude = 360.0f - lEntry.longitude;
                    }

                    // Clean white spaces
                    strncpy(lEntry.feature, strtok(lEntry.feature, "="), 256);

                    GlobeBrowsingModule* _globeBrowsingModule =
                        openspace::global::moduleEngine.module<openspace::GlobeBrowsingModule>();
                    lEntry.geoPosition = _globeBrowsingModule->cartesianCoordinatesFromGeo(
                        *_globe,
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

bool GlobeLabelsComponent::loadCachedFile(const std::string& file) {
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

bool GlobeLabelsComponent::saveCachedFile(const std::string& file) const {

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

void GlobeLabelsComponent::draw(const RenderData& data) {
    if (_labelsEnabled) {
        // Calculate the MVP matrix
        glm::dmat4 viewTransform = glm::dmat4(data.camera.combinedViewMatrix());
        glm::dmat4 vp = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
            viewTransform;
        glm::dmat4 mvp = vp * _globe->modelTransform();

        glm::dvec3 globePositionWorld = glm::dvec3(_globe->modelTransform() * 
            glm::vec4(0.0, 0.0, 0.0, 1.0));
        glm::dvec3 cameraToGlobeDistanceWorld = globePositionWorld - 
            data.camera.positionVec3();
        double distanceCameraGlobeWorld = glm::length(cameraToGlobeDistanceWorld);

        float varyingOpacity = 1.f;
        if (_labelsFadeInEnabled) {
            double averageRadius = (
                _globe->ellipsoid().radii().x + _globe->ellipsoid().radii().y + 
                _globe->ellipsoid().radii().z
                ) / 3.0;
            glm::dvec2 fadeRange = glm::dvec2(
                averageRadius + _labelsMinHeight
            );
            fadeRange.x += _labelsFadeInDist;
            double a = 1.0 / (fadeRange.y - fadeRange.x);
            double b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
            double funcValue = a * distanceCameraGlobeWorld + b;
            varyingOpacity *= funcValue > 1.0 ? 1.f : static_cast<float>(funcValue);

            if (varyingOpacity < minTransparencyValueConst) {
                return;
            }
        }

        if (_labelsFadeOutEnabled) {
            double averageRadius = (
                _globe->ellipsoid().radii().x + _globe->ellipsoid().radii().y +
                _globe->ellipsoid().radii().z
                ) / 3.0;

            glm::dvec2 fadeRange = glm::dvec2(
                averageRadius + _labelsMinHeight + labelFadeRangeConst
            );
            fadeRange.x += _labelsFadeOutDist;
            double a = rangeAngularCoefConst / (fadeRange.x - fadeRange.y);
            double b = -(fadeRange.y / (fadeRange.x - fadeRange.y));
            double funcValue = a * distanceCameraGlobeWorld + b;
            varyingOpacity *= funcValue > 1.0 ? 1.f : static_cast<float>(funcValue);

            if (varyingOpacity < minTransparencyValueConst) {
                return;
            }
        }

        renderLabels(
            data, 
            mvp, 
            static_cast<float>(distanceCameraGlobeWorld), 
            varyingOpacity
        );
    }
}

void GlobeLabelsComponent::renderLabels(const RenderData& data,
                                        const glm::dmat4& modelViewProjectionMatrix, 
                                        float distToCamera,
                                        float fadeInVariable
) {

    glm::vec4 textColor = _labelsColor;
    textColor.a *= fadeInVariable;
    constexpr double DIST_EPS = 6000.0;
    constexpr double SIN_EPS = 0.001;

    glm::dmat4 invMP = glm::inverse(_globe->modelTransform());
    glm::dmat4 invCombinedView = glm::inverse(data.camera.combinedViewMatrix());

    glm::dvec4 cameraPosWorld = invCombinedView * glm::dvec4(0.0, 0.0, 0.0, 1.0);
    glm::dvec3 cameraPosObj = glm::dvec3(invMP * cameraPosWorld);
    glm::dvec4 cameraUpVecWorld = glm::dvec4(data.camera.lookUpVectorWorldSpace(), 0.0); 
    glm::dvec3 cameraLookUpObj = glm::dvec3(invMP * cameraUpVecWorld);

    glm::dmat4 VP = glm::dmat4(
        data.camera.sgctInternal.projectionMatrix()
    ) * data.camera.combinedViewMatrix();


    glm::dmat4 invModelMatrix = glm::inverse(_globe->modelTransform());

    glm::dvec3 cameraViewDirectionObj = glm::dvec3(
        invModelMatrix * glm::dvec4(data.camera.viewDirectionWorldSpace(), 0.0)
    );
    glm::dvec3 cameraUpDirectionObj = glm::dvec3(
        invModelMatrix * glm::dvec4(data.camera.lookUpVectorWorldSpace(), 0.0)
    );
    glm::dvec3 orthoRight = glm::normalize(
        glm::cross(cameraViewDirectionObj, cameraUpDirectionObj)
    );
    if (orthoRight == glm::dvec3(0.0)) {
        glm::dvec3 otherVector(
            cameraUpDirectionObj.y,
            cameraUpDirectionObj.x,
            cameraUpDirectionObj.z
        );
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionObj));
    }
    glm::dvec3 orthoUp = glm::normalize(
        glm::cross(orthoRight, cameraViewDirectionObj)
    );

    for (const LabelEntry lEntry : _labels.labelsArray) {
        glm::vec3 position = lEntry.geoPosition;
        glm::dvec3 locationPositionWorld =
            glm::dvec3(_globe->modelTransform() * glm::dvec4(position, 1.0));
        double distanceCameraToLabelWorld =
            glm::length(locationPositionWorld - data.camera.positionVec3());

        bool draw = false;
        if (_labelsDisableCullingEnabled) {
            draw = true;
        }
        else if ((distToCamera > (distanceCameraToLabelWorld + _labelsDistaneEPS)) &&
            isLabelInFrustum(VP, locationPositionWorld)) { // culling
            draw = true;
        }

        if (draw) {
            if (_labelAlignmentOption == Circularly) {
                glm::dvec3 labelNormalObj = glm::dvec3(
                    invModelMatrix * glm::dvec4(data.camera.positionVec3(), 1.0)
                ) - glm::dvec3(position);

                glm::dvec3 labelUpDirectionObj = glm::dvec3(position);

                orthoRight = glm::normalize(
                    glm::cross(labelUpDirectionObj, labelNormalObj)
                );
                if (orthoRight == glm::dvec3(0.0)) {
                    glm::dvec3 otherVector(
                        labelUpDirectionObj.y,
                        labelUpDirectionObj.x,
                        labelUpDirectionObj.z
                    );
                    orthoRight = glm::normalize(glm::cross(otherVector, labelNormalObj));
                }
                orthoUp = glm::normalize(
                    glm::cross(labelNormalObj, orthoRight)
                );
            }

            position += _labelsMinHeight;

            ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;
            labelInfo.orthoRight = orthoRight;
            labelInfo.orthoUp = orthoUp;
            labelInfo.minSize = _labelsMinSize;
            labelInfo.maxSize = _labelsMaxSize;
            labelInfo.cameraPos = data.camera.positionVec3();
            labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
            labelInfo.renderType = 0;
            labelInfo.mvpMatrix = modelViewProjectionMatrix;
            labelInfo.scale = powf(2.f, _labelsSize);

            // Testing
            glm::dmat4 modelviewTransform = glm::dmat4(data.camera.combinedViewMatrix()) *
                _globe->modelTransform();
            labelInfo.modelViewMatrix = modelviewTransform;
            labelInfo.projectionMatrix = glm::dmat4(
                data.camera.sgctInternal.projectionMatrix()
            );

            ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
                *_font,
                position,
                lEntry.feature,
                textColor,
                labelInfo
            );
        }
    }
}

bool GlobeLabelsComponent::isLabelInFrustum(const glm::dmat4& MVMatrix,
    const glm::dvec3& position) const
{

    // Frustum Planes
    glm::dvec3 col1(MVMatrix[0][0], MVMatrix[1][0], MVMatrix[2][0]);
    glm::dvec3 col2(MVMatrix[0][1], MVMatrix[1][1], MVMatrix[2][1]);
    glm::dvec3 col3(MVMatrix[0][2], MVMatrix[1][2], MVMatrix[2][2]);
    glm::dvec3 col4(MVMatrix[0][3], MVMatrix[1][3], MVMatrix[2][3]);

    glm::dvec3 leftNormal = col4 + col1;
    glm::dvec3 rightNormal = col4 - col1;
    glm::dvec3 bottomNormal = col4 + col2;
    glm::dvec3 topNormal = col4 - col2;
    glm::dvec3 nearNormal = col3 + col4;
    glm::dvec3 farNormal = col4 - col3;

    // Plane Distances
    double leftDistance = MVMatrix[3][3] + MVMatrix[3][0];
    double rightDistance = MVMatrix[3][3] - MVMatrix[3][0];
    double bottomDistance = MVMatrix[3][3] + MVMatrix[3][1];
    double topDistance = MVMatrix[3][3] - MVMatrix[3][1];
    double nearDistance = MVMatrix[3][3] + MVMatrix[3][2];
    double farDistance = MVMatrix[3][3] - MVMatrix[3][2];

    // Normalize Planes
    double invMag = 1.0 / glm::length(leftNormal);
    leftNormal *= invMag;
    leftDistance *= invMag;

    invMag = 1.0 / glm::length(rightNormal);
    rightNormal *= invMag;
    rightDistance *= invMag;

    invMag = 1.0 / glm::length(bottomNormal);
    bottomNormal *= invMag;
    bottomDistance *= invMag;

    invMag = 1.0 / glm::length(topNormal);
    topNormal *= invMag;
    topDistance *= invMag;

    invMag = 1.0 / glm::length(nearNormal);
    nearNormal *= invMag;
    nearDistance *= invMag;

    invMag = 1.0 / glm::length(farNormal);
    farNormal *= invMag;
    farDistance *= invMag;

    float radius = 1.0;

    if ((glm::dot(leftNormal, position) + leftDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(rightNormal, position) + rightDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(bottomNormal, position) + bottomDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(topNormal, position) + topDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(nearNormal, position) + nearDistance) < -radius) {
        return false;
    }
    // The far plane testing is disabled because the atm has no depth.
    /*else if ((glm::dot(farNormal, position) + farDistance) < -radius) {
    return false;
    }*/

    return true;
}

} // namespace openspace
