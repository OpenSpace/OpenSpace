/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/globelabelscomponent.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <locale>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "GlobeLabels";

    constexpr const double LabelFadeOutLimitAltitudeMeters = 25000.0;
    constexpr const float MinOpacityValueConst = 0.009f;

    enum LabelRenderingAlignmentType {
        Horizontally = 0,
        Circularly
    };

    constexpr int8_t CurrentCacheVersion = 1;

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Enables and disables labels' rendering."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "Font size for the rendering labels. This is different fromt text size."
    };

    constexpr openspace::properties::Property::PropertyInfo MinMaxSizeInfo = {
        "MinMaxSize",
        "Min/Max Text Size",
        "Minimum and maximum label size, in pixels."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "LabelsSize",
        "Labels Size",
        "This value affects the size scale of the labels."
    };

    constexpr openspace::properties::Property::PropertyInfo HeightOffsetInfo = {
        "HeightOffset",
        "Height Offset",
        "This value moves the label away from the globe surface by the specified "
        "distance (in meters)."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The text color of the labels."
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "The opacity of the labels."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeDistancesInfo = {
        "FadeDistances",
        "Fade-In Distances",
        "The distances above the globe's surface at which the labels start fading in or "
        "out, given in meters. The final distances are also adjusted by the specified "
        "height offset."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInEnabledInfo = {
        "FadeInEnabled",
        "Fade In Enabled",
        "Sets whether the labels fade in when approaching the globe from a distance. If "
        "false, no fading happens and the labels immediately has full opacity."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeOutEnabledInfo = {
        "FadeOutEnabled",
        "Fade Out Enabled",
        "Sets whether the labels fade out when approaching the surface of the globe. If "
        "false, no fading happens and the labels stays in full opacity."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableCullingInfo = {
        "DisableCulling",
        "Culling Disabled",
        "Labels culling disabled."
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceEPSInfo = {
        "DistanceEPS",
        "Culling Distance",
        "Labels culling distance from globe's center."
    };

    constexpr openspace::properties::Property::PropertyInfo AlignmentOptionInfo = {
        "AlignmentOption",
        "Alignment Option",
        "Labels are aligned horizontally or circularly related to the planet."
    };

    struct [[codegen::Dictionary(GlobeLabelsComponent)]] Parameters {
        // The path to the labels file
        std::optional<std::filesystem::path> fileName;

        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(FontSizeInfo.description)]]
        std::optional<float> fontSize;

        // [[codegen::verbatim(MinMaxSizeInfo.description)]]
        std::optional<glm::ivec2> minMaxSize;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(HeightOffsetInfo.description)]]
        std::optional<float> heightOffset;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(OpacityInfo.description)]]
        std::optional<float> opacity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(FadeDistancesInfo.description)]]
        std::optional<glm::vec2> fadeDistances;

        // [[codegen::verbatim(FadeInEnabledInfo.description)]]
        std::optional<bool> fadeInEnabled;

        // [[codegen::verbatim(FadeOutEnabledInfo.description)]]
        std::optional<bool> fadeOutEnabled;

        // [[codegen::verbatim(DisableCullingInfo.description)]]
        std::optional<bool> disableCulling;

        // [[codegen::verbatim(DistanceEPSInfo.description)]]
        std::optional<float> distanceEPS;

        enum class Alignment {
            Horizontally,
            Circularly
        };
        // [[codegen::verbatim(AlignmentOptionInfo.description)]]
        std::optional<Alignment> alignmentOption;
    };
#include "globelabelscomponent_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation GlobeLabelsComponent::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_globelabelscomponent");
}

GlobeLabelsComponent::GlobeLabelsComponent()
    : properties::PropertyOwner({ "Labels" })
    , _enabled(EnabledInfo, false)
    , _fontSize(FontSizeInfo, 30, 1, 300)
    , _minMaxSize(MinMaxSizeInfo, glm::ivec2(1, 1000), glm::ivec2(1), glm::ivec2(1000))
    , _size(SizeInfo, 2.5, 0, 30)
    , _heightOffset(HeightOffsetInfo, 100.0, 0.0, 10000.0)
    , _color(ColorInfo, glm::vec3(1.f, 1.f, 0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _fadeDistances(
        FadeDistancesInfo,
        glm::vec2(1e4, 1e6),
        glm::vec2(1.f),
        glm::vec2(1e8)
    )
    , _fadeInEnabled(FadeInEnabledInfo, false)
    , _fadeOutEnabled(FadeOutEnabledInfo, false)
    , _disableCulling(DisableCullingInfo, false)
    , _distanceEPS(DistanceEPSInfo, 100000.f, 1000.f, 10000000.f)
    , _alignmentOption(
        AlignmentOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    addProperty(_enabled);
    addProperty(_fontSize);
    addProperty(_size);
    _minMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_minMaxSize);
    addProperty(_color);
    addProperty(_opacity);
    _fadeDistances.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    _fadeDistances.setExponent(3.f);
    addProperty(_fadeDistances);
    addProperty(_fadeInEnabled);
    addProperty(_fadeOutEnabled);
    addProperty(_heightOffset);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_disableCulling);
    addProperty(_distanceEPS);

    _alignmentOption.addOption(Horizontally, "Horizontally");
    _alignmentOption.addOption(Circularly, "Circularly");
    _alignmentOption = Horizontally;
    addProperty(_alignmentOption);
}

void GlobeLabelsComponent::initialize(const ghoul::Dictionary& dictionary,
                                      globebrowsing::RenderableGlobe* globe)
{
    ZoneScoped
    _globe = globe;

    const Parameters p = codegen::bake<Parameters>(dictionary);
    if (!p.fileName.has_value()) {
        return;
    }

    const bool loadSuccess = loadLabelsData(absPath(p.fileName->string()).string());
    if (!loadSuccess) {
        return;
    }

    _enabled = p.enabled.value_or(_enabled);
    _fontSize = p.fontSize.value_or(_fontSize);
    _fontSize.onChange([this]() { initializeFonts(); });
    _size = p.size.value_or(_size);
    _heightOffset = p.heightOffset.value_or(_heightOffset);
    _color = p.color.value_or(_color);
    _opacity = p.opacity.value_or(_opacity);
    _fadeInEnabled = p.fadeInEnabled.value_or(_fadeInEnabled);
    _fadeOutEnabled = p.fadeOutEnabled.value_or(_fadeOutEnabled);
    _fadeDistances = p.fadeDistances.value_or(_fadeDistances);
    _minMaxSize = p.minMaxSize.value_or(_minMaxSize);
    _disableCulling =
        p.disableCulling.value_or(_disableCulling);
    _distanceEPS = p.distanceEPS.value_or(_distanceEPS);

    if (p.alignmentOption.has_value()) {
        switch (*p.alignmentOption) {
            case Parameters::Alignment::Horizontally:
                _alignmentOption = Horizontally;
                break;
            case Parameters::Alignment::Circularly:
                _alignmentOption = Circularly;
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }

    initializeFonts();
}

void GlobeLabelsComponent::initializeFonts() {
    _font = openspace::global::fontManager->font(
        "Mono",
        static_cast<float>(_fontSize),
        ghoul::fontrendering::FontManager::Outline::Yes,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );
}

bool GlobeLabelsComponent::loadLabelsData(const std::string& file) {
    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        file,
        "GlobeLabelsComponent|" + identifier()
    );

    bool hasCachedFile = std::filesystem::is_regular_file(cachedFile);
    if (hasCachedFile) {
        LINFO(fmt::format("Cached file '{}' used for labels file: {}", cachedFile, file));

        const bool hasCache = loadCachedFile(cachedFile);
        if (hasCache) {
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

    bool success = readLabelsFile(file);
    if (success) {
        saveCachedFile(cachedFile);
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
        if (!csvLabelFile.is_open()) {
            return false;
        }

        _labels.labelsArray.clear();

        std::string sline;
        while (!csvLabelFile.eof()) {
            std::getline(csvLabelFile, sline);
            if (sline.size() <= 10) {
                continue;
            }

            std::istringstream iss(sline);
            std::string token;
            std::getline(iss, token, ',');

            // First line is just the Header
            if (token == "Feature_Name") {
                continue;
            }

            LabelEntry lEntry;

            // Non-ascii characters aren't displayed correctly by the text
            // rendering (We don't have the non-ascii character in the texture
            // atlas)
            // Once this limitation is fixed, we can remove the next piece of code
            // Removing non ASCII characters:
            strncpy(lEntry.feature, token.c_str(), 256);
            int tokenChar = 0;
            while (tokenChar < 256) {
                if (lEntry.feature[tokenChar] < 0 && lEntry.feature[tokenChar] != '\0') {
                    lEntry.feature[tokenChar] = '*';
                }
                else if (lEntry.feature[tokenChar] == '\"') {
                    lEntry.feature[tokenChar] = '=';
                }
                tokenChar++;
            }

            std::getline(iss, token, ','); // Target is not used

            std::getline(iss, token, ','); // Diameter
            lEntry.diameter = std::stof(token);

            std::getline(iss, token, ','); // Latitude
            lEntry.latitude = std::stof(token);

            std::getline(iss, token, ','); // Longitude
            lEntry.longitude = std::stof(token);

            std::getline(iss, token, ','); // Coord System
            std::string coordinateSystem(token);
            std::size_t found = coordinateSystem.find("West");
            if (found != std::string::npos) {
                lEntry.longitude = 360.0f - lEntry.longitude;
            }

            // Clean white spaces
            std::istringstream issFeature(lEntry.feature);
            std::getline(issFeature, token, '=');
            if (token.empty()) {
                std::getline(issFeature, token, '=');
            }
            strncpy(lEntry.feature, token.c_str(), 256);

            GlobeBrowsingModule* _globeBrowsingModule =
                global::moduleEngine->module<openspace::GlobeBrowsingModule>();
            lEntry.geoPosition = _globeBrowsingModule->cartesianCoordinatesFromGeo(
                *_globe,
                lEntry.latitude,
                lEntry.longitude,
                lEntry.diameter
            );

            _labels.labelsArray.push_back(lEntry);
        }

        return true;
    }
    catch (const std::fstream::failure& e) {
        LERROR(fmt::format("Failed reading labels file '{}'", file));
        LERROR(e.what());
        return false;
    }
}

bool GlobeLabelsComponent::loadCachedFile(const std::string& file) {
    std::ifstream fileStream(file, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
        return false;
    }

    int8_t version = 0;
    fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
    if (version != CurrentCacheVersion) {
        LINFO("The format of the cached file has changed: deleting old cache");
        fileStream.close();
        if (std::filesystem::is_regular_file(file)) {
            std::filesystem::remove(file);
        }
        return false;
    }

    int32_t nValues = 0;
    fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
    _labels.labelsArray.resize(nValues);

    fileStream.read(
        reinterpret_cast<char*>(_labels.labelsArray.data()),
        nValues * sizeof(LabelEntry)
    );

    return fileStream.good();
}

bool GlobeLabelsComponent::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return false;
    }
    fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion), sizeof(int8_t));

    int32_t nValues = static_cast<int32_t>(_labels.labelsArray.size());
    if (nValues == 0) {
        LERROR("Error writing cache: No values were loaded");
        return false;
    }
    fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

    size_t nBytes = nValues * sizeof(LabelEntry);
    fileStream.write(reinterpret_cast<const char*>(_labels.labelsArray.data()), nBytes);

    return fileStream.good();
}

void GlobeLabelsComponent::draw(const RenderData& data) {
    if (!_enabled) {
        return;
    }

    // Calculate the MVP matrix
    glm::dmat4 viewTransform = glm::dmat4(data.camera.combinedViewMatrix());
    glm::dmat4 vp = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
                    viewTransform;
    glm::dmat4 mvp = vp * _globe->modelTransform();

    glm::dvec3 globePosWorld =
        glm::dvec3(_globe->modelTransform() * glm::vec4(0.f, 0.f, 0.f, 1.f));
    glm::dvec3 cameraToGlobeWorld = globePosWorld - data.camera.positionVec3();
    double distanceCameraGlobeWorld = glm::length(cameraToGlobeWorld);

    float varyingOpacity = 1.f;

    const glm::dvec3 globeRadii = _globe->ellipsoid().radii();
    double averageRadius = (globeRadii.x + globeRadii.y + globeRadii.z) / 3.0;

    if (_fadeInEnabled) {
        glm::dvec2 fadeRange = glm::dvec2(averageRadius + _heightOffset);
        fadeRange.x += _fadeDistances.value().y;
        double a = 1.0 / (fadeRange.y - fadeRange.x);
        double b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        double funcValue = a * distanceCameraGlobeWorld + b;
        varyingOpacity *= static_cast<float>(std::min(funcValue, 1.0));

        if (varyingOpacity < MinOpacityValueConst) {
            return;
        }
    }

    if (_fadeOutEnabled) {
        glm::dvec2 fadeRange = glm::dvec2(
            averageRadius + _heightOffset + LabelFadeOutLimitAltitudeMeters
        );
        fadeRange.x += _fadeDistances.value().x;
        double a = 1.0 / (fadeRange.x - fadeRange.y);
        double b = -(fadeRange.y / (fadeRange.x - fadeRange.y));
        double funcValue = a * distanceCameraGlobeWorld + b;
        varyingOpacity *= static_cast<float>(std::min(funcValue, 1.0));

        if (varyingOpacity < MinOpacityValueConst) {
            return;
        }
    }

    renderLabels(data, mvp, static_cast<float>(distanceCameraGlobeWorld), varyingOpacity);
}

void GlobeLabelsComponent::renderLabels(const RenderData& data,
                                        const glm::dmat4& modelViewProjectionMatrix,
                                        float distToCamera, float fadeInVariable
) {
    glm::vec4 textColor = glm::vec4(
        glm::vec3(_color),
        _opacity * fadeInVariable
    );

    glm::dmat4 VP = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
                    data.camera.combinedViewMatrix();

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
    glm::dvec3 orthoUp = glm::normalize(glm::cross(orthoRight, cameraViewDirectionObj));

    for (const LabelEntry& lEntry : _labels.labelsArray) {
        glm::vec3 position = lEntry.geoPosition;
        glm::dvec3 locationPositionWorld =
            glm::dvec3(_globe->modelTransform() * glm::dvec4(position, 1.0));
        double distanceCameraToLabelWorld =
            glm::length(locationPositionWorld - data.camera.positionVec3());

        if (_disableCulling ||
            ((distToCamera > (distanceCameraToLabelWorld + _distanceEPS)) &&
            isLabelInFrustum(VP, locationPositionWorld)))
        {
            if (_alignmentOption == Circularly) {
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
                orthoUp = glm::normalize(glm::cross(labelNormalObj, orthoRight));
            }

            // Move the position along the normal. Note that position is in model space
            position += _heightOffset.value() * glm::normalize(position);

            ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;
            labelInfo.orthoRight = orthoRight;
            labelInfo.orthoUp = orthoUp;
            labelInfo.minSize = _minMaxSize.value().x;
            labelInfo.maxSize = _minMaxSize.value().y;
            labelInfo.cameraPos = data.camera.positionVec3();
            labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
            labelInfo.renderType = 0;
            labelInfo.mvpMatrix = modelViewProjectionMatrix;
            labelInfo.scale = powf(2.f, _size);
            labelInfo.enableDepth = true;
            labelInfo.enableFalseDepth = true;
            labelInfo.disableTransmittance = true;

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
    // double farDistance = MVMatrix[3][3] - MVMatrix[3][2];

    // Normalize Planes
    const double invMagLeft = 1.0 / glm::length(leftNormal);
    leftNormal *= invMagLeft;
    leftDistance *= invMagLeft;

    const double invMagRight = 1.0 / glm::length(rightNormal);
    rightNormal *= invMagRight;
    rightDistance *= invMagRight;

    const double invMagBottom = 1.0 / glm::length(bottomNormal);
    bottomNormal *= invMagBottom;
    bottomDistance *= invMagBottom;

    const double invMagTop = 1.0 / glm::length(topNormal);
    topNormal *= invMagTop;
    topDistance *= invMagTop;

    const double invMagNear = 1.0 / glm::length(nearNormal);
    nearNormal *= invMagNear;
    nearDistance *= invMagNear;

    const double invMagFar = 1.0 / glm::length(farNormal);
    farNormal *= invMagFar;
    // farDistance *= invMagFar;

    constexpr const float Radius = 1.0;

    if ((glm::dot(leftNormal, position) + leftDistance) < -Radius) {
        return false;
    }
    else if ((glm::dot(rightNormal, position) + rightDistance) < -Radius) {
        return false;
    }
    else if ((glm::dot(bottomNormal, position) + bottomDistance) < -Radius) {
        return false;
    }
    else if ((glm::dot(topNormal, position) + topDistance) < -Radius) {
        return false;
    }
    else if ((glm::dot(nearNormal, position) + nearDistance) < -Radius) {
        return false;
    }

    return true;
}

} // namespace openspace
