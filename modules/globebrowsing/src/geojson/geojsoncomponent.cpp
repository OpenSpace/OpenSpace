/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/globebrowsing/src/geojson/geojsoncomponent.h>

#include <modules/globebrowsing/src/geojson/globegeometryhelper.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/json.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/lightsource.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <filesystem>
#include <fstream>
#include <functional>
#include <optional>

namespace geos_nlohmann = nlohmann;
#include <geos/geom/Geometry.h>
#include <geos/io/GeoJSON.h>
#include <geos/io/GeoJSONReader.h>

namespace {
    constexpr std::string_view _loggerCat = "GeoJsonComponent";

    constexpr std::string_view KeyIdentifier = "Identifier";
    constexpr std::string_view KeyName = "Name";
    constexpr std::string_view KeyDesc = "Description";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "This setting determines whether this object will be visible or not",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "File",
        "Path to the GeoJSON file to base the rendering on",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HeightOffsetInfo = {
        "HeightOffset",
        "Height Offset",
        "A height offset value, in meters. Useful for moving a feature closer to or "
        "farther away from the surface",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo CoordinateOffsetInfo = {
        "CoordinateOffset",
        "Geographic Coordinate Offset",
        "A latitude and longitude offset value, in decimal degrees. Can be used to "
        "move the object on the surface and correct potential mismatches with other "
        "renderings. Note that changing it during runtime leads to all positions being "
        "recomputed",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawWireframeInfo = {
        "DrawWireframe",
        "Wireframe",
        "If true, draw the wire frame of the polygons. Used for testing and to "
        "investigate tessellation results",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo PreventHeightUpdateInfo = {
        "PreventHeightUpdate",
        "Prevent Update From Heightmap",
        "If true, the polygon mesh will not be automatically updated based on the "
        "heightmap, even if the 'RelativeToGround' altitude option is set and the "
        "heightmap updates. The data can still be force updated",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ForceUpdateHeightDataInfo = {
        "ForceUpdateHeightData",
        "Force Update Height Data",
        "Triggering this leads to a recomputation of the heights based on the globe "
        "height map value at the geometry's positions",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PointRenderModeInfo = {
        "PointRenderMode",
        "Points Aligned to",
        "Decides how the billboards for the points should be rendered in terms of up "
        "direction and whether the plane should face the camera. See details on the "
        "different options in the wiki",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FlyToFeatureInfo = {
        "FlyToFeature",
        "Fly To Feature",
        "Triggering this leads to the camera flying to a position that show the GeoJson "
        "feature. The flight will account for any lat, long or height offset",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo CentroidCoordinateInfo = {
        "CentroidCoordinate",
        "Centroid Coordinate",
        "The lat long coordinate of the centroid position of the read geometry. Note "
        "that this value does not incude the offset",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BoundingBoxInfo = {
        "BoundingBox",
        "Bounding Box",
        "The lat long coordinates of the lower and upper corner of the bounding box of "
        "the read geometry. Note that this value does not incude the offset",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeScaleInfo = {
        "PointSizeScale",
        "Point Size Scale",
        "An extra scale value that can be used to increase or decrease the scale of any "
        "rendered points in the component, even if a value is set from the GeoJson file",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthScaleInfo = {
        "LineWidthScale",
        "Line Width Scale",
        "An extra scale value that can be used to increase or decrease the width of any "
        "rendered lines in the component, even if a value is set from the GeoJson file. "
        "Note that there is a max limit for how wide lines can be.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo DeleteInfo = {
        "Delete",
        "Delete",
        "Triggering this will remove this GeoJson component from its globe. Note that "
        "the GUI may have to be reloaded for the change to be reflect in the user "
        "interface.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(GeoJsonComponent)]] Parameters {
        // The unique identifier for this layer. May not contain '.' or spaces
        std::string identifier;

        // A human-readable name for the user interface. If this is omitted, the
        // identifier is used instead
        std::optional<std::string> name;

        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // The opacity of the component
        std::optional<float> opacity [[codegen::inrange(0.0, 1.0)]];

        // A human-readable description of the layer to be used in informational texts
        // presented to the user
        std::optional<std::string> description;

        // If true, ignore any height values that are given in the file. Coordinates with
        // three values will then be treated as coordinates with only two values
        std::optional<bool> ignoreHeights;

        // [[codegen::verbatim(PreventHeightUpdateInfo.description)]]
        std::optional<bool> preventHeightUpdate;

        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;

        // [[codegen::verbatim(HeightOffsetInfo.description)]]
        std::optional<float> heightOffset;

        // [[codegen::verbatim(PointSizeScaleInfo.description)]]
        std::optional<float> pointSizeScale;

        // [[codegen::verbatim(LineWidthScaleInfo.description)]]
        std::optional<float> lineWidthScale;

        // [[codegen::verbatim(CoordinateOffsetInfo.description)]]
        std::optional<glm::vec2> coordinateOffset;

        enum class
        [[codegen::map(openspace::globebrowsing::GlobeGeometryFeature::PointRenderMode)]]
        PointRenderMode
        {
            AlignToCameraDir [[codegen::key("Camera Direction")]],
            AlignToCameraPos [[codegen::key("Camera Position")]],
            AlignToGlobeNormal [[codegen::key("Globe Normal")]],
            AlignToGlobeSurface [[codegen::key("Globe Surface")]]
        };
        // [[codegen::verbatim(PointRenderModeInfo.description)]]
        std::optional<PointRenderMode> pointRenderMode;

        // [[codegen::verbatim(DrawWireframeInfo.description)]]
        std::optional<bool> drawWireframe;

        // These properties will be used as default values for the geoJson rendering,
        // meaning that they will be used when there is no value given for the
        // individual geoJson features
        std::optional<ghoul::Dictionary> defaultProperties
            [[codegen::reference("globebrowsing_geojsonproperties")]];

        // A list of light sources that this object should accept light from
        std::optional<std::vector<ghoul::Dictionary>> lightSources
            [[codegen::reference("core_light_source")]];
    };
#include "geojsoncomponent_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GeoJsonComponent::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_geojsoncomponent");
}

GeoJsonComponent::SubFeatureProps::SubFeatureProps(
                                       properties::PropertyOwner::PropertyOwnerInfo info)
    : properties::PropertyOwner(std::move(info))
    , enabled(EnabledInfo, true)
    , centroidLatLong(
        CentroidCoordinateInfo,
        glm::vec2(0.f),
        glm::vec2(-90.f, -180.f),
        glm::vec2(90.f, 180.f)
    )
    , boundingboxLatLong(
        BoundingBoxInfo,
        glm::vec4(0.f),
        glm::vec4(-90.f, -180.f, -90.f, -180.f),
        glm::vec4(90.f, 180.f, 90.f, 180.f)
    )
    , flyToFeature(FlyToFeatureInfo)
{
    _opacity.setVisibility(openspace::properties::Property::Visibility::AdvancedUser);
    addProperty(Fadeable::_opacity);
    addProperty(Fadeable::_fade);

    addProperty(enabled);

    addProperty(flyToFeature);

    centroidLatLong.setReadOnly(true);
    addProperty(centroidLatLong);

    boundingboxLatLong.setReadOnly(true);
    addProperty(boundingboxLatLong);
}

GeoJsonComponent::GeoJsonComponent(const ghoul::Dictionary& dictionary,
                                   RenderableGlobe& globe)
    : properties::PropertyOwner({
        dictionary.value<std::string>(KeyIdentifier),
        dictionary.hasKey(KeyName) ? dictionary.value<std::string>(KeyName) : "",
        dictionary.hasKey(KeyDesc) ? dictionary.value<std::string>(KeyDesc) : ""
    })
    , _enabled(EnabledInfo, true)
    , _geoJsonFile(FileInfo)
    , _heightOffset(HeightOffsetInfo, 10.f, -1e12f, 1e12f)
    , _latLongOffset(
        CoordinateOffsetInfo,
        glm::vec2(0.f),
        glm::vec2(-90.0),
        glm::vec2(90.f)
    )
    , _pointSizeScale(PointSizeScaleInfo, 1.f, 0.01f, 100.f)
    , _lineWidthScale(LineWidthScaleInfo, 1.f, 0.01f, 10.f)
    , _pointRenderModeOption(
        PointRenderModeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _drawWireframe(DrawWireframeInfo, false)
    , _preventUpdatesFromHeightMap(PreventHeightUpdateInfo, false)
    , _forceUpdateHeightData(ForceUpdateHeightDataInfo)
    , _globeNode(globe)
    , _centerLatLong(
        CentroidCoordinateInfo,
        glm::vec2(0.f),
        glm::vec2(-90.f, -180.f),
        glm::vec2(90.f, 180.f)
    )
    , _flyToFeature(FlyToFeatureInfo)
    , _deletePropertyOwner({ "Deletion", "Deletion" })
    , _deleteThisComponent(DeleteInfo)
    , _lightSourcePropertyOwner({ "LightSources", "Light Sources" })
    , _featuresPropertyOwner({ "Features", "Features" })
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _enabled = p.enabled.value_or(_enabled);
    addProperty(_enabled);

    _opacity = p.opacity.value_or(_opacity);
    addProperty(_opacity);
    addProperty(_fade);

    _geoJsonFile = p.file.string();
    _geoJsonFile.setReadOnly(true);
    addProperty(_geoJsonFile);

    _ignoreHeightsFromFile = p.ignoreHeights.value_or(_ignoreHeightsFromFile);

    const float minGlobeRadius = static_cast<float>(
        _globeNode.ellipsoid().minimumRadius()
    );

    _heightOffset = p.heightOffset.value_or(_heightOffset);
    _heightOffset.onChange([this]() { _heightOffsetIsDirty = true; });
    constexpr float MinRadiusFactor = -0.9f;
    constexpr float MaxRadiusFactor = 5.f;
    _heightOffset.setMinValue(MinRadiusFactor * minGlobeRadius);
    _heightOffset.setMaxValue(MaxRadiusFactor * minGlobeRadius);
    addProperty(_heightOffset);

    _latLongOffset = p.coordinateOffset.value_or(_latLongOffset);
    _latLongOffset.onChange([this]() { _dataIsDirty = true; });
    addProperty(_latLongOffset);

    _pointSizeScale = p.pointSizeScale.value_or(_pointSizeScale);
    addProperty(_pointSizeScale);

    _lineWidthScale = p.lineWidthScale.value_or(_lineWidthScale);
    addProperty(_lineWidthScale);

    if (p.defaultProperties.has_value()) {
        _defaultProperties.createFromDictionary(*p.defaultProperties, _globeNode);
    }
    addPropertySubOwner(_defaultProperties);

    _defaultProperties.pointTexture.onChange([this]() {
        const std::filesystem::path texturePath = _defaultProperties.pointTexture.value();
        // Not ethat an empty texture is also valid => use default texture from module
        if (std::filesystem::is_regular_file(texturePath) || texturePath.empty()) {
            _textureIsDirty = true;
        }
        else {
            LERROR(fmt::format(
                "Provided texture file does not exist: {}",
                _defaultProperties.pointTexture.value()
            ));
        }
    });

    _defaultProperties.tessellation.enabled.onChange([this]() { _dataIsDirty = true; });
    _defaultProperties.tessellation.useLevel.onChange([this]() { _dataIsDirty = true; });
    _defaultProperties.tessellation.level.onChange([this]() { _dataIsDirty = true; });
    _defaultProperties.tessellation.distance.onChange([this]() {
        _dataIsDirty = true;
    });

    _forceUpdateHeightData.onChange([this]() {
        for (GlobeGeometryFeature& f : _geometryFeatures) {
            f.updateHeightsFromHeightMap();
        }
    });
    addProperty(_forceUpdateHeightData);

    _preventUpdatesFromHeightMap =
        p.preventHeightUpdate.value_or(_preventUpdatesFromHeightMap);
    addProperty(_preventUpdatesFromHeightMap);

    _drawWireframe = p.drawWireframe.value_or(_drawWireframe);
    addProperty(_drawWireframe);

    using PointRenderMode = GlobeGeometryFeature::PointRenderMode;
    _pointRenderModeOption.addOptions({
        { static_cast<int>(PointRenderMode::AlignToCameraDir), "Camera Direction"},
        { static_cast<int>(PointRenderMode::AlignToCameraPos), "Camera Position"},
        { static_cast<int>(PointRenderMode::AlignToGlobeNormal), "Globe Normal"},
        { static_cast<int>(PointRenderMode::AlignToGlobeSurface), "Globe Surface"}
    });
    if (p.pointRenderMode.has_value()) {
        _pointRenderModeOption =
            static_cast<int>(codegen::map<PointRenderMode>(*p.pointRenderMode));
    }
    addProperty(_pointRenderModeOption);

    _centerLatLong.setReadOnly(true);
    addProperty(_centerLatLong);

    _flyToFeature.onChange([this]() { flyToFeature(); });
    addProperty(_flyToFeature);

    // Add delete trigger under its own property owner, to make it more difficult to
    // access by mistake
    _deleteThisComponent.onChange([this]() { triggerDeletion(); });
    _deletePropertyOwner.addProperty(_deleteThisComponent);
    addPropertySubOwner(_deletePropertyOwner);

    readFile();

    if (p.lightSources.has_value()) {
        const std::vector<ghoul::Dictionary> lightsources = *p.lightSources;

        for (const ghoul::Dictionary& lsDictionary : lightsources) {
            std::unique_ptr<LightSource> lightSource =
                LightSource::createFromDictionary(lsDictionary);
            _lightSourcePropertyOwner.addPropertySubOwner(lightSource.get());
            _lightSources.push_back(std::move(lightSource));
        }
    }
    else {
        // If no light source provided, add a deafult light source from the camera
        using namespace std::string_literals;
        ghoul::Dictionary defaultLightSourceDict;
        defaultLightSourceDict.setValue("Identifier", "Camera"s);
        defaultLightSourceDict.setValue("Type", "CameraLightSource"s);
        defaultLightSourceDict.setValue("Intensity", 1.0);
        _lightSources.push_back(
            LightSource::createFromDictionary(defaultLightSourceDict)
        );
        _lightSourcePropertyOwner.addPropertySubOwner(_lightSources.back().get());
    }
    addPropertySubOwner(_lightSourcePropertyOwner);
    addPropertySubOwner(_featuresPropertyOwner);
}

GeoJsonComponent::~GeoJsonComponent() {}

bool GeoJsonComponent::enabled() const {
    return _enabled;
}

void GeoJsonComponent::initialize() {
    ZoneScoped;

    for (const std::unique_ptr<LightSource>& ls : _lightSources) {
        ls->initialize();
    }
}

void GeoJsonComponent::initializeGL() {
    ZoneScoped;

    _linesAndPolygonsProgram = global::renderEngine->buildRenderProgram(
        "GeoLinesAndPolygonProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/geojson_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/geojson_fs.glsl")
    );

    _pointsProgram = global::renderEngine->buildRenderProgram(
        "GeoPointsProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/geojson_points_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/geojson_points_fs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/geojson_points_gs.glsl")
    );

    for (GlobeGeometryFeature& g : _geometryFeatures) {
        g.initializeGL(_pointsProgram.get(), _linesAndPolygonsProgram.get());
    }
}

void GeoJsonComponent::deinitializeGL() {
    for (GlobeGeometryFeature& g : _geometryFeatures) {
        g.deinitializeGL();
    }

    global::renderEngine->removeRenderProgram(_linesAndPolygonsProgram.get());
    _linesAndPolygonsProgram = nullptr;

    global::renderEngine->removeRenderProgram(_pointsProgram.get());
    _pointsProgram = nullptr;
}

bool GeoJsonComponent::isReady() const {
    const bool isReady = std::all_of(
        std::begin(_geometryFeatures),
        std::end(_geometryFeatures),
        std::mem_fn(&GlobeGeometryFeature::isReady)
    );
    return isReady && _linesAndPolygonsProgram && _pointsProgram;
}

void GeoJsonComponent::render(const RenderData& data) {
    if (!_enabled || !isVisible()) {
        return;
    }

    // @TODO (2023-03-17, emmbr): Once the light source for the globe can be configured,
    // this code should use the same light source as the globe
    _lightsourceRenderData.updateBasedOnLightSources(data, _lightSources);

    // Change GL state:
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glEnable(GL_DEPTH_TEST);

    if (_drawWireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    }

    using PointRenderMode = GlobeGeometryFeature::PointRenderMode;
    PointRenderMode pointRenderMode =
        static_cast<PointRenderMode>(_pointRenderModeOption.value());

    // Compose extra data from relevant properties to pass to the individual features
    const GlobeGeometryFeature::ExtraRenderData extraRenderdata = {
        _pointSizeScale,
        _lineWidthScale,
        pointRenderMode,
        _lightsourceRenderData
    };

    // Do two render passes, to properly render opacity of overlaying objects
    for (int renderPass = 0; renderPass < 2; ++renderPass) {
        for (size_t i = 0; i < _geometryFeatures.size(); i++) {
            if (_features[i]->enabled && _features[i]->isVisible()) {
                _geometryFeatures[i].render(
                    data,
                    renderPass,
                    opacity() * _features[i]->opacity(),
                    extraRenderdata
                );
            }
        }
    }

    if (_drawWireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    }

    glBindVertexArray(0);

    // Restore GL State
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetLineState();
}

void GeoJsonComponent::update() {
    if (!_enabled || !isVisible()) {
        return;
    }

    const glm::vec3 offsets = glm::vec3(_latLongOffset.value(), _heightOffset);

    for (size_t i = 0; i < _geometryFeatures.size(); i++) {
        if (!_features[i]->enabled) {
            continue;
        }
        GlobeGeometryFeature& g = _geometryFeatures[i];

        if (_dataIsDirty || _heightOffsetIsDirty) {
            g.setOffsets(offsets);
        }

        if (_textureIsDirty) {
            g.updateTexture();
        }

        g.update(_dataIsDirty, _preventUpdatesFromHeightMap);
    }

    _textureIsDirty = false;
    _dataIsDirty = false;
}

void GeoJsonComponent::readFile() {
    std::ifstream file(_geoJsonFile);

    if (!file.good()) {
        LERROR(fmt::format("Failed to open GeoJSON file: {}", _geoJsonFile.value()));
        return;
    }

    _geometryFeatures.clear();

    const std::string content = std::string(
        std::istreambuf_iterator<char>(file),
        std::istreambuf_iterator<char>()
    );

    // Parse GeoJSON string into GeoJSON objects
    try {
        const geos::io::GeoJSONReader reader;
        const geos::io::GeoJSONFeatureCollection fc = reader.readFeatures(content);

        int count = 1;
        for (const geos::io::GeoJSONFeature& feature : fc.getFeatures()) {
            parseSingleFeature(feature, count);
            count++;
        }

        if (_geometryFeatures.empty()) {
            LWARNING(fmt::format(
                "No GeoJson features could be successfully created for GeoJson layer "
                "with identifier '{}'. Disabling layer.", identifier()
            ));
            _enabled = false;
        }
    }
    catch (const geos::util::GEOSException& e) {
        LERROR(fmt::format(
            "Error creating GeoJson layer with identifier '{}'. Problem reading "
            "GeoJson file '{}'. Error: {}", identifier(), _geoJsonFile.value(), e.what()
        ));
    }

    computeMainFeatureMetaPropeties();
}

void GeoJsonComponent::parseSingleFeature(const geos::io::GeoJSONFeature& feature,
                                          int indexInFile
) {
    // Read the geometry
    const geos::geom::Geometry* geom = feature.getGeometry();

    // Read the properties
    GeoJsonOverrideProperties propsFromFile = propsFromGeoJson(feature);

    std::vector<const geos::geom::Geometry*> geomsToAdd;
    if (!geom) {
        // Null geometry => no geometries to add
        LWARNING(fmt::format(
            "Feature {} in GeoJson file '{}' is a null geometry and will not be loaded",
            indexInFile, _geoJsonFile.value()
        ));
        // @TODO (emmbr26) We should eventually support features with null geometry
    }
    else if (geom->isPuntal()) {
        // If points, handle all point features as one feature, even multi-points
        geomsToAdd = { geom };
    }
    else {
        const size_t nGeom = geom->getNumGeometries();
        geomsToAdd.reserve(nGeom);
        for (size_t i = 0; i < nGeom; i++) {
            const geos::geom::Geometry* subGeometry = geom->getGeometryN(i);
            if (subGeometry) {
                geomsToAdd.push_back(subGeometry);
            }
        }
    }

    // Split other collection features into multiple individual rendered components

    for (const geos::geom::Geometry* geometry : geomsToAdd) {
        const int index = static_cast<int>(_geometryFeatures.size());
        try {
            GlobeGeometryFeature g(_globeNode, _defaultProperties, propsFromFile);
            g.createFromSingleGeosGeometry(geometry, index, _ignoreHeightsFromFile);
            g.initializeGL(_pointsProgram.get(), _linesAndPolygonsProgram.get());
            _geometryFeatures.push_back(std::move(g));

            std::string name = _geometryFeatures.back().key();
            std::string identifier = makeIdentifier(name);

            // If there is already an owner with that name as an identifier, make a
            // unique one
            if (_featuresPropertyOwner.hasPropertySubOwner(identifier)) {
                identifier = fmt::format("Feature{}-", index, identifier);
            }

            const properties::PropertyOwner::PropertyOwnerInfo info = {
                std::move(identifier),
                std::move(name)
                // @TODO: Use description from file, if any
            };
            _features.push_back(std::make_unique<SubFeatureProps>(info));

            addMetaPropertiesToFeature(*_features.back(), index, geometry);

            _featuresPropertyOwner.addPropertySubOwner(_features.back().get());
        }
        catch (const ghoul::RuntimeError& error) {
            LERROR(fmt::format(
                "Error creating GeoJson layer with identifier '{}'. Problem reading "
                "feature {} in GeoJson file '{}'.",
                identifier(), indexInFile, _geoJsonFile.value()
            ));
            LERRORC(error.component, error.message);
            // Do nothing
        }
    }
}

void GeoJsonComponent::addMetaPropertiesToFeature(SubFeatureProps& feature, int index,
                                                  const geos::geom::Geometry* geometry)
{
    std::unique_ptr<geos::geom::Point> centroid = geometry->getCentroid();
    // Using `auto` here as on MacOS `getCoordinate` returns:
    // geos::geom::Coordinate
    // but on Windows it returns
    // geos::geom::CoordinateXY
    auto centroidCoord = *centroid->getCoordinate();
    const glm::vec2 centroidLatLong = glm::vec2(centroidCoord.y, centroidCoord.x);
    feature.centroidLatLong = centroidLatLong;

    std::unique_ptr<geos::geom::Geometry> boundingbox = geometry->getEnvelope();
    std::unique_ptr<geos::geom::CoordinateSequence> coords =
        boundingbox->getCoordinates();
    glm::vec4 boundingboxLatLong;
    if (boundingbox->isRectangle()) {
        // A rectangle has 5 coordinates, where the first and third are two corners
        boundingboxLatLong = glm::vec4(
            (*coords)[0].y,
            (*coords)[0].x,
            (*coords)[2].y,
            (*coords)[2].x
        );
    }
    else {
        // Invalid boundingbox. Can happen e.g. for single points.
        // Just add a degree to every direction from the centroid
        boundingboxLatLong = glm::vec4(
            centroidLatLong.x - 1.f,
            centroidLatLong.y - 1.f,
            centroidLatLong.x + 1.f,
            centroidLatLong.y + 1.f
        );
    }

    feature.boundingboxLatLong = boundingboxLatLong;

    // Compute the diagonal distance of the bounding box
    const Geodetic2 pos0 = {
        glm::radians(boundingboxLatLong.x),
        glm::radians(boundingboxLatLong.y)
    };

    const Geodetic2 pos1 = {
        glm::radians(boundingboxLatLong.z),
        glm::radians(boundingboxLatLong.w)
    };
    feature.boundingBoxDiagonal = static_cast<float>(
        std::abs(_globeNode.ellipsoid().greatCircleDistance(pos0, pos1))
    );

    feature.flyToFeature.onChange([this, index]() { flyToFeature(index); });
}

void GeoJsonComponent::computeMainFeatureMetaPropeties() {
    if (_features.empty()) {
        return;
    }

    glm::vec2 bboxLowerCorner = {
        _features.front()->boundingboxLatLong.value().x,
        _features.front()->boundingboxLatLong.value().y
    };
    glm::vec2 bboxUpperCorner = {
        _features.front()->boundingboxLatLong.value().z,
        _features.front()->boundingboxLatLong.value().w
    };

    for (const std::unique_ptr<SubFeatureProps>& f : _features) {
        // Update bbox corners
        if (f->boundingboxLatLong.value().x < bboxLowerCorner.x) {
            bboxLowerCorner.x = f->boundingboxLatLong.value().x;
        }
        if (f->boundingboxLatLong.value().y < bboxLowerCorner.y) {
            bboxLowerCorner.y = f->boundingboxLatLong.value().y;
        }
        if (f->boundingboxLatLong.value().z > bboxUpperCorner.x) {
            bboxUpperCorner.x = f->boundingboxLatLong.value().z;
        }
        if (f->boundingboxLatLong.value().w > bboxUpperCorner.y) {
            bboxUpperCorner.y = f->boundingboxLatLong.value().w;
        }
    }

    // Identify the bounding box midpoints
    _centerLatLong = 0.5f * (bboxLowerCorner + bboxUpperCorner);

    // Compute the diagonal distance (size) of the bounding box
    const Geodetic2 pos0 = {
        glm::radians(bboxLowerCorner.x),
        glm::radians(bboxLowerCorner.y)
    };

    const Geodetic2 pos1 = {
        glm::radians(bboxUpperCorner.x),
        glm::radians(bboxUpperCorner.y)
    };
    _bboxDiagonalSize = static_cast<float>(
        std::abs(_globeNode.ellipsoid().greatCircleDistance(pos0, pos1))
    );
}

void GeoJsonComponent::flyToFeature(std::optional<int> index) const {
    // General size properties
    float diagonal = _bboxDiagonalSize;
    float centroidLat = _centerLatLong.value().x;
    float centroidLon = _centerLatLong.value().y;

    if (index.has_value()) {
        const SubFeatureProps* f = _features[*index].get();
        diagonal = f->boundingBoxDiagonal;
        centroidLat = f->centroidLatLong.value().x;
        centroidLon = f->centroidLatLong.value().y;
    }

    // Compute a good distance to travel to based on the feature's size.
    // Assumes 80 degree FOV
    constexpr float Angle = glm::radians(40.f);
    float d = diagonal / glm::tan(Angle);
    d += _heightOffset;

    float lat = centroidLat + _latLongOffset.value().x;
    float lon = centroidLon + _latLongOffset.value().y;

    global::scriptEngine->queueScript(
        fmt::format(
            "openspace.globebrowsing.flyToGeo([[{}]], {}, {}, {})",
            _globeNode.owner()->identifier(), lat, lon, d
        ),
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

void GeoJsonComponent::triggerDeletion() const {
    global::scriptEngine->queueScript(
        fmt::format(
            "openspace.globebrowsing.deleteGeoJson([[{}]], [[{}]])",
            _globeNode.owner()->identifier(), _identifier
        ),
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

} // namespace openspace::globebrowsing
