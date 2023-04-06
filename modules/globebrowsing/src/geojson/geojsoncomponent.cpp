/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
#include <geos/geom/Geometry.h>
#include <geos/io/GeoJSON.h>
#include <geos/io/GeoJSONReader.h>
#include <filesystem>
#include <fstream>
#include <optional>

// This has to be included after the geos files, since they have their own version
// of the nlohmann json library. Doing it in the other order leads to conflicts..
#include <openspace/json.h>

namespace {
    constexpr std::string_view _loggerCat = "GeoJsonComponent";

    constexpr std::string_view KeyIdentifier = "Identifier";
    constexpr std::string_view KeyName = "Name";
    constexpr std::string_view KeyDesc = "Description";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this object will be visible or not"
    };

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "File",
        "Path to the GeoJSON file to base the rendering on."
    };

    constexpr openspace::properties::Property::PropertyInfo HeightOffsetInfo = {
        "HeightOffset",
        "Height Offset",
        "A height offset value, in meters. Useful for moving a feature closer to or "
        "farther away from the surface."
    };

    constexpr openspace::properties::Property::PropertyInfo CoordinateOffsetInfo = {
        "CoordinateOffset",
        "Geographic Coordinate Offset",
        "A latitude and longitude offset value, in decimal degrees. Can be used to "
        "move the object on the surface and correct potential mismatches with other "
        "renderings."
    };

    constexpr openspace::properties::Property::PropertyInfo DrawWireframeInfo = {
        "DrawWireframe",
        "Wireframe",
        "If true, draw the wire frame of the polygons. Used for testing"
        // @TODO make debug/developer property
    };

    constexpr openspace::properties::Property::PropertyInfo PreventHeightUpdateInfo = {
        "PreventHeightUpdate",
        "Prevent Update From Heightmap",
        "If true, the polygon mesh will not be automatically updated based on the "
        "heightmap, even if the 'RelativeToGround' altitude option is set and the "
        "heightmap updates. The data can still be force updated."
    };

    constexpr openspace::properties::Property::PropertyInfo ForceUpdateHeightDataInfo = {
        "ForceUpdateHeightData",
        "Force Update Height Data",
        "Triggering this leads to a recomputation of the heights based on the globe "
        "height map value at the geometry's positions."
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourcesInfo = {
        "LightSources",
        "Light Sources",
        "A list of light sources that this object should accept light from"
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

        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;

        // [[codegen::verbatim(HeightOffsetInfo.description)]]
        std::optional<float> heightOffset;

        // [[codegen::verbatim(CoordinateOffsetInfo.description)]]
        std::optional<glm::vec2> coordinateOffset;

        // [[codegen::verbatim(DrawWireframeInfo.description)]]
        std::optional<bool> drawWireframe;

        // These properties will be used as default values for the geoJson rendering,
        // meaning that they will be used when there is no value given for the
        // individual geoJson features
        std::optional<ghoul::Dictionary> defaultProperties
            [[codegen::reference("globebrowsing_geojsonproperties")]];

        // [[codegen::verbatim(LightSourcesInfo.description)]]
        std::optional<std::vector<ghoul::Dictionary>> lightSources
            [[codegen::reference("core_light_source")]];
    };
#include "geojsoncomponent_codegen.cpp"


    constexpr openspace::properties::Property::PropertyInfo FlyToFeatureInfo = {
        "FlyToFeature",
        "Fly To Feature",
        "Triggering this leads to the camera flying to a position that show the GeoJSON "
        "feature. The flight will account for any lat, long or height offset."
    };

    constexpr openspace::properties::Property::PropertyInfo CentroidCoordinateInfo = {
        "CentroidCoordinate",
        "Centroid Coordinate",
        "The lat long coordinate of the centroid position of the read geometry. Note "
        "that this value does not incude the offset"
    };

    constexpr openspace::properties::Property::PropertyInfo BoundingBoxInfo = {
        "BoundingBox",
        "Bounding Box",
        "The lat long coordinates of the lower and upper corner of the bounding box of "
        "the read geometry. Note that this value does not incude the offset"
    };

} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GeoJsonComponent::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_geojsoncomponent");
}

GeoJsonComponent::NavigationFeature::NavigationFeature(
                                       properties::PropertyOwner::PropertyOwnerInfo info)
    : properties::PropertyOwner(info)
    , enabled(EnabledInfo, true)
    , flyToFeature(FlyToFeatureInfo)
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
{
    // From fadeable
    addProperty(_opacity);
    addProperty(_fade);

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
    , _globeNode(globe)
    , _geoJsonFile(FileInfo)
    , _heightOffset(HeightOffsetInfo, 0.f, -1e12f, 1e12f)
    , _latLongOffset(
        CoordinateOffsetInfo,
        glm::vec2(0.f),
        glm::vec2(-90.0),
        glm::vec2(90.f)
    )
    , _drawWireframe(DrawWireframeInfo, false)
    , _preventUpdatesFromHeightMap(PreventHeightUpdateInfo, false)
    , _forceUpdateHeightData(ForceUpdateHeightDataInfo)
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

    const float minGlobeRadius = static_cast<float>(
        _globeNode.ellipsoid().minimumRadius()
    );

    _heightOffset = p.heightOffset.value_or(_heightOffset);
    _heightOffset.onChange([this]() { _heightOffsetIsDirty = true; });
    _heightOffset.setMinValue(-0.9f * minGlobeRadius);
    _heightOffset.setMaxValue(5.f * minGlobeRadius);
    addProperty(_heightOffset);

    _latLongOffset = p.coordinateOffset.value_or(_latLongOffset);
    _latLongOffset.onChange([this]() { _dataIsDirty = true; });
    addProperty(_latLongOffset);

    if (p.defaultProperties.has_value()) {
        _defaultProperties.createFromDictionary(*p.defaultProperties);
    }
    addPropertySubOwner(_defaultProperties);

    _defaultProperties.pointTexture.onChange([&]() {
        std::filesystem::path texturePath = _defaultProperties.pointTexture.value();
        // Not ethat an empty texture is also valid => use default texture from module
        if (std::filesystem::is_regular_file(texturePath) || texturePath.empty()) {
            _textureIsDirty = true;
        }
        else {
            LERROR(fmt::format(
                "Provided texture file does not exist: '{}'",
                _defaultProperties.pointTexture
            ));
        }
    });

    _defaultProperties.shouldTesselate.onChange([&]() { _dataIsDirty = true; });
    _defaultProperties.tesselationLevel.onChange([&]() { _dataIsDirty = true; });
    _defaultProperties.tesselationMaxDistance.onChange([&]() { _dataIsDirty = true; });

    _forceUpdateHeightData.onChange([this]() {
        for (GlobeGeometryFeature& f : _geometryFeatures) {
            f.updateHeightsFromHeightMap();
        }
    });
    addProperty(_forceUpdateHeightData);

    addProperty(_preventUpdatesFromHeightMap);

    _drawWireframe = p.drawWireframe.value_or(_drawWireframe);
    addProperty(_drawWireframe);

    readFile();

    if (p.lightSources.has_value()) {
        std::vector<ghoul::Dictionary> lightsources = *p.lightSources;

        for (const ghoul::Dictionary& lsDictionary : lightsources) {
            std::unique_ptr<LightSource> lightSource =
                LightSource::createFromDictionary(lsDictionary);
            _lightSources.push_back(std::move(lightSource));
            _lightSourcePropertyOwner.addPropertySubOwner(_lightSources.back().get());
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
    bool isReady = std::all_of(
        std::begin(_geometryFeatures),
        std::end(_geometryFeatures),
        [](const GlobeGeometryFeature& g) {
            return g.isReady();
        }
    );
    return isReady &&
        (_linesAndPolygonsProgram != nullptr) && (_pointsProgram != nullptr);
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

    // Do two render passes, to properly render opacoty of overlaying objects
    for (int renderPass = 0; renderPass < 2; ++renderPass) {
        for (size_t i = 0; i < _geometryFeatures.size(); ++i) {
            if (_features[i]->enabled && _features[i]->isVisible()) {
                _geometryFeatures[i].render(
                    data,
                    renderPass,
                    opacity() * _features[i]->opacity(),
                    _lightsourceRenderData
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

    glm::vec3 offsets = glm::vec3(_latLongOffset.value(), _heightOffset);

    for (size_t i = 0; i < _geometryFeatures.size(); ++i) {
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
        LERROR(fmt::format("Failed to open geoJson file: {}", _geoJsonFile));
        return;
    }

    _geometryFeatures.clear();

    std::string content(
        (std::istreambuf_iterator<char>(file)),
        (std::istreambuf_iterator<char>())
    );

    // Parse GeoJSON string into GeoJSON objects
    using namespace geos::io;
    GeoJSONReader reader;

    try {
        GeoJSONFeatureCollection fc = reader.readFeatures(content);

        for (const GeoJSONFeature& feature : fc.getFeatures()) {
            parseSingleFeature(feature);
        }

        if (_geometryFeatures.empty()) {
            LWARNING(fmt::format(
                "No GeoJSON features could be successfully created for GeoJSON layer "
                "with identifier '{}'. Disabling layer.", this->identifier()
            ));
            _enabled = false;
        }
    }
    catch (const geos::util::GEOSException& e) {
        LERROR(fmt::format(
            "Error creating GeoJSON layer with identifier '{}'. Problem reading "
            "GeoJSON file '{}'. Error: '{}'", this->identifier(), _geoJsonFile, e.what()
        ));
    }
}

void GeoJsonComponent::parseSingleFeature(const geos::io::GeoJSONFeature& feature) {
    // Read the geometry
    const geos::geom::Geometry* geom = feature.getGeometry();

    // Read the properties
    GeoJsonOverrideProperties propsFromFile = propsFromGeoJson(feature);

    std::vector<const geos::geom::Geometry*> geomsToAdd;
    if (geom->isPuntal()) {
        // Handle all point features as one feature, even multi-points
        geomsToAdd = { geom };
    }
    else {
        size_t nGeom = geom->getNumGeometries();
        geomsToAdd.reserve(nGeom);
        for (size_t i = 0; i < nGeom; ++i) {
            geomsToAdd.push_back(geom->getGeometryN(i));
        }
    }

    // Split other collection features into multiple individual rendered components

    for (const geos::geom::Geometry* geometry : geomsToAdd) {
        const int index = static_cast<int>(_geometryFeatures.size());
        try {
            GlobeGeometryFeature g(_globeNode, _defaultProperties, propsFromFile);
            g.createFromSingleGeosGeometry(geometry, index);
            g.initializeGL(_pointsProgram.get(), _linesAndPolygonsProgram.get());
            _geometryFeatures.push_back(std::move(g));

            std::string name = _geometryFeatures.back().key();
            properties::PropertyOwner::PropertyOwnerInfo info = {
                makeIdentifier(name),
                name
                // @TODO: Use description from file, if any
            };
            _features.push_back(std::make_unique<NavigationFeature>(info));

            addMetaPropertiesToFeature(*_features.back(), index, geometry);

            _featuresPropertyOwner.addPropertySubOwner(_features.back().get());
        }
        catch (const ghoul::MissingCaseException&) {
            LERROR(fmt::format(
                "Error creating GeoJSON layer with identifier '{}'. Problem reading "
                "feature {} in GeoJSON file '{}'.", this->identifier(), index, _geoJsonFile
            ));
            // Do nothing
        }
    }
}

void GeoJsonComponent::addMetaPropertiesToFeature(NavigationFeature& feature, int index,
                                                  const geos::geom::Geometry* geometry)
{
    std::unique_ptr<geos::geom::Point> centroid = geometry->getCentroid();
    geos::geom::Coordinate centroidCoord = *centroid->getCoordinate();
    glm::vec2 centroidLatLong = glm::vec2(centroidCoord.y, centroidCoord.x);
    feature.centroidLatLong = centroidLatLong;

    std::unique_ptr<geos::geom::Geometry> boundingbox = geometry->getEnvelope();
    std::unique_ptr<geos::geom::CoordinateSequence> coords = boundingbox->getCoordinates();
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
    Geodetic2 pos0 = {
        glm::radians(boundingboxLatLong.x),
        glm::radians(boundingboxLatLong.y)
    };

    Geodetic2 pos1 = {
        glm::radians(boundingboxLatLong.z),
        glm::radians(boundingboxLatLong.w)
    };
    feature.boundingBoxDiagonal = static_cast<float>(
        _globeNode.ellipsoid().greatCircleDistance(pos0, pos1)
    );

    feature.flyToFeature.onChange([this, index]() { flyToFeature(index); });
}

void GeoJsonComponent::flyToFeature(int index) const {
    const NavigationFeature* f = _features[index].get();

    // Compute a good distance to travel to based on the feature's size.
    // Assumes 80 degree FOV
    float d = f->boundingBoxDiagonal / glm::tan(glm::radians(40.f));
    d += _heightOffset;

    float lat = f->centroidLatLong.value().x + _latLongOffset.value().x;
    float lon = f->centroidLatLong.value().y + _latLongOffset.value().y;

    global::scriptEngine->queueScript(
        fmt::format(
            "openspace.globebrowsing.flyToGeo({}, {}, {})", lat, lon, d
        ),
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

} // namespace openspace::globebrowsing
