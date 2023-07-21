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

#include <modules/globebrowsing/src/tileprovider/asdftileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <openspace/engine/globals.h>
#include <openspace/util/timemanager.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/spicemanager.h>
#include <openspace/json.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

#include <filesystem>

namespace {
    enum class RenderingMode {
        Lines = 0,
        Points,
    };

    constexpr openspace::properties::Property::PropertyInfo JSONPathInfo = {
        "JSON",
        "JSON",
        "The path of the JSON file to use with this tile provider",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo StartTimeInfo = {
        "StartTime",
        "Start Time",
        "The start time for the range of this SOMETHING",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ResolutionInfo = {
        "Resolution",
        "Resolution",
        "The resolution of the underlying rendertarget in the largest dimension",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB main color for the lines and points of the ASDF",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width or Point size of the ASDF",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RenderFullAsdfInfo = {
        "RenderFullAsdf",
        "Render Full Asdf",
        "If this value is set to 'true', the entire Asdf will be rendered; if it is "
        "'false', only the Asdf until the current time in the application will be shown",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderingModeInfo = {
        "Rendering",
        "Rendering Mode",
        "Determines how the asdf should be rendered to the screen. If 'Lines' is "
        "selected, only the line part is visible, if 'Points' is selected, only the "
        "corresponding points (and subpoints) are shown",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(AsdfTileProvider)]] Parameters {
        // [[codegen::verbatim(JSONPathInfo.description)]]
        std::string JSON;

        // [[codegen::verbatim(StartTimeInfo.description)]]
        std::string startTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // [[codegen::verbatim(ResolutionInfo.description)]]
        std::optional<int> resolution;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(RenderFullAsdfInfo.description)]]
        std::optional<bool> renderFullAsdf;

        enum class [[codegen::map(RenderingMode)]] RenderingMode {
            Lines = 0,
            Points
        };

        // [[codegen::verbatim(RenderingModeInfo.description)]]
        std::optional<RenderingMode> renderingMode;
    };

#include "asdftileprovider_codegen.cpp"
}

namespace openspace::globebrowsing {

AsdfTileProvider::AsdfTileProvider(const ghoul::Dictionary& dictionary) :
    _JSONPath(JSONPathInfo),
    _startTime(StartTimeInfo),
    _resolution(ResolutionInfo, 4096, 256, OpenGLCap.max2DTextureSize()),
    _color(ColorInfo, glm::vec3(1.f), glm::vec3(1.f), glm::vec3(1.f)),
    _lineWidth(LineWidthInfo, 10.f, 1.f, 250.f),
    _renderFullAsdf(RenderFullAsdfInfo, false),
    _renderingMode(RenderingModeInfo,
        openspace::properties::OptionProperty::DisplayType::Dropdown),
    _start(0.0),
    _fbo(0),
    _vao(0),
    _vbo(0),
    _bounds(-1.0, -1.0, -1.0, -1.0),
    _rendertargetDimensions(-1, -1)
{
    ZoneScoped;

    addProperty(_JSONPath);
    _JSONPath.setReadOnly(true);

    addProperty(_startTime);
    _startTime.setReadOnly(true);

    addProperty(_resolution);
    _resolution.setReadOnly(true);

    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    addProperty(_lineWidth);

    addProperty(_renderFullAsdf);

    _renderingMode.addOptions({
        { static_cast<int>(RenderingMode::Lines), "Lines" },
        { static_cast<int>(RenderingMode::Points), "Points" },
    });
    addProperty(_renderingMode);

    const Parameters p = codegen::bake<Parameters>(dictionary);
    _JSONPath = p.JSON;
    _startTime = p.startTime;
    _resolution = p.resolution.value_or(_resolution);
    _color = p.color.value_or(_color);
    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _renderFullAsdf = p.renderFullAsdf.value_or(_renderFullAsdf);
    if (p.renderingMode.has_value()) {
        _renderingMode = static_cast<int>(codegen::map<RenderingMode>(*p.renderingMode));
    }
}

AsdfTileProvider::~AsdfTileProvider() {}

void AsdfTileProvider::internalInitialize() {
    ZoneScoped;

    const auto _loggerCat = "AsdfTileProvider";
    std::ifstream file(absPath(_JSONPath));
    nlohmann::json content = nlohmann::json::parse(file);
    double minLon = std::numeric_limits<double>::max();
    double maxLon = -std::numeric_limits<double>::max();
    double minLat = std::numeric_limits<double>::max();
    double maxLat = -std::numeric_limits<double>::max();
    for (const auto feature : content.at("features")) {
        try {
            const int time = feature.at("properties").at("surface_seconds");

            const std::array<double, 2> lonlat = feature.at("geometry").at("coordinates");
            const double lon = glm::radians(lonlat[0]);
            const double lat = glm::radians(lonlat[1]);

            _features.push_back({ lon, lat, time });
            minLon = std::min(minLon, lon);
            maxLon = std::max(maxLon, lon);
            minLat = std::min(minLat, lat);
            maxLat = std::max(maxLat, lat);
        } catch (nlohmann::json::exception e) {
            // ... Raise a warning here about missing required data
        }
    }

    // To avoid points/lines being clipped at the end of the region-of-interest
    // we extend our region by 2.5% in all directions. (magic number)
    constexpr double ext = 1.025;
    const double halfSizeLon = std::abs(maxLon - minLon) / 2.0;
    const double halfSizeLat = std::abs(maxLat - minLat) / 2.0;
    _bounds = GeodeticPatch(
        maxLat - halfSizeLat,
        maxLon - halfSizeLon,
        halfSizeLat * ext,
        halfSizeLon * ext
    );

    // Convert the start and end time from string representations to J2000 seconds
    _start = SpiceManager::ref().ephemerisTimeFromDate(_startTime);

    glGenFramebuffers(1, &_fbo);
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_DOUBLE, GL_FALSE, 2 * sizeof(double), nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    _program = global::renderEngine->buildRenderProgram(
        "AsdfProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/asdf_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/asdf_fs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/asdf_gs.glsl")
    );

    const double aspect = halfSizeLon / halfSizeLat;
    _rendertargetDimensions = {
        aspect > 1.0 ? _resolution : _resolution * aspect,
        aspect > 1.0 ? _resolution / aspect : _resolution
    };

    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(_rendertargetDimensions.x, _rendertargetDimensions.y, 1),
        GL_TEXTURE_2D
    );
    glBindTexture(GL_TEXTURE_2D, *_texture);
    glTexParameterfv(
        GL_TEXTURE_2D,
        GL_TEXTURE_BORDER_COLOR,
        glm::value_ptr(glm::vec4(0.f, 0.f, 0.f, 0.f))
    );
    glTexParameteri(
        GL_TEXTURE_2D,
        GL_TEXTURE_WRAP_S,
        GL_CLAMP_TO_BORDER
    );
    glTexParameteri(
        GL_TEXTURE_2D,
        GL_TEXTURE_WRAP_T,
        GL_CLAMP_TO_BORDER
    );
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA,
        static_cast<GLsizei>(_rendertargetDimensions.x),
        static_cast<GLsizei>(_rendertargetDimensions.y),
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    glBindTexture(GL_TEXTURE_2D, 0);

    update();
}

void AsdfTileProvider::internalDeinitialize() {
    glDeleteFramebuffers(1, &_fbo);
    glDeleteBuffers(1, &_vbo);
    glDeleteVertexArrays(1, &_vao);
    _texture.reset();
}

Tile AsdfTileProvider::tile(const globebrowsing::TileIndex& tileIndex) {
    return Tile{ _texture.get(), std::nullopt, Tile::Status::OK };
}

Tile::Status AsdfTileProvider::tileStatus(const globebrowsing::TileIndex& tileIndex) {
    GeodeticPatch patch(tileIndex);
    if (_bounds.overlaps(patch)) {
        return Tile::Status::OK;
    }
    return globebrowsing::Tile::Status::OutOfRange;
}

TileDepthTransform AsdfTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

globebrowsing::ChunkTile AsdfTileProvider::chunkTile(
    globebrowsing::TileIndex tileIndex,
    int parents,
    int maxParents)
{
    using namespace globebrowsing;
    std::function<void(TileIndex&, TileUvTransform&)> ascendToParent =
        [](TileIndex& ti, TileUvTransform&) {
        ti.x /= 2;
        ti.y /= 2;
        ti.level--;
    };

    const GeodeticPatch patch(tileIndex);
    const double u0
        = (patch.minLon() - _bounds.minLon()) / (_bounds.maxLon() - _bounds.minLon());
    const double u1
        = (patch.maxLon() - _bounds.minLon()) / (_bounds.maxLon() - _bounds.minLon());
    const double v0
        = (patch.minLat() - _bounds.minLat()) / (_bounds.maxLat() - _bounds.minLat());
    const double v1
        = (patch.maxLat() - _bounds.minLat()) / (_bounds.maxLat() - _bounds.minLat());

    TileUvTransform uvTransform = {
        .uvOffset = glm::vec2(u0, v0),
        .uvScale = glm::vec2(u1 - u0, v1 - v0)
    };

    return traverseTree(tileIndex, parents, maxParents, ascendToParent, uvTransform);
}

void AsdfTileProvider::update() {
    ZoneScoped;

    // Necessary since update is called before internalInitialize
    // @TODO(jockekilby): remove when https://github.com/OpenSpace/OpenSpace/issues/2679 is fixed
    if (!_texture) {
        return;
    }

    std::vector<std::array<double, 2>> points;
    // add two for adjacency information in rendering
    points.reserve(_features.size() + 2);

    if (_renderFullAsdf) {
        std::transform(
            _features.begin(),
            _features.end(),
            std::back_inserter(points),
            [](const Feature& feature) {
                return std::array<double, 2>{ feature._lat, feature._lon };
       });
    }
    else {
        const double now = openspace::global::timeManager->time().j2000Seconds();
        if (_start <= now) {
            std::size_t index = 0;
            Feature last;

            // Add all feature-points up until the current time
            for (const Feature& feature : _features) {
                if (_start + feature._time > now) {
                    break;
                }
                points.push_back({ feature._lat, feature._lon });
                last = feature;
                index++;
            }

            // If there's still feature-points left, interpolate between current & next
            if (index > 0 && index < _features.size() - 1) {
                Feature next = _features[index];
                const double fact
                    = (now - (_start + last._time)) / (next._time - last._time);
                points.push_back(
                    {
                        last._lat + fact * (next._lat - last._lat),
                        last._lon + fact * (next._lon - last._lon)
                    }
                );
            }
        }
    }

    if(points.size() > 0) {
        // duplicate first & last points so we don't loose them
        // to adjacency information during rendering
        points.insert(points.begin(), points.front());
        points.push_back(points.back());
    }

    const glm::dmat4 projection = glm::ortho(
        _bounds.minLon(),
        _bounds.maxLon(),
        _bounds.minLat(),
        _bounds.maxLat()
    );

    GLint prevProgram, prevFBO;
    glGetIntegerv(GL_CURRENT_PROGRAM, &prevProgram);
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &prevFBO);

    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *_texture.get(),
        0
    );

    glViewport(
        0,
        0,
        static_cast<GLsizei>(_rendertargetDimensions.x),
        static_cast<GLsizei>(_rendertargetDimensions.y)
    );

    //glClearColor(.2f, .2f, .2f, 0.f);
    glClearColor(.0f, .0f, .0f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);

    _program->activate();
    _program->setUniform("viewport",
        glm::vec2(_rendertargetDimensions.x, _rendertargetDimensions.y));
    _program->setUniform("lineWidth", _lineWidth);
    const float blend = 2.0;
    //_program->setUniform("blendFactor", blend);
    _program->setUniform("color", _color);
    _program->setUniform("projectionMatrix", projection);

    glBindVertexArray(_vao);

    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        points.size() * sizeof(std::array<double, 2>),
        points.data(),
        GL_DYNAMIC_DRAW
    );

    if (_renderingMode == static_cast<int>(RenderingMode::Lines)) {
        glDrawArrays(GL_LINE_STRIP_ADJACENCY, 0, points.size());
    }
    else {
        glPointSize(_lineWidth);
        glDrawArrays(GL_POINTS, 0, points.size());
    }

    // Reset FBO, shader program and viewport
    glUseProgram(prevProgram);
    glBindFramebuffer(GL_FRAMEBUFFER, prevFBO);
    global::renderEngine->openglStateCache().resetViewportState();
}

void AsdfTileProvider::reset() {
}

int AsdfTileProvider::minLevel() {
    return 1;
}

int AsdfTileProvider::maxLevel() {
    return 1337;
}

float AsdfTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
