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

#include <modules/globebrowsing/src/tileprovider/planetarytrailtileprovider.h>

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
    enum class KernelSize {
        Disabled = 0,
        Five = 5,
        Nine = 9,
        Thirteen = 13
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
        "The start time for the range of this planetary trail. Its internal"
        "start time will be adjusted with this value.",
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
        "This value determines the RGB color for the planetary trail",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo CutoffInfo = {
        "CutOff",
        "Cutoff",
        "This value specifies the trail cutoff (in seconds). The higher the value the "
        "longer the tail of the trail will be.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width planetary trail.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo KernelSizeInfo = {
        "KernelSize",
        "Kernel size",
        "Specifies the kernel size of the gaussian blur filter used to smooth out"
        "the edges of rendered path.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RenderFullTrailInfo = {
        "RenderFullTrail",
        "Render Full Trail",
        "If this value is set to 'true', the entire planetary trail will be rendered;"
        "if it is 'false', only the portion of the trail up until the current time"
        "in the application will be shown",
        openspace::properties::Property::Visibility::NoviceUser
    };

    struct [[codegen::Dictionary(PlanetaryTrailTileProvider)]] Parameters {
        // [[codegen::verbatim(JSONPathInfo.description)]]
        std::string JSON;

        // [[codegen::verbatim(StartTimeInfo.description)]]
        std::string startTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // [[codegen::verbatim(ResolutionInfo.description)]]
        std::optional<int> resolution;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(CutoffInfo.description)]]
        std::optional<float> cutoff;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(RenderFullTrailInfo.description)]]
        std::optional<bool> renderFullTrail;

        enum class [[codegen::map(KernelSize)]] KernelSize {
            Disabled = 0,
            Five = 5,
            Nine = 9,
            Thirteen = 13
        };
        // [[codegen::verbatim(KernelSizeInfo.description)]]
        std::optional<KernelSize> kernelSize;
    };

#include "planetarytrailtileprovider_codegen.cpp"
}

namespace openspace::globebrowsing {

PlanetaryTrailTileProvider::PlanetaryTrailTileProvider(const ghoul::Dictionary& dictionary) :
    _JSONPath(JSONPathInfo),
    _startTime(StartTimeInfo),
    _resolution(ResolutionInfo, 4096, 256, OpenGLCap.max2DTextureSize()),
    _color(ColorInfo, glm::vec3(1.f), glm::vec3(1.f), glm::vec3(1.f)),
    _lineWidth(LineWidthInfo, 10.f, 1.f, 1000.f),
    _cutoff(CutoffInfo, 30.f, 0.f, 3600.f, 1.f),
    _renderFullTrail(RenderFullTrailInfo, false),
    _kernelSize(KernelSizeInfo,
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

    addProperty(_cutoff);

    addProperty(_renderFullTrail);

    _kernelSize.addOptions({
        { static_cast<int>(KernelSize::Disabled), "Disabled" },
        { static_cast<int>(KernelSize::Five), "5" },
        { static_cast<int>(KernelSize::Nine), "9" },
        { static_cast<int>(KernelSize::Thirteen), "13" },
        });
    addProperty(_kernelSize);

    const Parameters p = codegen::bake<Parameters>(dictionary);
    _JSONPath = p.JSON;
    _startTime = p.startTime;
    _resolution = p.resolution.value_or(_resolution);
    _color = p.color.value_or(_color);
    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _cutoff = p.cutoff.value_or(_cutoff);
    _renderFullTrail = p.renderFullTrail.value_or(_renderFullTrail);
    if (p.kernelSize.has_value()) {
        _kernelSize = static_cast<int>(codegen::map<KernelSize>(*p.kernelSize));
    }
}

PlanetaryTrailTileProvider::~PlanetaryTrailTileProvider() {}

void PlanetaryTrailTileProvider::internalInitialize() {
    ZoneScoped;

    const auto _loggerCat = "PlanetaryTrailTileProvider";
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
    glVertexAttribPointer(0, 3, GL_DOUBLE, GL_FALSE, 3 * sizeof(double), nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    _program = global::renderEngine->buildRenderProgram(
        "PlanetaryTrailProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/planetarytrail_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/planetarytrail_fs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/planetarytrail_gs.glsl")
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
    glTexParameteri(
        GL_TEXTURE_2D,
        GL_TEXTURE_MAG_FILTER,
        GL_LINEAR
    );
    glTexParameteri(
        GL_TEXTURE_2D,
        GL_TEXTURE_MIN_FILTER,
        GL_LINEAR
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

    static constexpr GLfloat vertices[] = {
        -1.f, -1.f,
        1.f, -1.f,
        -1.f,  1.f,
        -1.f,  1.f,
        1.f, -1.f,
        1.f,  1.f,
    };

    glGenVertexArrays(1, &_quadVao);
    glBindVertexArray(_quadVao);
    glGenBuffers(1, &_quadVbo);
    glBindBuffer(GL_ARRAY_BUFFER, _quadVbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        2 * sizeof(GL_FLOAT),
        reinterpret_cast<void*>(0)
    );
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    _texture2 = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(_rendertargetDimensions.x, _rendertargetDimensions.y, 1),
        GL_TEXTURE_2D
    );

    glBindTexture(GL_TEXTURE_2D, *_texture2);
    glTexParameteri(
        GL_TEXTURE_2D,
        GL_TEXTURE_MAG_FILTER,
        GL_LINEAR
    );
    glTexParameteri(
        GL_TEXTURE_2D,
        GL_TEXTURE_MIN_FILTER,
        GL_LINEAR
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

    glGenFramebuffers(1, &_fbo2);

    _program2 = global::renderEngine->buildRenderProgram(
        "PlanetaryTrailBlurProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/planetarytrail_blur_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/planetarytrail_blur_fs.glsl")
    );

    update();
}

void PlanetaryTrailTileProvider::internalDeinitialize() {
    glDeleteFramebuffers(1, &_fbo);
    glDeleteBuffers(1, &_vbo);
    glDeleteVertexArrays(1, &_vao);
    _texture.reset();
}

Tile PlanetaryTrailTileProvider::tile(const globebrowsing::TileIndex& tileIndex) {
    return Tile{ _texture.get(), std::nullopt, Tile::Status::OK };
}

Tile::Status PlanetaryTrailTileProvider::tileStatus(const globebrowsing::TileIndex& tileIndex) {
    GeodeticPatch patch(tileIndex);
    if (_bounds.overlaps(patch)) {
        return Tile::Status::OK;
    }
    return globebrowsing::Tile::Status::OutOfRange;
}

TileDepthTransform PlanetaryTrailTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

globebrowsing::ChunkTile PlanetaryTrailTileProvider::chunkTile(
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

void PlanetaryTrailTileProvider::update() {
    ZoneScoped;

    // Necessary since update is called before internalInitialize
    // @TODO(jockekilby): remove when https://github.com/OpenSpace/OpenSpace/issues/2679 is fixed
    if (!_texture) {
        return;
    }

    std::vector<std::array<double, 3>> points;
    // add two for adjacency information in rendering
    points.reserve(_features.size() + 2);

    if (_renderFullTrail) {
        std::transform(
            _features.begin(),
            _features.end(),
            std::back_inserter(points),
            [](const Feature& feature) {
                return std::array<double, 3>{ feature._lat, feature._lon, 1.0 };
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
                double opacity = 1.0 - glm::smoothstep(
                    static_cast<double>(_start + feature._time),
                    static_cast<double>(_start + feature._time + _cutoff),
                    now);
                points.push_back({ feature._lat, feature._lon, opacity });
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
                        last._lon + fact * (next._lon - last._lon),
                        1.0
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

    glClearColor(.0f, .0f, .0f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);

    _program->activate();
    _program->setUniform("viewport",
        glm::vec2(_rendertargetDimensions.x, _rendertargetDimensions.y));
    _program->setUniform("lineWidth", _lineWidth);
    _program->setUniform("nPoints", static_cast<GLint>(points.size()-1));
    _program->setUniform("color", _color);
    _program->setUniform("projectionMatrix", projection);

    glBindVertexArray(_vao);

    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        points.size() * sizeof(std::array<double, 3>),
        points.data(),
        GL_DYNAMIC_DRAW
    );
    glDrawArrays(GL_LINE_STRIP_ADJACENCY, 0, points.size());

    if (_kernelSize.value() != static_cast<int>(KernelSize::Disabled)) {
        // Two-pass blur

        // First pass - horizontally
        glBindFramebuffer(GL_FRAMEBUFFER, _fbo2);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D,
            *_texture2.get(),
            0
        );

        glViewport(
            0,
            0,
            static_cast<GLsizei>(_rendertargetDimensions.x),
            static_cast<GLsizei>(_rendertargetDimensions.y)
        );
        glClear(GL_COLOR_BUFFER_BIT);

        _program2->activate();
        _program2->setUniform("resolution",
            glm::vec2(_rendertargetDimensions.x, _rendertargetDimensions.y));
        _program2->setUniform("direction", glm::vec2(1, 0));
        _program2->setUniform("kernelSize", _kernelSize.value());

        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, *_texture.get());
        glBindVertexArray(_quadVao);
        glDrawArrays(GL_TRIANGLES, 0, 6);

        // First pass - vertically
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
        glClear(GL_COLOR_BUFFER_BIT);

        _program2->activate();
        _program2->setUniform("resolution",
            glm::vec2(_rendertargetDimensions.x, _rendertargetDimensions.y));
        _program2->setUniform("direction", glm::vec2(0, 1));
        _program2->setUniform("kernelSize", _kernelSize.value());

        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, *_texture2.get());
        glBindVertexArray(_quadVao);
        glDrawArrays(GL_TRIANGLES, 0, 6);
    }

    // Reset FBO, shader program and viewport
    glUseProgram(prevProgram);
    glBindFramebuffer(GL_FRAMEBUFFER, prevFBO);
    global::renderEngine->openglStateCache().resetViewportState();
}

void PlanetaryTrailTileProvider::reset() {
}

int PlanetaryTrailTileProvider::minLevel() {
    return 1;
}

int PlanetaryTrailTileProvider::maxLevel() {
    return 1337;
}

float PlanetaryTrailTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
