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

#include <modules/globebrowsing/src/tileprovider/temporaltileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <modules/globebrowsing/src/tileprovider/defaulttileprovider.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>
#include "cpl_minixml.h"
#include <fstream>

namespace {
    constexpr const char* KeyBasePath = "BasePath";

    constexpr const char* UrlTimePlaceholder = "${OpenSpaceTimeId}";
    //constexpr const char* TimeStart = "OpenSpaceTimeStart";
    //constexpr const char* TimeEnd = "OpenSpaceTimeEnd";
    //constexpr const char* TimeResolution = "OpenSpaceTimeResolution";
    //constexpr const char* TimeFormat = "OpenSpaceTimeIdFormat";
    //constexpr const char* TimeInterpolation = "OpenSpaceTimeInterpolation";
    //constexpr const char* TransferFunction = "OpenSpaceTransferFunction";
    
    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "This is the path to the XML configuration file that describes the temporal tile "
        "information."
    };

    constexpr openspace::properties::Property::PropertyInfo UseFixedTimeInfo = {
        "UseFixedTime",
        "Use Fixed Time",
        "If this value is enabled, the time-varying timevarying dataset will always use "
        "the time that is specified in the 'FixedTime' property, rather than using the "
        "actual time from OpenSpace"
    };

    constexpr openspace::properties::Property::PropertyInfo FixedTimeInfo = {
        "FixedTime",
        "Fixed Time",
        "If the 'UseFixedTime' is enabled, this time will be used instead of the actual "
        "time taken from OpenSpace for the displayed tiles."
    };

    struct [[codegen::Dictionary(TemporalTileProvider)]] Parameters {
        // [[codegen::verbatim(FilePathInfo.description)]]
        std::string filePath;

        // [[codegen::verbatim(UseFixedTimeInfo.description)]]
        std::optional<bool> useFixedTime;

        // [[codegen::verbatim(FixedTimeInfo.description)]]
        std::optional<std::string> fixedTime;

        struct Generative {
            struct Time {
                // The (inclusive) starting time of the temporal image range
                std::string start;
                // The (inclusive) ending time of the temporal image range
                std::string end;
            };
            // The starting and ending times for the range of values
            Time time;

            // The temporal resolution between each image
            std::string temporalResolution;

            // The specification of the date format that is used in the tile provider
            std::string timeFormat;
        };
        std::optional<Generative> generative;

        std::optional<bool> interpolation;

        std::optional<std::string> colormap;
    };
#include "temporaltileprovider_codegen.cpp"


    // Buffer needs at least 22 characters space
    std::string_view timeStringify(
                      openspace::globebrowsing::TemporalTileProvider::TimeFormatType type,
                                                                 const openspace::Time& t)
    {
        ZoneScoped

        using namespace openspace;
        using namespace openspace::globebrowsing;

        char* buffer = reinterpret_cast<char*>(
            global::memoryManager->TemporaryMemory.allocate(22)
        );

        std::memset(buffer, 0, 22);
        const double time = t.j2000Seconds();

        switch (type) {
            case TemporalTileProvider::TimeFormatType::YYYY_MM_DD:
            {
                constexpr const char Format[] = "YYYY-MM-DD";
                constexpr const int Size = sizeof(Format);
                SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
                return std::string_view(buffer, Size - 1);
            }
            case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmmss: {
                constexpr const char Format[] = "YYYYMMDD_HRMNSC";
                constexpr const int Size = sizeof(Format);
                SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
                return std::string_view(buffer, Size - 1);
            }
            case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmm: {
                constexpr const char Format[] = "YYYYMMDD_HRMN";
                constexpr const int Size = sizeof(Format);
                SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
                return std::string_view(buffer, Size - 1);
            }
            case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThhColonmmColonssZ:
            {
                constexpr const char Format[] = "YYYY-MM-DDTHR:MN:SCZ";
                constexpr const int Size = sizeof(Format);
                SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
                return std::string_view(buffer, Size - 1);
            }
            case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThh_mm_ssZ: {
                constexpr const char Format[] = "YYYY-MM-DDTHR_MN_SCZ";
                constexpr const int Size = sizeof(Format);
                SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
                return std::string_view(buffer, Size - 1);
            }
            default:
                throw ghoul::MissingCaseException();
        }
    }
} // namespace

namespace ghoul {
    template <>
    constexpr openspace::globebrowsing::TemporalTileProvider::TimeFormatType
        from_string(std::string_view string)
    {
        using namespace openspace::globebrowsing;
        if (string == "YYYY-MM-DD") {
            return TemporalTileProvider::TimeFormatType::YYYY_MM_DD;
        }
        else if (string == "YYYY-MM-DDThh:mm:ssZ") {
            return TemporalTileProvider::TimeFormatType::YYYY_MM_DDThhColonmmColonssZ;
        }
        else if (string == "YYYY-MM-DDThh_mm_ssZ") {
            return TemporalTileProvider::TimeFormatType::YYYY_MM_DDThh_mm_ssZ;
        }
        else if (string == "YYYYMMDD_hhmmss") {
            return TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmmss;
        }
        else if (string == "YYYYMMDD_hhmm") {
            return TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmm;
        }
        else {
            throw ghoul::RuntimeError("Unknown timeformat '" + std::string(string) + "'");
        }
    }
} // namespace ghoul

namespace openspace::globebrowsing {

TemporalTileProvider::TemporalTileProvider(const ghoul::Dictionary& dictionary)
    : _initDict(dictionary)
    , _filePath(FilePathInfo)
    , _useFixedTime(UseFixedTimeInfo, false)
    , _fixedTime(FixedTimeInfo)
{
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _filePath = p.filePath;
    addProperty(_filePath);

    _useFixedTime = p.useFixedTime.value_or(_useFixedTime);
    addProperty(_useFixedTime);

    _fixedTime = p.fixedTime.value_or(_fixedTime);
    addProperty(_fixedTime);


    // File path was not a path to a file but a GDAL config or empty
    std::filesystem::path f(_filePath.value());
    if (std::filesystem::is_regular_file(f)) {
        _initDict.setValue(KeyBasePath, f.parent_path().string());
    }

    _colormap = p.colormap.value_or(_colormap);

    if (p.generative.has_value()) {
        Time start = Time(p.generative->time.start);
        Time end = Time::now();
        Time endOfInterval = Time(p.generative->time.end);
        _startTimeJ2000 = start.j2000Seconds();
        _endTimeJ2000 = endOfInterval.j2000Seconds();
        if (p.generative->time.end == "Yesterday") {
            end.advanceTime(-60.0 * 60.0 * 24.0); // Go back one day
        }
        else if (p.generative->time.end != "Today") {
            end.setTime(p.generative->time.end);
        }

        try {
            _timeQuantizer.setStartEndRange(
                std::string(start.ISO8601()),
                std::string(end.ISO8601())
            );
            _timeQuantizer.setResolution(p.generative->temporalResolution);
            _myResolution = p.generative->temporalResolution;
        }
        catch (const ghoul::RuntimeError& e) {
            throw ghoul::RuntimeError(fmt::format(
                "Could not create time quantizer for Temporal GDAL dataset '{}'. {}",
                _filePath.value(), e.message
            ));
        }
        _timeFormat = ghoul::from_string<TimeFormatType>(p.generative->timeFormat);
    }
    _interpolation = p.interpolation.value_or(_interpolation);

    _gdalXmlTemplate = p.filePath;

    if (_interpolation) {
        _interpolateTileProvider = std::make_unique<InterpolateTileProvider>(dictionary);
        _interpolateTileProvider->colormap = _colormap;
        _interpolateTileProvider->initialize();
        ghoul::Dictionary dict;
        dict.setValue("FilePath", _colormap);
        _interpolateTileProvider->singleImageProvider =
            std::make_unique<SingleImageProvider>(dict);
    }
}

void TemporalTileProvider::ensureUpdated() {
    ZoneScoped

    if (!_currentTileProvider) {
        update();
    }
}

Tile TemporalTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    ensureUpdated();
    return _currentTileProvider->tile(tileIndex);
}

Tile::Status TemporalTileProvider::tileStatus(const TileIndex& index) {
    ensureUpdated();
    return _currentTileProvider->tileStatus(index);
}

TileDepthTransform TemporalTileProvider::depthTransform() {
    ensureUpdated();
    return _currentTileProvider->depthTransform();
}

void TemporalTileProvider::update() {
    TileProvider* newCurr = tileProvider(global::timeManager->time());
    if (newCurr) {
        _currentTileProvider = newCurr;
    }
    if (_currentTileProvider) {
        _currentTileProvider->update();
    }
}

void TemporalTileProvider::reset() {
    for (std::pair<const TimeKey, std::unique_ptr<TileProvider>>& it : _tileProviderMap) {
        it.second->reset();
    }
}

int TemporalTileProvider::maxLevel() {
    ensureUpdated();
    return _currentTileProvider->maxLevel();
}

float TemporalTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

std::unique_ptr<TileProvider> TemporalTileProvider::initTileProvider(
                                                                 std::string_view timekey)
{
    ZoneScoped

    static const std::vector<std::string> IgnoredTokens = {
        // From: http://www.gdal.org/frmt_wms.html
        "${x}",
        "${y}",
        "${z}",
        "${version}",
        "${format}",
        "${layer}"
    };


    std::string xmlTemplate(_gdalXmlTemplate);
    const size_t pos = xmlTemplate.find(UrlTimePlaceholder);
    const size_t numChars = strlen(UrlTimePlaceholder);
    // @FRAGILE:  This will only find the first instance. Dangerous if that instance is
    // commented out ---abock
    std::string xml = xmlTemplate.replace(pos, numChars, timekey);

    xml = FileSys.expandPathTokens(std::move(xml), IgnoredTokens).string();

    _initDict.setValue("FilePath", xml);
    return std::make_unique<DefaultTileProvider>(_initDict);
}

TileProvider* TemporalTileProvider::tileProvider(std::string_view timekey) {
    ZoneScoped

    // @TODO (abock, 2020-08-20) This std::string creation can be removed once we switch
    // to C++20 thanks to P0919R2
    const auto it = _tileProviderMap.find(std::string(timekey));
    if (it != _tileProviderMap.end()) {
        return it->second.get();
    }
    else {
        std::unique_ptr<TileProvider> tileProvider = initTileProvider(timekey);
        tileProvider->initialize();

        TileProvider* res = tileProvider.get();
        _tileProviderMap[std::string(timekey)] = std::move(tileProvider);
        return res;
    }
}

TileProvider* TemporalTileProvider::tileProvider(const Time& time) {
    ZoneScoped

    if (!_interpolation) {
        if (_useFixedTime && !_fixedTime.value().empty()) {
            try {
                return tileProvider(_fixedTime.value());
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC("TemporalTileProvider", e.message);
                return nullptr;
            }
        }
        else {
            Time tCopy(time);
            if (_timeQuantizer.quantize(tCopy, true)) {
                std::string_view timeStr = timeStringify(_timeFormat, tCopy);
                try {
                    return tileProvider(timeStr);
                }
                catch (const ghoul::RuntimeError& e) {
                    LERRORC("TemporalTileProvider", e.message);
                    return nullptr;
                }
            }
        }
    }

    Time tCopy(time);
    if (!_timeQuantizer.quantize(tCopy, true)) {
        return nullptr;
    }

    Time simulationTime(time);
    Time nextTile;
    Time nextNextTile;
    Time prevTile;
    Time secondToLast;
    Time secondToFirst;

    std::string_view tCopyStr = timeStringify(_timeFormat, tCopy);
    try {
        _interpolateTileProvider->t1 = tileProvider(tCopyStr);
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC("TemporalTileProvider", e.message);
        return nullptr;
    }
    // if the images are for each hour
    if (_myResolution == "1h") {
        // the second tile to interpolate between
        nextTile.setTime(tCopy.j2000Seconds() + 60 * 60);
        // the tile after the second tile
        nextNextTile.setTime(tCopy.j2000Seconds() + 120 * 60);
        // the tile before the first tile
        prevTile.setTime(tCopy.j2000Seconds() - 60 * 60 + 1);
        // to make sure that an image outside the dataset is not searched for both ends of
        // the dataset are calculated
        secondToLast.setTime(_endTimeJ2000 - 60 * 60);
        secondToFirst.setTime(_startTimeJ2000 + 60 * 60);
    }
    // if the images are for each month
    if (_myResolution == "1M") {
        // the second tile to interpolate between
        nextTile.setTime(tCopy.j2000Seconds() + 32 * 60 * 60 * 24);
        // the tile after the second tile
        nextNextTile.setTime(tCopy.j2000Seconds() + 64 * 60 * 60 * 24);
        // the tile before the first tile
        prevTile.setTime(tCopy.j2000Seconds() - 2 * 60 * 60 * 24);
        // to make sure that an image outside the dataset is not searched for both ends of
        // the dataset are calculated
        secondToLast.setTime(_endTimeJ2000 - 2 * 60 * 60 * 24);
        secondToFirst.setTime(_startTimeJ2000 + 32 * 60 * 60 * 24);

        // since months vary in length the time strings are set to the first of each month
        auto setToFirstOfMonth = [](Time& time) {
            std::string timeString = std::string(time.ISO8601());
            timeString[8] = '0';
            timeString[9] = '1';
            time.setTime(timeString);
        };

        setToFirstOfMonth(nextTile);
        setToFirstOfMonth(nextNextTile);
        setToFirstOfMonth(prevTile);
        setToFirstOfMonth(secondToLast);
        setToFirstOfMonth(secondToFirst);
    }

    std::string_view nextTileStr = timeStringify(_timeFormat, nextTile);
    std::string_view nextNextTileStr = timeStringify(_timeFormat, nextNextTile);
    std::string_view prevTileStr = timeStringify(_timeFormat, prevTile);
    try {
        // the necessary tile providers are loaded if they exist within the
        // dataset's timespan
        if (secondToLast.j2000Seconds() > simulationTime.j2000Seconds() &&
            secondToFirst.j2000Seconds() < simulationTime.j2000Seconds())
        {
            _interpolateTileProvider->t2 = tileProvider(nextTileStr);
            _interpolateTileProvider->future = tileProvider(nextNextTileStr);
            _interpolateTileProvider->before = tileProvider(prevTileStr);
        }
        else if (secondToLast.j2000Seconds() < simulationTime.j2000Seconds() &&
                 _endTimeJ2000 > simulationTime.j2000Seconds())
        {
            _interpolateTileProvider->t2 = tileProvider(nextTileStr);
            _interpolateTileProvider->future = tileProvider(tCopyStr);
            _interpolateTileProvider->before = tileProvider(prevTileStr);
        }
        else if (secondToFirst.j2000Seconds() > simulationTime.j2000Seconds() &&
                 _startTimeJ2000 < simulationTime.j2000Seconds())
        {
            _interpolateTileProvider->t2 = tileProvider(nextTileStr);
            _interpolateTileProvider->future = tileProvider(nextNextTileStr);
            _interpolateTileProvider->before = tileProvider(tCopyStr);
        }
        else {
            _interpolateTileProvider->t2 = tileProvider(tCopyStr);
            _interpolateTileProvider->future = tileProvider(tCopyStr);
            _interpolateTileProvider->before = tileProvider(tCopyStr);
        }
        _interpolateTileProvider->factor =
            (simulationTime.j2000Seconds() - tCopy.j2000Seconds()) /
            (nextTile.j2000Seconds() - tCopy.j2000Seconds());

        if (_interpolateTileProvider->factor > 1) {
            _interpolateTileProvider->factor = 1;
        }
        return _interpolateTileProvider.get();
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC("TemporalTileProvider", e.message);
        return nullptr;
    }
}

TemporalTileProvider::InterpolateTileProvider::InterpolateTileProvider(
                                                                 const ghoul::Dictionary&)
{
    ZoneScoped

    glGenFramebuffers(1, &fbo);
    glGenVertexArrays(1, &vaoQuad);
    glGenBuffers(1, &vboQuad);
    glBindVertexArray(vaoQuad);
    glBindBuffer(GL_ARRAY_BUFFER, vboQuad);
    tileCache = global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
    // Quad for fullscreen with vertex (xy) and texture coordinates (uv)
    const GLfloat vertexData[] = {
        // x    y    u    v
        -1.f, -1.f, 0.f, 0.f,
         1.f,  1.f, 1.f, 1.f,
        -1.f,  1.f, 0.f, 1.f,
        -1.f, -1.f, 0.f, 0.f,
         1.f, -1.f, 1.f, 0.f,
         1.f,  1.f, 1.f, 1.f
    };
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    // vertex coordinates at location 0
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);
    glEnableVertexAttribArray(0);
    // texture coords at location 1
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        4 * sizeof(GLfloat),
        reinterpret_cast<void*>(2 * sizeof(GLfloat))
    );
    glEnableVertexAttribArray(1);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    shaderProgram = global::renderEngine->buildRenderProgram(
        "InterpolatingProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/interpolate_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/interpolate_fs.glsl")
    );
}

TemporalTileProvider::InterpolateTileProvider::~InterpolateTileProvider() {
    glDeleteFramebuffers(1, &fbo);
    glDeleteBuffers(1, &vboQuad);
    glDeleteVertexArrays(1, &vaoQuad);
}

Tile TemporalTileProvider::InterpolateTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    TracyGpuZone("tile");

    // prev and next are the two tiles to interpolate between
    Tile prev = t1->tile(tileIndex);
    Tile next = t2->tile(tileIndex);
    // the tile before and the tile after the interpolation interval are loaded so the
    // interpolation goes smoother
    Tile prevprev = before->tile(tileIndex);
    Tile nextnext = future->tile(tileIndex);
    cache::ProviderTileKey key = { tileIndex, uniqueIdentifier };

    if (!prev.texture || !next.texture) {
        return Tile{ nullptr, std::nullopt, Tile::Status::Unavailable };
    }

    // There is a previous and next texture to interpolate between so do the interpolation

    // The texture that will give the color for the interpolated texture
    ghoul::opengl::Texture* colormapTexture = singleImageProvider->tile(tileIndex).texture;
    long long hkey = cache::ProviderTileHasher()(key);
    // The data for initializing the texture
    TileTextureInitData initData(
        prev.texture->dimensions().x,
        prev.texture->dimensions().y,
        prev.texture->dataType(),
        prev.texture->format(),
        TileTextureInitData::PadTiles::No,
        TileTextureInitData::ShouldAllocateDataOnCPU::No
    );

    // Check if a tile exists for the given key in the tileCache
    // Initializing the tile that will contian the interpolated texture
    Tile ourTile;
    // The texture that will contain the interpolated image
    ghoul::opengl::Texture* writeTexture;
    if (tileCache->exist(key)) {
        // Get the tile from the tilecache
        ourTile = tileCache->get(key);
        // Use the texture from the tileCache
        writeTexture = ourTile.texture;
    }
    else {
        // Create a texture with the initialization data
        writeTexture = tileCache->texture(initData);
        // Create a tile with the texture
        ourTile = Tile{ writeTexture, std::nullopt, Tile::Status::OK };
        // Add it to the tilecache
        tileCache->put(key, initData.hashKey, ourTile);
    }

    // Saves current state
    GLint currentFBO;
    GLint viewport[4];
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &currentFBO);
    global::renderEngine->openglStateCache().viewport(viewport);
    // Bind render texture to FBO
    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        *writeTexture,
        0
    );
    glDisable(GL_BLEND);
    GLenum textureBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, textureBuffers);
    // Check that our framebuffer is ok
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LERRORC("TileProvider", "Incomplete framebuffer");
    }
    // Setup our own viewport settings
    GLsizei w = static_cast<GLsizei>(writeTexture->width());
    GLsizei h = static_cast<GLsizei>(writeTexture->height());
    glViewport(0, 0, w, h);
    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);
    GLint id;
    glGetIntegerv(GL_CURRENT_PROGRAM, &id);
    // Activate shader and bind uniforms
    shaderProgram->activate();
    shaderProgram->setUniform("blendFactor", factor);

    ghoul::opengl::TextureUnit colormapUnit;
    colormapUnit.activate();
    colormapTexture->bind();
    shaderProgram->setUniform("colormapTexture", colormapUnit);

    ghoul::opengl::TextureUnit prevUnit;
    prevUnit.activate();
    prev.texture->bind();
    shaderProgram->setUniform("prevTexture", prevUnit);

    ghoul::opengl::TextureUnit nextUnit;
    nextUnit.activate();
    next.texture->bind();
    shaderProgram->setUniform("nextTexture", nextUnit);

    // Render to the texture
    glBindVertexArray(vaoQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6); // 2 triangles
    // Deactivate shader program (when rendering is completed)
    shaderProgram->deactivate();
    glUseProgram(id);
    // Restores system state
    glBindFramebuffer(GL_FRAMEBUFFER, currentFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetColorState();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
    global::renderEngine->openglStateCache().resetViewportState();

    return ourTile;
}

Tile::Status TemporalTileProvider::InterpolateTileProvider::tileStatus(
                                                                   const TileIndex& index)
{
    Tile::Status t1Stat = t1->tileStatus(index);
    Tile::Status t2Stat = t2->tileStatus(index);
    if (t1Stat <= t2Stat) {
        return t1Stat;
    }
    else {
        return t2Stat;
    }
}

TileDepthTransform TemporalTileProvider::InterpolateTileProvider::depthTransform() {
    return t1->depthTransform();
}

void TemporalTileProvider::InterpolateTileProvider::update() {
    t1->update();
    t2->update();
    before->update();
    future->update();
}

void TemporalTileProvider::InterpolateTileProvider::reset() {
    t1->reset();
    t2->reset();
    before->reset();
    future->reset();
}

int TemporalTileProvider::InterpolateTileProvider::maxLevel() {
    return glm::min(t1->maxLevel(), t2->maxLevel());
}

float TemporalTileProvider::InterpolateTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
