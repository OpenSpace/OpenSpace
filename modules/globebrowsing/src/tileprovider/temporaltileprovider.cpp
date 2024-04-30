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
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <sstream>

namespace {
    constexpr std::string_view TimePlaceholder = "${OpenSpaceTimeId}";

    constexpr openspace::properties::Property::PropertyInfo UseFixedTimeInfo = {
        "UseFixedTime",
        "Use Fixed Time",
        "If this value is enabled, the time-varying timevarying dataset will always use "
        "the time that is specified in the 'FixedTime' property, rather than using the "
        "actual time from OpenSpace.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FixedTimeInfo = {
        "FixedTime",
        "Fixed Time",
        "If the 'UseFixedTime' is enabled, this time will be used instead of the actual "
        "time taken from OpenSpace for the displayed tiles.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(TemporalTileProvider)]] Parameters {
        // [[codegen::verbatim(UseFixedTimeInfo.description)]]
        std::optional<bool> useFixedTime;

        // [[codegen::verbatim(FixedTimeInfo.description)]]
        std::optional<std::string> fixedTime;

        enum class Mode {
            Prototyped,
            Folder
        };
        // The mode that his temporal tile provider operates in. In the `Prototyped` mode,
        // a given start and end time, temporal resolution, and perscriptive time format
        // is used to generate the information used by GDAL to access the data. In the
        // `folder` method, a folder and a time format is provided and each file in the
        // folder is scanned using the time format instead
        Mode mode;

        struct Prototyped {
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

            // The specification of the date format that is used in the tile provider. The
            // time format must be specified in a manner appropriate for the SPICE
            // function `timout_c`.
            // https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/timout_c.html
            std::string timeFormat;

            // The text that will be used as the prototype to generate the data to load
            // the image layer. Any occurance of `${OpenSpaceTimeId}` in this prototype
            // is replaced with the current date according to the remaining information
            // such as the resolution and the format and the resulting text is used to
            // load the corresponding images
            std::string prototype;

        };
        std::optional<Prototyped> prototyped;

        struct Folder {
            // The folder that is parsed for files. Every file in the provided directory
            // is checked against the provided format and added if it adheres to said
            // format
            std::filesystem::path folder [[codegen::directory()]];

            // The format of files that is pared in the provided folder. The format string
            // has to be compatible to the C++ function get_time.
            // https://en.cppreference.com/w/cpp/io/manip/get_time
            std::string format;
        };
        std::optional<Folder> folder;

        // Determines whether this tile provider should interpolate between two adjacent
        // layers
        std::optional<bool> interpolation;

        // If provided, the tile provider will use this color map to convert a greyscale
        // image to color
        std::optional<std::string> colormap;
    };
#include "temporaltileprovider_codegen.cpp"

    std::string_view timeStringify(const std::string& format, const openspace::Time& t) {
        ZoneScoped;

        constexpr int BufferSize = 64;
        ghoul_assert(format.size() < BufferSize, "Format string too long");

        using namespace openspace;

        char FormatBuf[BufferSize];
        std::memset(FormatBuf, '\0', BufferSize);
        std::memcpy(FormatBuf, format.c_str(), format.size());

        char* OutBuf = reinterpret_cast<char*>(
            global::memoryManager->TemporaryMemory.allocate(BufferSize)
        );
        std::memset(OutBuf, '\0', BufferSize);

        const double time = t.j2000Seconds();
        SpiceManager::ref().dateFromEphemerisTime(time, OutBuf, BufferSize, FormatBuf);
        return std::string_view(OutBuf, format.size());
    }
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation TemporalTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_temporaltileprovider");
}

TemporalTileProvider::TemporalTileProvider(const ghoul::Dictionary& dictionary)
    : _initDict(dictionary)
    , _useFixedTime(UseFixedTimeInfo, false)
    , _fixedTime(FixedTimeInfo)
{
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    // Make sure that the user provided the data that they requested. The mode parameter
    // is a required one and these two if statements tie the table requirement to the mode
    if (p.mode == Parameters::Mode::Folder && !p.folder.has_value()) {
        throw ghoul::RuntimeError(
            "When selecting the `Folder` mode, a `Folder` table must be specified"
        );
    }
    if (p.mode == Parameters::Mode::Prototyped && !p.prototyped.has_value()) {
        throw ghoul::RuntimeError(
            "When selecting the `Prototyped` mode, a `Prototyped` table must be specified"
        );
    }

    _useFixedTime = p.useFixedTime.value_or(_useFixedTime);
    _useFixedTime.onChange([this]() { _fixedTimeDirty = true; });
    addProperty(_useFixedTime);

    _fixedTime = p.fixedTime.value_or(_fixedTime);
    _fixedTime.onChange([this]() { _fixedTimeDirty = true; });
    addProperty(_fixedTime);

    _colormap = p.colormap.value_or(_colormap);

    if (p.prototyped.has_value()) {
        _mode = Mode::Prototype;

        const Time start = Time(p.prototyped->time.start);
        Time end = Time::now();
        _prototyped.startTimeJ2000 = start.j2000Seconds();
        _prototyped.endTimeJ2000 = Time(p.prototyped->time.end).j2000Seconds();
        if (p.prototyped->time.end == "Yesterday") {
            end.advanceTime(-60.0 * 60.0 * 24.0); // Go back one day
        }
        else if (p.prototyped->time.end != "Today") {
            end.setTime(p.prototyped->time.end);
        }

        try {
            _prototyped.timeQuantizer.setStartEndRange(
                std::string(start.ISO8601()),
                std::string(end.ISO8601())
            );
            _prototyped.timeQuantizer.setResolution(p.prototyped->temporalResolution);
            _prototyped.temporalResolution = p.prototyped->temporalResolution;
        }
        catch (const ghoul::RuntimeError& e) {
            throw ghoul::RuntimeError(std::format(
                "Could not create time quantizer for Temporal GDAL dataset: {}", e.message
            ));
        }

        if (p.prototyped->timeFormat.size() >= 64) {
            throw ghoul::RuntimeError(std::format(
                "Time format string '{}' too large. Maximum length of 64 is allowed",
                p.prototyped->timeFormat
            ));
        }
        _prototyped.timeFormat = p.prototyped->timeFormat;
        _prototyped.prototype = p.prototyped->prototype;
    }

    if (p.folder.has_value()) {
        _mode = Mode::Folder;

        _folder.folder = p.folder->folder;
        _folder.format = p.folder->format;

        namespace fs = std::filesystem;
        for (const fs::directory_entry& path : fs::directory_iterator(_folder.folder)) {
            if (!path.is_regular_file()) {
                continue;
            }

            const std::string file = path.path().filename().string();
            std::istringstream ss = std::istringstream(file);

            std::tm tm = {};
            ss >> std::get_time(&tm, p.folder->format.c_str());
            if (!ss.fail()) {
                std::string date;
                if (p.folder->format.find("%j") != std::string::npos) {
                    // If the user asked for a day-of-year, the day-of-month and the month
                    // fields will not be set and calls to std::asctime will assert
                    // unfortunately.  Luckily, Spice understands DOY date formats, so
                    // we can specify those directly and noone would use a DOY and a DOM
                    // time string in the same format string, right?  Right?!
                    date = std::format(
                        "{}-{}T{}:{}:{}",
                        tm.tm_year + 1900,
                        tm.tm_yday,
                        tm.tm_hour,
                        tm.tm_min,
                        tm.tm_sec
                    );
                }
                else {
                    date = std::format(
                        "{}-{}-{} {}:{}:{}",
                        tm.tm_year + 1900,
                        tm.tm_mon + 1,
                        tm.tm_mday + 1,
                        tm.tm_hour,
                        tm.tm_min,
                        tm.tm_sec
                    );
                }

                const double et = SpiceManager::ref().ephemerisTimeFromDate(date);
                _folder.files.emplace_back(et, path.path().string());
            }
        }

        using K = double;
        using V = std::string;
        std::sort(
            _folder.files.begin(),
            _folder.files.end(),
            [](const std::pair<K, V>& lhs, const std::pair<K, V>& rhs) {
                return lhs.first < rhs.first;
            }
        );

        if (_folder.files.empty()) {
            throw ghoul::RuntimeError(std::format(
                "Error loading layer '{}'. Folder '{}' does not contain any files that "
                "matched the time format",
                _identifier, _folder.folder
            ));
        }
    }

    _isInterpolating = p.interpolation.value_or(_isInterpolating);
    if (_isInterpolating) {
        _interpolateTileProvider = std::make_unique<InterpolateTileProvider>(dictionary);
        _interpolateTileProvider->initialize();
        _interpolateTileProvider->colormap =
            ghoul::io::TextureReader::ref().loadTexture(_colormap, 1);
        _interpolateTileProvider->colormap->uploadTexture();
        _interpolateTileProvider->colormap->setFilter(
            ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
        );
    }
}

Tile TemporalTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped;
    if (!_currentTileProvider) {
        update();
    }

    return _currentTileProvider->tile(tileIndex);
}

Tile::Status TemporalTileProvider::tileStatus(const TileIndex& index) {
    if (!_currentTileProvider) {
        update();
    }

    return _currentTileProvider->tileStatus(index);
}

TileDepthTransform TemporalTileProvider::depthTransform() {
    if (!_currentTileProvider) {
        update();
    }

    return _currentTileProvider->depthTransform();
}

void TemporalTileProvider::update() {
    TileProvider* newCurr = nullptr;
    try {
        if (_useFixedTime && !_fixedTime.value().empty()) {
            if (_fixedTimeDirty) {
                const std::string fixedTime = _fixedTime.value();
                const double et = SpiceManager::ref().ephemerisTimeFromDate(fixedTime);
                newCurr = retrieveTileProvider(Time(et));
                _fixedTimeDirty = false;
            }
        }
        else {
            newCurr = tileProvider(global::timeManager->time());
        }
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC("TemporalTileProvider", e.message);
    }

    if (newCurr) {
        _currentTileProvider = newCurr;
    }
    if (_currentTileProvider) {
        _currentTileProvider->update();
    }
}

void TemporalTileProvider::reset() {
    for (std::pair<const double, DefaultTileProvider>& it : _tileProviderMap) {
        it.second.reset();
    }
}

int TemporalTileProvider::minLevel() {
    return 1;
}

int TemporalTileProvider::maxLevel() {
    if (!_currentTileProvider) {
        update();
    }
    return _currentTileProvider->maxLevel();
}

float TemporalTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

DefaultTileProvider TemporalTileProvider::createTileProvider(
                                                           std::string_view timekey) const
{
    ZoneScoped;

    std::string value;
    switch (_mode) {
        case Mode::Prototype: {
            static const std::vector<std::string> IgnoredTokens = {
                // From: http://www.gdal.org/frmt_wms.html
                "${x}", "${y}", "${z}", "${version}", "${format}", "${layer}"
            };

            value = _prototyped.prototype;
            while (true) {
                const size_t pos = value.find(TimePlaceholder);

                if (pos == std::string::npos) {
                    break;
                }

                const size_t numChars = std::string_view(TimePlaceholder).size();
                value = value.replace(pos, numChars, timekey);
            }

            value = FileSys.expandPathTokens(std::move(value), IgnoredTokens).string();
            break;
        }
        case Mode::Folder:
            value = std::string(timekey);
            break;
    }

    ghoul::Dictionary dict = _initDict;
    dict.setValue("FilePath", value);
    return DefaultTileProvider(dict);
}

DefaultTileProvider* TemporalTileProvider::retrieveTileProvider(const Time& t) {
    ZoneScoped;

    const double time = t.j2000Seconds();
    if (const auto it = _tileProviderMap.find(time);  it != _tileProviderMap.end()) {
        return &it->second;
    }

    const std::string_view timeStr = [this, time]() {
        switch (_mode) {
            case Mode::Prototype:
                return timeStringify(_prototyped.timeFormat, Time(time));
            case Mode::Folder: {
                // Yes this will have to be done twice since we do the check previously
                // but it is only happening when the images change, so I think that should
                // be fine
                auto it = std::lower_bound(
                    _folder.files.cbegin(),
                    _folder.files.cend(),
                    time,
                    [](const std::pair<double, std::string>& p, double sec) {
                        return p.first < sec;
                    }
                );
                return std::string_view(it->second);
            }
            default:
                throw ghoul::MissingCaseException();
        };
    }();

    DefaultTileProvider tileProvider = createTileProvider(timeStr);
    tileProvider.initialize();

    auto it = _tileProviderMap.insert({ time, std::move(tileProvider) });
    return &it.first->second;
}

template <>
TileProvider*
TemporalTileProvider::tileProvider<TemporalTileProvider::Mode::Folder, false>(
                                                                         const Time& time)
{
    // Find the most current image that matches the current time. We can't pass the `time`
    // variable into the retrieveTileProvider function as it would generate a new
    // non-existing TileProvider for every new frame
    auto it = std::lower_bound(
        _folder.files.begin(),
        _folder.files.end(),
        time.j2000Seconds(),
        [](const std::pair<double, std::string>& p, double t) {
            return p.first < t;
        }
    );

    if (it != _folder.files.begin()) {
        it -= 1;
    }

    const double t = it->first;
    return retrieveTileProvider(Time(t));
}

template <>
TileProvider*
TemporalTileProvider::tileProvider<TemporalTileProvider::Mode::Folder, true>(
                                                                         const Time& time)
{
    auto next = std::lower_bound(
        _folder.files.begin(),
        _folder.files.end(),
        time.j2000Seconds(),
        [](const std::pair<double, std::string>& p, double t) {
            return p.first < t;
        }
    );

    auto curr = next != _folder.files.begin() ? next - 1 : next;
    auto nextNext = next != _folder.files.end() ? next + 1 : curr;

    if (next == _folder.files.end()) {
        curr = _folder.files.end() - 1;
        next = curr;
        nextNext = curr;
    }

    auto prev = curr != _folder.files.begin() ? curr - 1 : curr;

    _interpolateTileProvider->t1 = retrieveTileProvider(Time(curr->first));
    _interpolateTileProvider->t2 = retrieveTileProvider(Time(next->first));
    _interpolateTileProvider->future = retrieveTileProvider(Time(nextNext->first));
    _interpolateTileProvider->before = retrieveTileProvider(Time(prev->first));

    const float factor = static_cast<float>(
        (time.j2000Seconds() - curr->first) / (next->first - curr->first)
    );

    _interpolateTileProvider->factor = std::clamp(factor, 0.f, 1.f);

    return _interpolateTileProvider.get();
}

template <>
TileProvider*
TemporalTileProvider::tileProvider<TemporalTileProvider::Mode::Prototype, false>(
                                                                         const Time& time)
{
    Time tCopy(time);
    if (_prototyped.timeQuantizer.quantize(tCopy, true)) {
        return retrieveTileProvider(tCopy);
    }
    else {
        return nullptr;
    }
}

template <>
TileProvider*
TemporalTileProvider::tileProvider<TemporalTileProvider::Mode::Prototype, true>(
                                                                         const Time& time)
{
    Time tCopy(time);
    if (!_prototyped.timeQuantizer.quantize(tCopy, true)) {
        return nullptr;
    }

    Time nextTile = tCopy;
    Time nextNextTile = tCopy;
    Time prevTile = tCopy;
    Time secondToLast = Time(_prototyped.endTimeJ2000);
    Time secondToFirst = Time(_prototyped.startTimeJ2000);

    _interpolateTileProvider->t1 = retrieveTileProvider(tCopy);

    // if the images are for each hour
    if (_prototyped.temporalResolution == "1h") {
        constexpr int Hour = 60 * 60;
        // the second tile to interpolate between
        nextTile.advanceTime(Hour);
        // the tile after the second tile
        nextNextTile.advanceTime(2 * Hour);
        // the tile before the first tile
        prevTile.advanceTime(-Hour + 1);
        // to make sure that an image outside the dataset is not searched for both
        // ends of the dataset are calculated
        secondToLast.advanceTime(-Hour);
        secondToFirst.advanceTime(Hour);
    }
    // if the images are for each month
    if (_prototyped.temporalResolution == "1M") {
        constexpr int Day = 24 * 60 * 60;

        // the second tile to interpolate between
        nextTile.advanceTime(32 * Day);
        // the tile after the second tile
        nextNextTile.advanceTime(64 * Day);
        // the tile before the first tile
        prevTile.advanceTime(-2 * Day);
        // to make sure that an image outside the dataset is not searched for both
        // ends of the dataset are calculated
        secondToLast.advanceTime(-2 * Day);
        secondToFirst.advanceTime(32 * Day);

        // since months vary in length the time is set to the first of each month
        auto setToFirstOfMonth = [](Time& t) {
            std::string timeString = std::string(t.ISO8601());
            timeString[8] = '0';
            timeString[9] = '1';
            t.setTime(timeString);
        };

        setToFirstOfMonth(nextTile);
        setToFirstOfMonth(nextNextTile);
        setToFirstOfMonth(prevTile);
        setToFirstOfMonth(secondToLast);
        setToFirstOfMonth(secondToFirst);
    }

    // the necessary tile providers are loaded if they exist within the timespan
    if (secondToLast.j2000Seconds() > time.j2000Seconds() &&
        secondToFirst.j2000Seconds() < time.j2000Seconds())
    {
        _interpolateTileProvider->t2 = retrieveTileProvider(nextTile);
        _interpolateTileProvider->future = retrieveTileProvider(nextNextTile);
        _interpolateTileProvider->before = retrieveTileProvider(prevTile);
    }
    else if (secondToLast.j2000Seconds() < time.j2000Seconds() &&
        _prototyped.endTimeJ2000 > time.j2000Seconds())
    {
        _interpolateTileProvider->t2 = retrieveTileProvider(nextTile);
        _interpolateTileProvider->future = retrieveTileProvider(tCopy);
        _interpolateTileProvider->before = retrieveTileProvider(prevTile);
    }
    else if (secondToFirst.j2000Seconds() > time.j2000Seconds() &&
        _prototyped.startTimeJ2000 < time.j2000Seconds())
    {
        _interpolateTileProvider->t2 = retrieveTileProvider(nextTile);
        _interpolateTileProvider->future = retrieveTileProvider(nextNextTile);
        _interpolateTileProvider->before = retrieveTileProvider(tCopy);
    }
    else {
        _interpolateTileProvider->t2 = retrieveTileProvider(tCopy);
        _interpolateTileProvider->future = retrieveTileProvider(tCopy);
        _interpolateTileProvider->before = retrieveTileProvider(tCopy);
    }
    _interpolateTileProvider->factor = static_cast<float>(
        (time.j2000Seconds() - tCopy.j2000Seconds()) /
        (nextTile.j2000Seconds() - tCopy.j2000Seconds())
    );

    if (_interpolateTileProvider->factor > 1.f) {
        _interpolateTileProvider->factor = 1.f;
    }
    return _interpolateTileProvider.get();
}

TileProvider* TemporalTileProvider::tileProvider(const Time& time) {
    if (_isInterpolating) {
        switch (_mode) {
            case Mode::Folder:
                return tileProvider<Mode::Folder, true>(time);
            case Mode::Prototype:
                return tileProvider<Mode::Prototype, true>(time);
            default:
                throw ghoul::MissingCaseException();
        }
    }
    else {
        switch (_mode) {
            case Mode::Folder:
                return tileProvider<Mode::Folder, false>(time);
            case Mode::Prototype:
                return tileProvider<Mode::Prototype, false>(time);
            default:
                throw ghoul::MissingCaseException();
        }
    }
}

TemporalTileProvider::InterpolateTileProvider::InterpolateTileProvider(
                                                                 const ghoul::Dictionary&)
{
    ZoneScoped;

    glGenFramebuffers(1, &fbo);
    glGenVertexArrays(1, &vaoQuad);
    glGenBuffers(1, &vboQuad);
    glBindVertexArray(vaoQuad);
    glBindBuffer(GL_ARRAY_BUFFER, vboQuad);
    // Quad for fullscreen with vertex (xy) and texture coordinates (uv)
    constexpr std::array<GLfloat, 24> VertexData = {
        // x    y    u    v
        -1.f, -1.f, 0.f, 0.f,
         1.f,  1.f, 1.f, 1.f,
        -1.f,  1.f, 0.f, 1.f,
        -1.f, -1.f, 0.f, 0.f,
         1.f, -1.f, 1.f, 0.f,
         1.f,  1.f, 1.f, 1.f
    };
    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
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
    ZoneScoped;
    TracyGpuZone("tile");

    // prev and next are the two tiles to interpolate between
    const Tile prev = t1->tile(tileIndex);
    const Tile next = t2->tile(tileIndex);
    // the tile before and the tile after the interpolation interval are loaded so the
    // interpolation goes smoother. It is on purpose that we are not actually storing the
    // return tile here, we just want to trigger the load already
    before->tile(tileIndex);
    future->tile(tileIndex);
    const cache::ProviderTileKey key = { tileIndex, uniqueIdentifier };

    if (!prev.texture || !next.texture) {
        return Tile{ nullptr, std::nullopt, Tile::Status::Unavailable };
    }

    // The data for initializing the texture
    const TileTextureInitData initData(
        prev.texture->dimensions().x,
        prev.texture->dimensions().y,
        prev.texture->dataType(),
        prev.texture->format(),
        TileTextureInitData::ShouldAllocateDataOnCPU::No
    );

    // Check if a tile exists for the given key in the tileCache
    // Initializing the tile that will contian the interpolated texture
    Tile ourTile;
    // The texture that will contain the interpolated image
    ghoul::opengl::Texture* writeTexture = nullptr;
    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
    if (tileCache->exist(key)) {
        ourTile = tileCache->get(key);
        writeTexture = ourTile.texture;
    }
    else {
        // Create a texture with the initialization data
        writeTexture = tileCache->texture(initData);
        ourTile = Tile{ writeTexture, std::nullopt, Tile::Status::OK };
        tileCache->put(key, initData.hashKey, ourTile);
    }

    // Saves current state
    GLint currentFBO = 0;
    std::array<GLint, 4> viewport;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &currentFBO);
    global::renderEngine->openglStateCache().viewport(viewport.data());
    // Bind render texture to FBO
    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, *writeTexture, 0);
    glDisable(GL_BLEND);
    const GLenum textureBuffers = GL_COLOR_ATTACHMENT0;
    glDrawBuffers(1, &textureBuffers);

    // Setup our own viewport settings
    const GLsizei w = static_cast<GLsizei>(writeTexture->width());
    const GLsizei h = static_cast<GLsizei>(writeTexture->height());
    glViewport(0, 0, w, h);
    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);
    GLint id = 0;
    glGetIntegerv(GL_CURRENT_PROGRAM, &id);
    // Activate shader and bind uniforms
    shaderProgram->activate();
    shaderProgram->setUniform("blendFactor", factor);

    // The texture that will give the color for the interpolated texture
    ghoul::opengl::TextureUnit colormapUnit;
    colormapUnit.activate();
    colormap->bind();
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
    return std::min(t1->tileStatus(index), t2->tileStatus(index));
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

int TemporalTileProvider::InterpolateTileProvider::minLevel() {
    return glm::max(t1->minLevel(), t2->minLevel());
}

int TemporalTileProvider::InterpolateTileProvider::maxLevel() {
    return glm::min(t1->maxLevel(), t2->maxLevel());
}

float TemporalTileProvider::InterpolateTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
