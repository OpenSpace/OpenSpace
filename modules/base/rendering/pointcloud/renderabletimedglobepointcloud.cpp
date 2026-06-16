#include <modules/base/rendering/pointcloud/renderabletimedglobepointcloud.h>

#include <openspace/engine/globals.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>

#include <algorithm>
#include <cctype>
#include <cmath>
#include <fstream>
#include <numeric>
#include <stdexcept>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableTimedGlobePointCloud";

    constexpr Property::PropertyInfo SourceFileInfo = {
        "SourceFile",
        "Source file",
        "The CSV file containing the globe point observations.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo IgnoreTimeFilteringInfo = {
        "IgnoreTimeFiltering",
        "Ignore time filtering",
        "If enabled, all points are rendered regardless of simulation time.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo LatitudeColumnInfo = {
        "LatitudeColumn",
        "Latitude column",
        "The CSV column containing point latitudes in degrees.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo LongitudeColumnInfo = {
        "LongitudeColumn",
        "Longitude column",
        "The CSV column containing point longitudes in degrees.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo StartTimeColumnInfo = {
        "StartTimeColumn",
        "Start time column",
        "The CSV column containing the start time for each point.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo EndTimeColumnInfo = {
        "EndTimeColumn",
        "End time column",
        "The CSV column containing the end time for each point.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo AltitudeColumnInfo = {
        "AltitudeColumn",
        "Altitude column",
        "The CSV column containing per-point altitude in meters.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FixedAltitudeInfo = {
        "FixedAltitude",
        "Fixed altitude",
        "The altitude in meters used for all points when no altitude column is set.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo TextureIndexColumnInfo = {
        "TextureIndexColumn",
        "Texture index column",
        "The CSV column containing a numeric texture index for each point.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo TextureKeyColumnInfo = {
        "TextureKeyColumn",
        "Texture key column",
        "The CSV column containing a texture key for each point.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo GlobeRadiusInfo = {
        "GlobeRadius",
        "Globe radius",
        "The globe radius in meters used to convert geodetic coordinates to local space.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo LoadedPointCountInfo = {
        "LoadedPointCount",
        "Loaded points",
        "The total number of timed globe points loaded from the CSV file.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo ActivePointCountInfo = {
        "ActivePointCount",
        "Active points",
        "The number of timed globe points active at the current simulation time.",
        Property::Visibility::User
    };

    std::vector<std::vector<std::string>> loadCsvFileRobust(const std::filesystem::path& path)
    {
        std::ifstream file(path, std::ios::binary);
        if (!file.is_open()) {
            throw ghoul::RuntimeError(std::format(
                "Could not open globe point CSV file '{}'",
                path
            ));
        }

        std::vector<std::vector<std::string>> rows;
        std::vector<std::string> row;
        std::string field;
        bool inQuotes = false;
        bool atFieldStart = true;

        char c = '\0';
        while (file.get(c)) {
            if (c == '\r') {
                continue;
            }

            if (atFieldStart && rows.empty() && row.empty() && field.empty()) {
                const unsigned char byte = static_cast<unsigned char>(c);
                if (byte == 0xEF) {
                    char b1 = '\0';
                    char b2 = '\0';
                    if (file.get(b1) && file.get(b2)) {
                        const bool hasBom =
                            static_cast<unsigned char>(b1) == 0xBB &&
                            static_cast<unsigned char>(b2) == 0xBF;
                        if (!hasBom) {
                            field += c;
                            field += b1;
                            field += b2;
                            atFieldStart = false;
                        }
                    }
                    continue;
                }
            }

            if (inQuotes) {
                if (c == '"') {
                    if (file.peek() == '"') {
                        file.get(c);
                        field += '"';
                    }
                    else {
                        inQuotes = false;
                    }
                }
                else {
                    field += c;
                }
                atFieldStart = false;
                continue;
            }

            if (c == '"' && atFieldStart) {
                inQuotes = true;
                continue;
            }
            if (c == ',') {
                row.push_back(field);
                field.clear();
                atFieldStart = true;
                continue;
            }
            if (c == '\n') {
                row.push_back(field);
                field.clear();
                atFieldStart = true;

                bool hasData = false;
                for (std::string& value : row) {
                    ghoul::trimWhitespace(value);
                    hasData = hasData || !value.empty();
                }
                if (hasData && !row.front().starts_with('#')) {
                    rows.push_back(std::move(row));
                }
                row.clear();
                continue;
            }

            field += c;
            atFieldStart = false;
        }

        if (inQuotes) {
            throw ghoul::RuntimeError(std::format(
                "Malformed CSV file '{}': unterminated quoted value",
                path
            ));
        }

        if (!field.empty() || !row.empty()) {
            row.push_back(field);
            bool hasData = false;
            for (std::string& value : row) {
                ghoul::trimWhitespace(value);
                hasData = hasData || !value.empty();
            }
            if (hasData && !row.front().starts_with('#')) {
                rows.push_back(std::move(row));
            }
        }

        return rows;
    }
} // namespace

namespace openspace {

openspace::Documentation RenderableTimedGlobePointCloud::Documentation() {
    return {
        "RenderableTimedGlobePointCloud",
        "base_renderable_timed_globe_point_cloud",
        "A textured point cloud on a globe with per-point time intervals and geodetic CSV "
            "input.",
        {
            { "SourceFile", new StringVerifier, Optional::No },
            { "IgnoreTimeFiltering", new BoolVerifier, Optional::Yes },
            { "LatitudeColumn", new StringVerifier, Optional::No },
            { "LongitudeColumn", new StringVerifier, Optional::No },
            { "StartTimeColumn", new StringVerifier, Optional::No },
            { "EndTimeColumn", new StringVerifier, Optional::Yes },
            { "AltitudeColumn", new StringVerifier, Optional::Yes },
            { "FixedAltitude", new DoubleVerifier, Optional::Yes },
            { "TextureIndexColumn", new StringVerifier, Optional::Yes },
            { "TextureKeyColumn", new StringVerifier, Optional::Yes },
            {
                "TextureFiles",
                new TableVerifier({ { "*", new StringVerifier, Optional::Yes, Private::No } }),
                Optional::Yes
            },
            {
                "TextureKeys",
                new TableVerifier({ { "*", new StringVerifier, Optional::Yes, Private::No } }),
                Optional::Yes
            },
            { "GlobeRadius", new DoubleVerifier, Optional::Yes }
        }
    };
}

ghoul::Dictionary RenderableTimedGlobePointCloud::sanitizedDictionary(
                                                  const ghoul::Dictionary& dictionary)
{
    ghoul::Dictionary result = dictionary;
    result.removeValue("SourceFile");
    result.removeValue("IgnoreTimeFiltering");
    result.removeValue("LatitudeColumn");
    result.removeValue("LongitudeColumn");
    result.removeValue("StartTimeColumn");
    result.removeValue("EndTimeColumn");
    result.removeValue("AltitudeColumn");
    result.removeValue("FixedAltitude");
    result.removeValue("TextureIndexColumn");
    result.removeValue("TextureKeyColumn");
    result.removeValue("TextureFiles");
    result.removeValue("TextureKeys");
    result.removeValue("GlobeRadius");
    return result;
}

RenderableTimedGlobePointCloud::RenderableTimedGlobePointCloud(
                                                    const ghoul::Dictionary& dictionary)
    : RenderablePointCloud(sanitizedDictionary(dictionary))
    , _sourceFile(SourceFileInfo)
    , _ignoreTimeFiltering(IgnoreTimeFilteringInfo, false)
    , _latitudeColumn(LatitudeColumnInfo)
    , _longitudeColumn(LongitudeColumnInfo)
    , _startTimeColumn(StartTimeColumnInfo)
    , _endTimeColumn(EndTimeColumnInfo)
    , _altitudeColumn(AltitudeColumnInfo)
    , _fixedAltitude(FixedAltitudeInfo, 0.f, -1000000.f, 1000000.f)
    , _textureIndexColumn(TextureIndexColumnInfo)
    , _textureKeyColumn(TextureKeyColumnInfo)
    , _globeRadius(GlobeRadiusInfo, 6378137.f, 1.f, 1e9f)
    , _loadedPointCount(LoadedPointCountInfo, 0)
    , _activePointCount(ActivePointCountInfo, 0)
{
    if (dictionary.hasValue<std::filesystem::path>("SourceFile")) {
        _dataPath = absPath(dictionary.value<std::filesystem::path>("SourceFile"));
    }
    else if (dictionary.hasValue<std::string>("SourceFile")) {
        _dataPath = absPath(dictionary.value<std::string>("SourceFile"));
    }
    else {
        throw ghoul::RuntimeError("Missing required key 'SourceFile'");
    }
    _sourceFile = _dataPath.string();

    _latitudeColumn = dictionary.value<std::string>("LatitudeColumn");
    _longitudeColumn = dictionary.value<std::string>("LongitudeColumn");
    _startTimeColumn = dictionary.value<std::string>("StartTimeColumn");

    if (dictionary.hasValue<std::string>("EndTimeColumn")) {
        _endTimeColumn = dictionary.value<std::string>("EndTimeColumn");
    }
    if (dictionary.hasValue<std::string>("AltitudeColumn")) {
        _altitudeColumn = dictionary.value<std::string>("AltitudeColumn");
    }
    if (dictionary.hasValue<double>("FixedAltitude")) {
        _fixedAltitude = static_cast<float>(dictionary.value<double>("FixedAltitude"));
    }
    else if (dictionary.hasValue<int>("FixedAltitude")) {
        _fixedAltitude = static_cast<float>(dictionary.value<int>("FixedAltitude"));
    }
    if (dictionary.hasValue<std::string>("TextureIndexColumn")) {
        _textureIndexColumn = dictionary.value<std::string>("TextureIndexColumn");
    }
    if (dictionary.hasValue<std::string>("TextureKeyColumn")) {
        _textureKeyColumn = dictionary.value<std::string>("TextureKeyColumn");
    }
    if (dictionary.hasValue<std::vector<std::string>>("TextureFiles")) {
        _textureFiles = dictionary.value<std::vector<std::string>>("TextureFiles");
    }
    if (dictionary.hasValue<std::vector<std::string>>("TextureKeys")) {
        _textureKeys = dictionary.value<std::vector<std::string>>("TextureKeys");
    }
    if (dictionary.hasValue<double>("GlobeRadius")) {
        _globeRadius = static_cast<float>(dictionary.value<double>("GlobeRadius"));
    }
    else if (dictionary.hasValue<int>("GlobeRadius")) {
        _globeRadius = static_cast<float>(dictionary.value<int>("GlobeRadius"));
    }
    if (dictionary.hasValue<bool>("IgnoreTimeFiltering")) {
        _ignoreTimeFiltering = dictionary.value<bool>("IgnoreTimeFiltering");
    }

    if (!_textureKeyColumn.value().empty() && !_textureIndexColumn.value().empty()) {
        throw ghoul::RuntimeError(
            "Specify either 'TextureIndexColumn' or 'TextureKeyColumn', not both"
        );
    }
    if (!_textureKeys.empty() && _textureKeys.size() != _textureFiles.size()) {
        throw ghoul::RuntimeError(
            "'TextureKeys' and 'TextureFiles' must contain the same number of entries"
        );
    }

    _loadedPointCount.setReadOnly(true);
    _activePointCount.setReadOnly(true);

    addProperty(_sourceFile);
    addProperty(_ignoreTimeFiltering);
    addProperty(_latitudeColumn);
    addProperty(_longitudeColumn);
    addProperty(_startTimeColumn);
    addProperty(_endTimeColumn);
    addProperty(_altitudeColumn);
    addProperty(_fixedAltitude);
    addProperty(_textureIndexColumn);
    addProperty(_textureKeyColumn);
    addProperty(_globeRadius);
    addProperty(_loadedPointCount);
    addProperty(_activePointCount);
}

void RenderableTimedGlobePointCloud::initialize() {
    RenderablePointCloud::initialize();

    loadPoints();

    _dataset.textureDataIndex = 0;
    _dataset.textures = textureMapping();
    _hasDataFile = true;
    _hasOrientationData = false;

    if (_shouldComputeScaleExponent && _maxPointRadius > 0.0) {
        const float exponent = static_cast<float>(std::log10(_maxPointRadius));
        _sizeSettings.scaleExponent = 0.9f * exponent;
    }

    double currentTime = 0.0;
    if (global::timeManager) {
        currentTime = global::timeManager->time().j2000Seconds();
    }

    if (_ignoreTimeFiltering) {
        _activePointIndices.resize(_points.size());
        std::iota(_activePointIndices.begin(), _activePointIndices.end(), size_t{ 0 });
    }
    else {
        rebuildActiveSet(currentTime);
    }

    rebuildDatasetEntries();
    _dataIsDirty = true;

    LINFOC(
        _loggerCat,
        std::format(
            "Loaded {} timed globe points from '{}' with {} active at {}",
            _points.size(),
            _dataPath.string(),
            _activePointIndices.size(),
            currentTime
        )
    );
}

void RenderableTimedGlobePointCloud::update(const UpdateData& data) {
    if (_ignoreTimeFiltering) {
        RenderablePointCloud::update(data);
        return;
    }

    const double currentTime = data.time.j2000Seconds();
    if (std::isnan(_lastUpdateTime) || currentTime < _lastUpdateTime) {
        rebuildActiveSet(currentTime);
        rebuildDatasetEntries();
        _dataIsDirty = true;
    }
    else {
        const bool needsStartUpdate =
            _nextStartIndex < _startTimes.size() &&
            _startTimes[_nextStartIndex].first <= currentTime;
        const bool needsEndUpdate =
            _nextEndIndex < _endTimes.size() &&
            _endTimes[_nextEndIndex].first < currentTime;
        if (needsStartUpdate || needsEndUpdate) {
            advanceActiveSet(currentTime);
            rebuildDatasetEntries();
            _dataIsDirty = true;
        }
    }

    _lastUpdateTime = currentTime;
    RenderablePointCloud::update(data);
}

void RenderableTimedGlobePointCloud::updateBufferData() {
    if (_dataset.entries.empty()) {
        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glNamedBufferData(_vbo, 0, nullptr, GL_STATIC_DRAW);
        glBindVertexArray(0);

        for (TextureArrayInfo& textureArray : _textureArrays) {
            textureArray.startOffset = 0;
            textureArray.nPoints = 0;
        }

        _dataIsDirty = false;
        return;
    }

    RenderablePointCloud::updateBufferData();
}

void RenderableTimedGlobePointCloud::validateSourceColumns(
                                                    const HeaderMap& headerMap) const
{
    requiredColumnIndex(headerMap, _latitudeColumn.value());
    requiredColumnIndex(headerMap, _longitudeColumn.value());
    requiredColumnIndex(headerMap, _startTimeColumn.value());
    if (!_endTimeColumn.value().empty()) {
        requiredColumnIndex(headerMap, _endTimeColumn.value());
    }
    if (!_altitudeColumn.value().empty()) {
        requiredColumnIndex(headerMap, _altitudeColumn.value());
    }
    if (!_textureIndexColumn.value().empty()) {
        requiredColumnIndex(headerMap, _textureIndexColumn.value());
    }
    if (!_textureKeyColumn.value().empty()) {
        requiredColumnIndex(headerMap, _textureKeyColumn.value());
    }
}

std::optional<RenderableTimedGlobePointCloud::TimedPoint>
RenderableTimedGlobePointCloud::pointFromRow(const std::vector<std::string>& row,
                                                     const HeaderMap& headerMap) const
{
    const size_t latitudeColumn = requiredColumnIndex(headerMap, _latitudeColumn.value());
    const size_t longitudeColumn = requiredColumnIndex(headerMap, _longitudeColumn.value());
    const size_t startTimeColumn = requiredColumnIndex(headerMap, _startTimeColumn.value());

    const std::optional<size_t> endTimeColumn = _endTimeColumn.value().empty() ?
        std::nullopt :
        optionalColumnIndex(headerMap, _endTimeColumn.value());
    const std::optional<size_t> altitudeColumn = _altitudeColumn.value().empty() ?
        std::nullopt :
        optionalColumnIndex(headerMap, _altitudeColumn.value());
    const std::optional<size_t> textureIndexColumn = _textureIndexColumn.value().empty() ?
        std::nullopt :
        optionalColumnIndex(headerMap, _textureIndexColumn.value());
    const std::optional<size_t> textureKeyColumn = _textureKeyColumn.value().empty() ?
        std::nullopt :
        optionalColumnIndex(headerMap, _textureKeyColumn.value());

    const double latitude = std::stod(row[latitudeColumn]);
    const double longitude = std::stod(row[longitudeColumn]);

    const std::string start = normalizeTimeString(row[startTimeColumn]);
    if (start.empty()) {
        return std::nullopt;
    }

    std::string end = start;
    if (endTimeColumn.has_value()) {
        const std::string value = normalizeTimeString(row[*endTimeColumn]);
        if (!value.empty()) {
            end = value;
        }
    }

    double altitude = _fixedAltitude.value();
    if (altitudeColumn.has_value() && !row[*altitudeColumn].empty()) {
        altitude = std::stod(row[*altitudeColumn]);
    }

    int textureIndex = 0;
    if (textureIndexColumn.has_value() && !row[*textureIndexColumn].empty()) {
        textureIndex = std::stoi(row[*textureIndexColumn]);
    }
    else if (textureKeyColumn.has_value()) {
        textureIndex = textureIndexForKey(row[*textureKeyColumn]);
    }

    if (!_textureFiles.empty()) {
        if (textureIndex < 0 || textureIndex >= static_cast<int>(_textureFiles.size())) {
            throw ghoul::RuntimeError(std::format(
                "Texture index {} is outside the configured texture range [0, {})",
                textureIndex,
                _textureFiles.size()
            ));
        }
    }

    TimedPoint point;
    point.position = glm::vec3(cartesianPositionFromDegrees(
        latitude,
        longitude,
        altitude,
        _globeRadius.value()
    ));
    point.textureIndex = static_cast<float>(textureIndex);
    point.startTime = Time::convertTime(start);
    point.endTime = Time::convertTime(end);

    if (point.endTime < point.startTime) {
        std::swap(point.startTime, point.endTime);
    }

    return point;
}

size_t RenderableTimedGlobePointCloud::requiredColumnIndex(
                                            const HeaderMap& headerMap, std::string_view name)
{
    const auto it = headerMap.find(std::string(name));
    if (it == headerMap.end()) {
        throw ghoul::RuntimeError(std::format("Missing required CSV column '{}'", name));
    }
    return it->second;
}

std::optional<size_t> RenderableTimedGlobePointCloud::optionalColumnIndex(
                                            const HeaderMap& headerMap, std::string_view name)
{
    const auto it = headerMap.find(std::string(name));
    if (it == headerMap.end()) {
        return std::nullopt;
    }
    return it->second;
}

std::string RenderableTimedGlobePointCloud::normalizeTimeString(std::string value) {
    ghoul::trimWhitespace(value);
    if (value.empty()) {
        return value;
    }

    const size_t plusOffset = value.find('+');
    if (plusOffset != std::string::npos) {
        value = value.substr(0, plusOffset);
    }

    const size_t zOffset = value.find('Z');
    if (zOffset != std::string::npos) {
        value = value.substr(0, zOffset);
    }

    if (value.size() > 19) {
        value = value.substr(0, 19);
    }
    if (value.size() >= 11 && value[10] == ' ') {
        value[10] = 'T';
    }

    return value;
}

std::string RenderableTimedGlobePointCloud::lowered(std::string value) {
    ghoul::trimWhitespace(value);
    std::transform(
        value.begin(),
        value.end(),
        value.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); }
    );
    return value;
}

glm::dvec3 RenderableTimedGlobePointCloud::cartesianPositionFromDegrees(
    double latitude,
    double longitude,
    double altitude,
    double globeRadius)
{
    const double latRad = glm::radians(latitude);
    const double lonRad = glm::radians(longitude);
    const double radius = globeRadius + altitude;
    return glm::dvec3(
        radius * std::cos(latRad) * std::cos(lonRad),
        radius * std::cos(latRad) * std::sin(lonRad),
        radius * std::sin(latRad)
    );
}

int RenderableTimedGlobePointCloud::textureIndexForKey(std::string_view key) const {
    if (_textureKeys.empty()) {
        throw ghoul::RuntimeError(
            "Texture keys were requested but no 'TextureKeys' were configured"
        );
    }

    const std::string normalizedKey = lowered(std::string(key));
    for (size_t i = 0; i < _textureKeys.size(); ++i) {
        if (lowered(_textureKeys[i]) == normalizedKey) {
            return static_cast<int>(i);
        }
    }

    throw ghoul::RuntimeError(std::format(
        "No texture mapping exists for key '{}'",
        key
    ));
}

void RenderableTimedGlobePointCloud::loadPoints() {
    if (!std::filesystem::is_regular_file(_dataPath)) {
        throw ghoul::RuntimeError(std::format(
            "Could not find globe point CSV file '{}'",
            _dataPath
        ));
    }

    const std::vector<std::vector<std::string>> rows = loadCsvFileRobust(_dataPath);
    if (rows.size() < 2) {
        throw ghoul::RuntimeError(std::format(
            "Globe point CSV file '{}' does not contain any observations",
            _dataPath
        ));
    }

    HeaderMap headerMap;
    for (size_t i = 0; i < rows.front().size(); ++i) {
        headerMap[rows.front()[i]] = i;
    }
    validateSourceColumns(headerMap);

    _points.clear();
    _startTimes.clear();
    _endTimes.clear();
    _activePointIndices.clear();
    _activePointSlots.clear();
    _maxPointRadius = 0.0;

    _points.reserve(rows.size() - 1);
    for (size_t i = 1; i < rows.size(); ++i) {
        try {
            const std::optional<TimedPoint> point = pointFromRow(rows[i], headerMap);
            if (!point.has_value()) {
                continue;
            }

            _maxPointRadius = std::max(
                _maxPointRadius,
                glm::length(glm::dvec3(point->position))
            );
            _points.push_back(*point);
        }
        catch (const std::exception& e) {
            LWARNINGC(
                _loggerCat,
                std::format(
                    "Skipping timed globe point row {} in '{}': {}",
                    i,
                    _dataPath.string(),
                    e.what()
                )
            );
        }
    }

    _startTimes.reserve(_points.size());
    _endTimes.reserve(_points.size());
    for (size_t i = 0; i < _points.size(); ++i) {
        _startTimes.emplace_back(_points[i].startTime, i);
        _endTimes.emplace_back(_points[i].endTime, i);
    }

    std::sort(_startTimes.begin(), _startTimes.end());
    std::sort(_endTimes.begin(), _endTimes.end());

    _activePointSlots.assign(_points.size(), -1);
    _loadedPointCount = static_cast<unsigned int>(_points.size());
}

void RenderableTimedGlobePointCloud::rebuildActiveSet(double currentTime) {
    std::fill(_activePointSlots.begin(), _activePointSlots.end(), -1);
    _activePointIndices.clear();

    _nextStartIndex = static_cast<size_t>(std::upper_bound(
        _startTimes.begin(),
        _startTimes.end(),
        std::pair<double, size_t>(currentTime, std::numeric_limits<size_t>::max())
    ) - _startTimes.begin());

    _nextEndIndex = static_cast<size_t>(std::lower_bound(
        _endTimes.begin(),
        _endTimes.end(),
        std::pair<double, size_t>(currentTime, 0)
    ) - _endTimes.begin());

    for (size_t i = 0; i < _nextStartIndex; ++i) {
        const size_t pointIndex = _startTimes[i].second;
        const TimedPoint& point = _points[pointIndex];
        if (currentTime >= point.startTime && currentTime <= point.endTime) {
            activatePoint(pointIndex);
        }
    }
}

void RenderableTimedGlobePointCloud::advanceActiveSet(double currentTime) {
    while (_nextStartIndex < _startTimes.size() && _startTimes[_nextStartIndex].first <= currentTime) {
        activatePoint(_startTimes[_nextStartIndex].second);
        ++_nextStartIndex;
    }

    while (_nextEndIndex < _endTimes.size() && _endTimes[_nextEndIndex].first < currentTime) {
        deactivatePoint(_endTimes[_nextEndIndex].second);
        ++_nextEndIndex;
    }
}

void RenderableTimedGlobePointCloud::activatePoint(size_t index) {
    if (_activePointSlots[index] >= 0) {
        return;
    }
    _activePointSlots[index] = static_cast<int>(_activePointIndices.size());
    _activePointIndices.push_back(index);
}

void RenderableTimedGlobePointCloud::deactivatePoint(size_t index) {
    const int activeSlot = _activePointSlots[index];
    if (activeSlot < 0) {
        return;
    }

    const size_t slot = static_cast<size_t>(activeSlot);
    const size_t lastIndex = _activePointIndices.back();
    _activePointIndices[slot] = lastIndex;
    _activePointSlots[lastIndex] = static_cast<int>(slot);
    _activePointIndices.pop_back();
    _activePointSlots[index] = -1;
}

void RenderableTimedGlobePointCloud::rebuildDatasetEntries() {
    _dataset.entries.clear();
    _dataset.entries.reserve(_activePointIndices.size());

    for (size_t index : _activePointIndices) {
        const TimedPoint& point = _points[index];
        dataloader::Dataset::Entry entry;
        entry.position = point.position;
        entry.data = { point.textureIndex };
        _dataset.entries.push_back(std::move(entry));
    }

    _nDataPoints = static_cast<unsigned int>(_dataset.entries.size());
    _activePointCount = _nDataPoints.value();
}

std::vector<dataloader::Dataset::Texture>
RenderableTimedGlobePointCloud::textureMapping() const
{
    std::vector<dataloader::Dataset::Texture> result;
    result.reserve(_textureFiles.size());
    for (size_t i = 0; i < _textureFiles.size(); ++i) {
        result.push_back({
            .index = static_cast<int>(i),
            .file = _textureFiles[i]
        });
    }
    return result;
}

} // namespace openspace
