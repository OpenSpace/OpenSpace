#include <modules/base/rendering/pointcloud/renderableaurorasaurusobservationcloud.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/util/geodetic.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <fstream>
#include <numeric>
#include <limits>
#include <string>
#include <string_view>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "AurorasaurusObservationCloud";
    constexpr double EarthRadius = 6378137.0;

    constexpr Property::PropertyInfo ObservationFileInfo = {
        "ObservationFile",
        "Observation file",
        "The CSV file containing the Aurorasaurus observations.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo IgnoreTimeFilteringInfo = {
        "IgnoreTimeFiltering",
        "Ignore time filtering",
        "If enabled, all observations are rendered regardless of simulation time."
            " Useful for debugging visibility and sizing.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo ObservedAltitudeInfo = {
        "ObservedAltitude",
        "Observed altitude",
        "The altitude in meters used for aurora observations.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo NotObservedAltitudeInfo = {
        "NotObservedAltitude",
        "Not observed altitude",
        "The altitude in meters used for reports where aurora was not seen.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo LoadedObservationCountInfo = {
        "LoadedObservationCount",
        "Loaded observations",
        "The number of Aurorasaurus observations loaded from the CSV file.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo ActiveObservationCountInfo = {
        "ActiveObservationCount",
        "Active observations",
        "The number of Aurorasaurus observations currently visible for the active time.",
        Property::Visibility::User
    };

    std::string lowered(std::string value) {
        ghoul::trimWhitespace(value);
        std::transform(
            value.begin(),
            value.end(),
            value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); }
        );
        return value;
    }

    std::vector<std::vector<std::string>> loadCsvFileRobust(
        const std::filesystem::path& path)
    {
        std::ifstream file(path, std::ios::binary);
        if (!file.is_open()) {
            throw ghoul::RuntimeError(std::format(
                "Could not open Aurorasaurus CSV file '{}'",
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

openspace::Documentation RenderableAurorasaurusObservationCloud::Documentation() {
    return {
        "RenderableAurorasaurusObservationCloud",
        "base_renderable_aurorasaurus_observation_cloud",
        "A scalable, time-filtered Aurorasaurus icon cloud rendered as a single point-"
            "cloud-style scene graph node.",
        {
            { "ObservationFile", new StringVerifier, Optional::No },
            { "IgnoreTimeFiltering", new BoolVerifier, Optional::Yes },
            { "ObservedAltitude", new DoubleVerifier, Optional::Yes },
            { "NotObservedAltitude", new DoubleVerifier, Optional::Yes }
        }
    };
}

ghoul::Dictionary RenderableAurorasaurusObservationCloud::sanitizedDictionary(
                                                   const ghoul::Dictionary& dictionary)
{
    ghoul::Dictionary result = dictionary;
    result.removeValue("ObservationFile");
    result.removeValue("IgnoreTimeFiltering");
    result.removeValue("ObservedAltitude");
    result.removeValue("NotObservedAltitude");
    return result;
}

RenderableAurorasaurusObservationCloud::RenderableAurorasaurusObservationCloud(
                                                  const ghoul::Dictionary& dictionary)
    : RenderablePointCloud(sanitizedDictionary(dictionary))
    , _observationFile(ObservationFileInfo)
    , _ignoreTimeFiltering(IgnoreTimeFilteringInfo, false)
    , _observedAltitude(ObservedAltitudeInfo, 10000.f, 0.f, 1000000.f)
    , _notObservedAltitude(NotObservedAltitudeInfo, 9500.f, 0.f, 1000000.f)
    , _loadedObservationCount(LoadedObservationCountInfo, 0)
    , _activeObservationCount(ActiveObservationCountInfo, 0)
{
    if (dictionary.hasValue<std::filesystem::path>("ObservationFile")) {
        _dataPath = absPath(dictionary.value<std::filesystem::path>("ObservationFile"));
    }
    else if (dictionary.hasValue<std::string>("ObservationFile")) {
        _dataPath = absPath(dictionary.value<std::string>("ObservationFile"));
    }
    else {
        throw ghoul::RuntimeError("Missing required key 'ObservationFile'");
    }
    _observationFile = _dataPath.string();

    if (dictionary.hasValue<double>("ObservedAltitude")) {
        _observedAltitude = static_cast<float>(dictionary.value<double>("ObservedAltitude"));
    }
    else if (dictionary.hasValue<int>("ObservedAltitude")) {
        _observedAltitude = static_cast<float>(dictionary.value<int>("ObservedAltitude"));
    }

    if (dictionary.hasValue<double>("NotObservedAltitude")) {
        _notObservedAltitude = static_cast<float>(
            dictionary.value<double>("NotObservedAltitude")
        );
    }
    else if (dictionary.hasValue<int>("NotObservedAltitude")) {
        _notObservedAltitude = static_cast<float>(
            dictionary.value<int>("NotObservedAltitude")
        );
    }

    if (dictionary.hasValue<bool>("IgnoreTimeFiltering")) {
        _ignoreTimeFiltering = dictionary.value<bool>("IgnoreTimeFiltering");
    }

    _loadedObservationCount.setReadOnly(true);
    _activeObservationCount.setReadOnly(true);

    addProperty(_observationFile);
    addProperty(_ignoreTimeFiltering);
    addProperty(_observedAltitude);
    addProperty(_notObservedAltitude);
    addProperty(_loadedObservationCount);
    addProperty(_activeObservationCount);
}

void RenderableAurorasaurusObservationCloud::initialize() {
    RenderablePointCloud::initialize();

    loadObservations();

    _dataset.textureDataIndex = 0;
    _dataset.textures = textureMapping();
    _hasDataFile = true;
    _hasOrientationData = false;

    if (_shouldComputeScaleExponent && _maxObservationRadius > 0.0) {
        const float exponent = static_cast<float>(std::log10(_maxObservationRadius));
        _sizeSettings.scaleExponent = 0.9f * exponent;
    }

    double currentTime = 0.0;
    if (global::timeManager) {
        currentTime = global::timeManager->time().j2000Seconds();
    }
    if (_ignoreTimeFiltering) {
        _activeObservationIndices.resize(_observations.size());
        std::iota(
            _activeObservationIndices.begin(),
            _activeObservationIndices.end(),
            size_t{ 0 }
        );
    }
    else {
        rebuildActiveSet(currentTime);
    }
    rebuildDatasetEntries();
    _dataIsDirty = true;

    LINFO(std::format(
        "Loaded {} Aurorasaurus observations from '{}' with {} active at {}",
        _observations.size(),
        _dataPath.string(),
        _activeObservationIndices.size(),
        currentTime
    ));
}

void RenderableAurorasaurusObservationCloud::update(const UpdateData& data) {
    if (_ignoreTimeFiltering) {
        RenderablePointCloud::update(data);
        return;
    }

    const double currentTime = data.time.j2000Seconds();
    if (std::isnan(_lastUpdateTime)) {
        rebuildActiveSet(currentTime);
        rebuildDatasetEntries();
        _dataIsDirty = true;
    }
    else if (currentTime < _lastUpdateTime) {
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

void RenderableAurorasaurusObservationCloud::updateBufferData() {
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

void RenderableAurorasaurusObservationCloud::loadObservations() {
    if (!std::filesystem::is_regular_file(_dataPath)) {
        throw ghoul::RuntimeError(std::format(
            "Could not find Aurorasaurus CSV file '{}'",
            _dataPath
        ));
    }

    std::vector<std::vector<std::string>> rows = loadCsvFileRobust(_dataPath);
    if (rows.size() < 2) {
        throw ghoul::RuntimeError(std::format(
            "Aurorasaurus CSV file '{}' does not contain any observations",
            _dataPath
        ));
    }

    const std::vector<std::string>& header = rows.front();
    auto findColumn = [&header](std::string_view name) {
        auto it = std::find(header.begin(), header.end(), name);
        return it == header.end() ? static_cast<size_t>(-1) :
            static_cast<size_t>(std::distance(header.begin(), it));
    };

    const size_t latColumn = findColumn("st_y");
    const size_t lonColumn = findColumn("st_x");
    const size_t startColumn = findColumn("time_start");
    const size_t endColumn = findColumn("time_end");
    const size_t seenColumn = findColumn("see_aurora");
    const size_t colorsColumn = findColumn("colors");

    if (
        latColumn == static_cast<size_t>(-1) ||
        lonColumn == static_cast<size_t>(-1) ||
        startColumn == static_cast<size_t>(-1) ||
        endColumn == static_cast<size_t>(-1) ||
        seenColumn == static_cast<size_t>(-1) ||
        colorsColumn == static_cast<size_t>(-1)
    ) {
        throw ghoul::RuntimeError(std::format(
            "Aurorasaurus CSV file '{}' is missing one or more required columns",
            _dataPath
        ));
    }

    _observations.clear();
    _startTimes.clear();
    _endTimes.clear();
    _activeObservationIndices.clear();
    _activeObservationSlots.clear();
    _maxObservationRadius = 0.0;

    _observations.reserve(rows.size() - 1);

    for (size_t i = 1; i < rows.size(); ++i) {
        const std::vector<std::string>& row = rows[i];
        const size_t maxColumn = std::max({
            latColumn,
            lonColumn,
            startColumn,
            endColumn,
            seenColumn,
            colorsColumn
        });
        if (row.size() <= maxColumn) {
            continue;
        }

        try {
            const double latitude = std::stod(row[latColumn]);
            const double longitude = std::stod(row[lonColumn]);

            const std::string start = normalizeTimeString(row[startColumn]);
            std::string end = normalizeTimeString(row[endColumn]);
            if (start.empty()) {
                continue;
            }
            if (end.empty()) {
                end = start;
            }

            const bool sawAurora = lowered(row[seenColumn]) == "true";
            const double altitude = sawAurora ? _observedAltitude.value() :
                _notObservedAltitude.value();

            const double latRad = glm::radians(latitude);
            const double lonRad = glm::radians(longitude);
            const double radius = EarthRadius + altitude;
            const glm::dvec3 positionModelSpace = glm::dvec3(
                radius * std::cos(latRad) * std::cos(lonRad),
                radius * std::cos(latRad) * std::sin(lonRad),
                radius * std::sin(latRad)
            );
            const int textureIndex = textureIndexForObservation(sawAurora, row[colorsColumn]);

            Observation obs;
            obs.position = glm::vec3(positionModelSpace);
            obs.textureIndex = static_cast<float>(textureIndex);
            obs.startTime = Time::convertTime(start);
            obs.endTime = Time::convertTime(end);

            if (obs.endTime < obs.startTime) {
                std::swap(obs.startTime, obs.endTime);
            }

            _maxObservationRadius = std::max(
                _maxObservationRadius,
                glm::length(glm::dvec3(obs.position))
            );
            _observations.push_back(obs);
        }
        catch (const std::exception& e) {
            LWARNING(std::format(
                "Skipping Aurorasaurus row {} in '{}': {}",
                i,
                _dataPath,
                e.what()
            ));
        }
    }

    _startTimes.reserve(_observations.size());
    _endTimes.reserve(_observations.size());
    for (size_t i = 0; i < _observations.size(); ++i) {
        _startTimes.emplace_back(_observations[i].startTime, i);
        _endTimes.emplace_back(_observations[i].endTime, i);
    }

    std::sort(_startTimes.begin(), _startTimes.end());
    std::sort(_endTimes.begin(), _endTimes.end());

    _activeObservationSlots.assign(_observations.size(), -1);
    _loadedObservationCount = static_cast<unsigned int>(_observations.size());
}

void RenderableAurorasaurusObservationCloud::rebuildActiveSet(double currentTime) {
    std::fill(_activeObservationSlots.begin(), _activeObservationSlots.end(), -1);
    _activeObservationIndices.clear();

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
        const size_t observationIndex = _startTimes[i].second;
        const Observation& obs = _observations[observationIndex];
        if (currentTime >= obs.startTime && currentTime <= obs.endTime) {
            activateObservation(observationIndex);
        }
    }
}

void RenderableAurorasaurusObservationCloud::advanceActiveSet(double currentTime) {
    while (
        _nextStartIndex < _startTimes.size() &&
        _startTimes[_nextStartIndex].first <= currentTime
    ) {
        activateObservation(_startTimes[_nextStartIndex].second);
        ++_nextStartIndex;
    }

    while (_nextEndIndex < _endTimes.size() && _endTimes[_nextEndIndex].first < currentTime) {
        deactivateObservation(_endTimes[_nextEndIndex].second);
        ++_nextEndIndex;
    }
}

void RenderableAurorasaurusObservationCloud::activateObservation(size_t index) {
    if (_activeObservationSlots[index] >= 0) {
        return;
    }

    _activeObservationSlots[index] = static_cast<int>(_activeObservationIndices.size());
    _activeObservationIndices.push_back(index);
}

void RenderableAurorasaurusObservationCloud::deactivateObservation(size_t index) {
    const int activeSlot = _activeObservationSlots[index];
    if (activeSlot < 0) {
        return;
    }

    const size_t slot = static_cast<size_t>(activeSlot);
    const size_t lastIndex = _activeObservationIndices.back();
    _activeObservationIndices[slot] = lastIndex;
    _activeObservationSlots[lastIndex] = static_cast<int>(slot);
    _activeObservationIndices.pop_back();
    _activeObservationSlots[index] = -1;
}

void RenderableAurorasaurusObservationCloud::rebuildDatasetEntries() {
    _dataset.entries.clear();
    _dataset.entries.reserve(_activeObservationIndices.size());

    for (size_t index : _activeObservationIndices) {
        const Observation& obs = _observations[index];
        dataloader::Dataset::Entry entry;
        entry.position = obs.position;
        entry.data = { obs.textureIndex };
        _dataset.entries.push_back(std::move(entry));
    }

    _nDataPoints = static_cast<unsigned int>(_dataset.entries.size());
    _activeObservationCount = _nDataPoints.value();
}

std::string RenderableAurorasaurusObservationCloud::normalizeTimeString(std::string value) {
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

int RenderableAurorasaurusObservationCloud::textureIndexForObservation(
                                               bool seeAurora, std::string_view colors)
{
    constexpr int Red = 1;
    constexpr int White = 2;
    constexpr int Green = 4;
    constexpr int Pink = 8;

    if (!seeAurora) {
        return 0;
    }

    int mask = 0;
    std::vector<std::string> tokens = ghoul::tokenizeString(std::string(colors), ',');
    for (std::string& token : tokens) {
        token = lowered(token);
        if (token.empty()) {
            continue;
        }

        if (token.starts_with("red")) {
            mask |= Red;
        }
        else if (token.starts_with("whit") || token.starts_with("white")) {
            mask |= White;
        }
        else if (token.starts_with("gree") || token.starts_with("green")) {
            mask |= Green;
        }
        else if (token.starts_with("pink")) {
            mask |= Pink;
        }
    }

    switch (mask) {
        case 0: return 1;
        case Red: return 2;
        case White: return 3;
        case Green: return 1;
        case Pink: return 4;
        case Red | White: return 5;
        case Red | Green: return 6;
        case Red | Pink: return 7;
        case White | Green: return 8;
        case White | Pink: return 9;
        case Green | Pink: return 10;
        case Red | White | Green: return 11;
        case Red | White | Pink: return 12;
        case Red | Green | Pink: return 13;
        case White | Green | Pink: return 14;
        case Red | White | Green | Pink: return 15;
        default: return 1;
    }
}

std::vector<dataloader::Dataset::Texture>
RenderableAurorasaurusObservationCloud::textureMapping()
{
    return {
        { .index = 0, .file = "grayIcon.png" },
        { .index = 1, .file = "green2.png" },
        { .index = 2, .file = "red2.png" },
        { .index = 3, .file = "white2.png" },
        { .index = 4, .file = "pink2.png" },
        { .index = 5, .file = "redWhite2.png" },
        { .index = 6, .file = "greenRed2.png" },
        { .index = 7, .file = "redPink2.png" },
        { .index = 8, .file = "greenWhite2.png" },
        { .index = 9, .file = "whitePink2.png" },
        { .index = 10, .file = "greenPink2.png" },
        { .index = 11, .file = "greenRedWhite2.png" },
        { .index = 12, .file = "redWhitePink2.png" },
        { .index = 13, .file = "greenRedPink2.png" },
        { .index = 14, .file = "greenWhitePink2.png" },
        { .index = 15, .file = "greenRedWhitePink2.png" }
    };
}

} // namespace openspace
