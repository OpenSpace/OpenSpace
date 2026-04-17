#include <modules/solarbrowsing/util/dynamichelioviewerimagedownloader.h>

#include <modules/solarbrowsing/util/solarbrowsinghelper.h>
#include <openspace/json.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <scn/scan.h>
#include <algorithm>
#include <cmath>
#include <format>
#include <fstream>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "DynamicHelioviewerImageDownloader";

    std::chrono::steady_clock::time_point wallClockNow() {
        return std::chrono::steady_clock::now();
    }
} // namespace

namespace openspace {

DynamicHelioviewerImageDownloader::DynamicHelioviewerImageDownloader(
    std::filesystem::path outputFolder,
    std::string spacecraftName,
    int sourceId,
    std::string instrument,
    double cadenceSeconds,
    int prefetchBefore,
    int prefetchAfter,
    int maxConcurrentDownloads,
    double retryBackoffSeconds,
    int maxRetries)
    : _outputFolder(std::move(outputFolder))
    , _spacecraftName(std::move(spacecraftName))
    , _sourceId(sourceId)
    , _instrument(std::move(instrument))
    , _cadenceSeconds(cadenceSeconds)
    , _prefetchBefore(prefetchBefore)
    , _prefetchAfter(prefetchAfter)
    , _maxConcurrentDownloads(std::max(1, maxConcurrentDownloads))
    , _retryBackoffSeconds(std::max(1.0, retryBackoffSeconds))
    , _maxRetries(std::max(0, maxRetries))
{
    if (_sourceId < 0) {
        throw ghoul::RuntimeError("sourceId must be non-negative");
    }
    if (_cadenceSeconds <= 0.0) {
        throw ghoul::RuntimeError("cadenceSeconds must be greater than zero");
    }

    if (!std::filesystem::exists(_outputFolder)) {
        std::filesystem::create_directories(_outputFolder);
    }

    _status.activeInstrument = _instrument;
    _status.sourceId = _sourceId;
    _status.availabilityState = ImageryAvailabilityState::NoData;
    _status.statusText = "No data requested yet";

    scanForNewLocalFiles();
    clearDownloaded();
}

void DynamicHelioviewerImageDownloader::update(double currentTimeJ2000, double deltaTime) {
    const std::vector<RequestKey> requestedKeys = desiredKeys(currentTimeJ2000, deltaTime);
    _lastDesiredKeys = requestedKeys;
    _status.currentRequestKey = requestedKeys.empty() ? 0 : requestedKeys.front();
    _status.requestedSimTimeJ2000 = currentTimeJ2000;

    pollListingRequest(currentTimeJ2000);
    reprioritizeQueue(requestedKeys, currentTimeJ2000);
    startQueuedDownloads();
    pollDownloads(currentTimeJ2000);

    const bool allKnownEmpty =
        !requestedKeys.empty() &&
        std::all_of(
            requestedKeys.begin(),
            requestedKeys.end(),
            [this](RequestKey key) {
                auto it = _frameKnowledge.find(key);
                return it != _frameKnowledge.end() && it->second == FrameKnowledgeState::KnownEmpty;
            }
        );

    if (allKnownEmpty) {
        _status.availabilityState = ImageryAvailabilityState::NoData;
        updateStatusText();
        return;
    }

    const bool needListing = std::any_of(
        requestedKeys.begin(),
        requestedKeys.end(),
        [this](RequestKey key) {
            return !_knownLocalKeys.contains(key) &&
                !_availableFrames.contains(key) &&
                shouldRetry(key);
        }
    );

    if (needListing && !_activeListingRequest && shouldRequestNewWindow(currentTimeJ2000) &&
        wallClockNow() >= _nextListingAllowedTime)
    {
        startListingRequest(currentTimeJ2000);
    }

    if (_activeListingRequest) {
        _status.availabilityState = ImageryAvailabilityState::WaitingForListing;
    }
    else if (!_activeDownloads.empty()) {
        _status.availabilityState = ImageryAvailabilityState::WaitingForDownload;
    }
    else if (std::any_of(requestedKeys.begin(), requestedKeys.end(), [this](RequestKey key) {
        return _knownLocalKeys.contains(key);
    })) {
        _status.availabilityState = ImageryAvailabilityState::DecodePending;
    }
    updateStatusText();
}

const std::vector<std::filesystem::path>&
DynamicHelioviewerImageDownloader::downloadedFiles() const
{
    return _downloadedFiles;
}

void DynamicHelioviewerImageDownloader::clearDownloaded() {
    _downloadedFiles.clear();
}

void DynamicHelioviewerImageDownloader::deinitialize(bool saveDownloadsOnShutdown) {
    if (_activeListingRequest && _activeListingRequest->request) {
        _activeListingRequest->request->cancel();
        _activeListingRequest->request->wait();
        _activeListingRequest = nullptr;
    }

    for (auto& [key, download] : _activeDownloads) {
        if (download.request) {
            download.request->cancel();
            download.request->wait();
        }

        std::error_code ec;
        std::filesystem::remove(download.temporaryPath, ec);
        if (ec) {
            LWARNING(std::format(
                "Failed to remove temporary Helioviewer download '{}' [{}]",
                download.temporaryPath,
                ec.message()
            ));
        }
    }
    _activeDownloads.clear();

    if (saveDownloadsOnShutdown) {
        return;
    }

    for (const std::filesystem::path& file : _runtimeDownloadedFiles) {
        std::error_code ec;
        std::filesystem::remove(file, ec);
        if (ec) {
            LWARNING(std::format(
                "Failed to remove streamed Helioviewer file '{}' [{}]",
                file,
                ec.message()
            ));
        }
    }
}

const RuntimeImageryStatus& DynamicHelioviewerImageDownloader::status() const {
    return _status;
}

void DynamicHelioviewerImageDownloader::scanForNewLocalFiles() {
    ++_instrumentation.localScans;
    std::vector<std::filesystem::path> files = ghoul::filesystem::walkDirectory(
        _outputFolder,
        ghoul::filesystem::Recursive::No,
        ghoul::filesystem::Sorted::Yes,
        isJp2ImageFile
    );

    for (const std::filesystem::path& file : files) {
        const std::string filename = file.filename().string();
        if (_knownFiles.contains(filename)) {
            continue;
        }

        const std::string stem = file.stem().string();
        const std::optional<double> j2000 = j2000FromHelioviewerFilename(file);
        if (!j2000.has_value()) {
            LERROR(std::format("Ignoring file with unexpected name '{}'", file));
            continue;
        }

        const RequestKey key = requestKeyForTime(*j2000);

        _knownFiles.insert(filename);
        _knownLocalKeys.insert(key);
        _frameKnowledge[key] = FrameKnowledgeState::KnownLocal;
        _availableFrames[key] = {
            .unixTimestamp = 0.0,
            .j2000Timestamp = *j2000,
            .destinationPath = file
        };
        _downloadedFiles.push_back(file);
    }

    LDEBUG(std::format(
        "Helioviewer local scan #{} in '{}' found {} known files",
        _instrumentation.localScans,
        _outputFolder,
        _knownFiles.size()
    ));
}

void DynamicHelioviewerImageDownloader::pollListingRequest(double currentTimeJ2000) {
    if (!_activeListingRequest || !_activeListingRequest->request) {
        return;
    }

    HttpMemoryDownload& request = *_activeListingRequest->request;
    if (request.hasSucceeded()) {
        request.wait();

        const std::vector<char>& listingData = request.downloadedData();
        const std::string listingString(listingData.begin(), listingData.end());

        try {
            nlohmann::json json = nlohmann::json::parse(listingString.c_str());
            std::vector<double> frames;
            if (json.contains("frames") && !json["frames"].is_null()) {
                frames = json["frames"].get<std::vector<double>>();
            }

            if (json.contains("message") && json["message"].is_string()) {
                const std::string message = json["message"].get<std::string>();
                if (!message.empty()) {
                    LWARNING(message);
                }
            }

            if (frames.empty()) {
                for (RequestKey key : _lastDesiredKeys) {
                    if (!_knownLocalKeys.contains(key)) {
                        _frameKnowledge[key] = FrameKnowledgeState::KnownEmpty;
                    }
                }
                LINFO(std::format(
                    "No Helioviewer frames available for instrument '{}' in requested window",
                    _instrument
                ));
                _status.availabilityState = ImageryAvailabilityState::NoData;
            }

            for (double unixTimestamp : frames) {
                const double j2000 = j2000FromUnix(unixTimestamp);
                const RequestKey key = requestKeyForTime(j2000);
                const double keyStart = startTimeForKey(key);

                RemoteFrame candidate = {
                    .unixTimestamp = unixTimestamp,
                    .j2000Timestamp = j2000,
                    .destinationPath = expectedFilename(
                        isoStringFromUnixTimestamp(unixTimestamp, true)
                    )
                };

                auto it = _availableFrames.find(key);
                if (it == _availableFrames.end()) {
                    _frameKnowledge[key] = _knownLocalKeys.contains(key) ?
                        FrameKnowledgeState::KnownLocal : FrameKnowledgeState::KnownRemote;
                    _availableFrames[key] = std::move(candidate);
                }
                else {
                    const double oldDistance = std::abs(it->second.j2000Timestamp - keyStart);
                    const double newDistance = std::abs(candidate.j2000Timestamp - keyStart);
                    if (newDistance < oldDistance) {
                        it->second = std::move(candidate);
                    }
                    _frameKnowledge[key] = _knownLocalKeys.contains(key) ?
                        FrameKnowledgeState::KnownLocal : FrameKnowledgeState::KnownRemote;
                }
            }

            _lastRequestedCenterTime = _activeListingRequest->centerTimeJ2000;
            _nextListingAllowedTime = SteadyTimePoint::min();
        }
        catch (const std::exception& e) {
            _status.availabilityState = ImageryAvailabilityState::Error;
            LERROR(std::format(
                "Failed to parse Helioviewer frame response [{}]",
                e.what()
            ));
            _nextListingAllowedTime = wallClockNow() + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                std::chrono::duration<double>(_retryBackoffSeconds)
            );
        }

        _activeListingRequest = nullptr;
    }
    else if (request.hasFailed()) {
        request.wait();
        _status.availabilityState = ImageryAvailabilityState::Error;
        LERROR(std::format(
            "Failed to fetch Helioviewer frame list from '{}'",
            request.url()
        ));
        _nextListingAllowedTime = wallClockNow() + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
            std::chrono::duration<double>(_retryBackoffSeconds)
        );
        _activeListingRequest = nullptr;
    }
}

void DynamicHelioviewerImageDownloader::startListingRequest(double currentTimeJ2000) {
    const double begin = currentTimeJ2000 - _requestWindowBefore;
    const double end = currentTimeJ2000 + _requestWindowAfter;

    auto request = std::make_unique<HttpMemoryDownload>(frameListingUrl(begin, end));
    request->start();

    ++_instrumentation.listingsStarted;
    _status.availabilityState = ImageryAvailabilityState::WaitingForListing;
    LINFO(std::format("Requesting Helioviewer frames: {}", request->url()));
    LDEBUG(std::format(
        "Helioviewer listing count for '{}' is now {}",
        _instrument,
        _instrumentation.listingsStarted
    ));
    _activeListingRequest = std::make_unique<ListingRequest>(ListingRequest{
        .request = std::move(request),
        .centerTimeJ2000 = currentTimeJ2000
    });
}

void DynamicHelioviewerImageDownloader::reprioritizeQueue(
    const std::vector<RequestKey>& desired,
    double currentTimeJ2000)
{
    std::deque<RequestKey> nextQueue;
    std::unordered_set<RequestKey> nextQueuedKeys;

    for (RequestKey key : desired) {
        if (_knownLocalKeys.contains(key) || !_availableFrames.contains(key) ||
            _activeDownloads.contains(key) || !shouldRetry(key))
        {
            continue;
        }

        _frameKnowledge[key] = FrameKnowledgeState::KnownRemote;
        nextQueue.push_back(key);
        nextQueuedKeys.insert(key);
    }

    _queuedDownloadKeys = std::move(nextQueue);
    _queuedKeys = std::move(nextQueuedKeys);
}

void DynamicHelioviewerImageDownloader::startQueuedDownloads() {
    while (_activeDownloads.size() < static_cast<size_t>(_maxConcurrentDownloads) &&
           !_queuedDownloadKeys.empty())
    {
        const RequestKey key = _queuedDownloadKeys.front();
        _queuedDownloadKeys.pop_front();
        _queuedKeys.erase(key);

        auto frameIt = _availableFrames.find(key);
        if (frameIt == _availableFrames.end() || _knownLocalKeys.contains(key) ||
            _activeDownloads.contains(key))
        {
            continue;
        }

        const RemoteFrame& frame = frameIt->second;
        const std::filesystem::path finalPath = frame.destinationPath;
        const std::filesystem::path tempPath = finalPath.string() + ".part";

        std::error_code ec;
        std::filesystem::remove(tempPath, ec);

        auto request = std::make_unique<HttpFileDownload>(imageUrl(frame.unixTimestamp), tempPath);
        request->start();

        ++_instrumentation.downloadsStarted;
        _status.availabilityState = ImageryAvailabilityState::WaitingForDownload;
        LINFO(std::format("Downloading Helioviewer image: {}", request->url()));
        LDEBUG(std::format(
            "Helioviewer download count for '{}' is now {}",
            _instrument,
            _instrumentation.downloadsStarted
        ));
        _activeDownloads.emplace(key, ActiveDownload{
            .request = std::move(request),
            .key = key,
            .unixTimestamp = frame.unixTimestamp,
            .temporaryPath = tempPath,
            .finalPath = finalPath
        });
    }
}

void DynamicHelioviewerImageDownloader::pollDownloads(double currentTimeJ2000) {
    for (auto it = _activeDownloads.begin(); it != _activeDownloads.end();) {
        ActiveDownload& download = it->second;
        HttpFileDownload& request = *download.request;

        if (request.hasSucceeded()) {
            request.wait();

            std::error_code ec;
            const bool exists = std::filesystem::exists(download.temporaryPath, ec);
            const uintmax_t size = exists ? std::filesystem::file_size(download.temporaryPath, ec) : 0;
            if (ec || !exists || size == 0) {
                LERROR(std::format(
                    "Downloaded Helioviewer file '{}' was empty or invalid",
                    download.temporaryPath
                ));
                std::filesystem::remove(download.temporaryPath, ec);
                _failedUntil[download.key] = wallClockNow() + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                    std::chrono::duration<double>(_retryBackoffSeconds)
                );
                ++_retryCounts[download.key];
                _frameKnowledge[download.key] = FrameKnowledgeState::RetryBackoff;
                it = _activeDownloads.erase(it);
                continue;
            }

            std::filesystem::create_directories(download.finalPath.parent_path());
            std::filesystem::remove(download.finalPath, ec);
            std::filesystem::rename(download.temporaryPath, download.finalPath, ec);
            if (ec) {
                LERROR(std::format(
                    "Failed to finalize Helioviewer file '{}' [{}]",
                    download.finalPath,
                    ec.message()
                ));
                _failedUntil[download.key] = wallClockNow() + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                    std::chrono::duration<double>(_retryBackoffSeconds)
                );
                ++_retryCounts[download.key];
                _frameKnowledge[download.key] = FrameKnowledgeState::RetryBackoff;
                it = _activeDownloads.erase(it);
                continue;
            }

            const std::string filename = download.finalPath.filename().string();
            _knownFiles.insert(filename);
            _knownLocalKeys.insert(download.key);
            _frameKnowledge[download.key] = FrameKnowledgeState::KnownLocal;
            _downloadedFiles.push_back(download.finalPath);
            _runtimeDownloadedFiles.push_back(download.finalPath);
            _retryCounts.erase(download.key);
            _failedUntil.erase(download.key);
            ++_instrumentation.downloadsCompleted;
            LDEBUG(std::format(
                "Helioviewer completed download count for '{}' is now {}",
                _instrument,
                _instrumentation.downloadsCompleted
            ));
            scanForNewLocalFiles();
            it = _activeDownloads.erase(it);
            continue;
        }

        if (request.hasFailed()) {
            request.wait();
            LERROR(std::format(
                "Failed to download Helioviewer image '{}'",
                request.url()
            ));

            std::error_code ec;
            std::filesystem::remove(download.temporaryPath, ec);

            const int retries = ++_retryCounts[download.key];
            const double multiplier = std::pow(2.0, static_cast<double>(std::max(0, retries - 1)));
            _failedUntil[download.key] = wallClockNow() + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                std::chrono::duration<double>(_retryBackoffSeconds * multiplier)
            );
            _frameKnowledge[download.key] = FrameKnowledgeState::RetryBackoff;
            it = _activeDownloads.erase(it);
            continue;
        }

        ++it;
    }
}

void DynamicHelioviewerImageDownloader::enqueueKey(RequestKey key) {
    if (_queuedKeys.contains(key) || _activeDownloads.contains(key) ||
        _knownLocalKeys.contains(key))
    {
        return;
    }

    _queuedDownloadKeys.push_back(key);
    _queuedKeys.insert(key);
}

std::vector<DynamicHelioviewerImageDownloader::RequestKey>
DynamicHelioviewerImageDownloader::desiredKeys(double currentTimeJ2000,
                                               double deltaTime) const
{
    const RequestKey currentKey = requestKeyForTime(currentTimeJ2000);
    std::vector<RequestKey> result;
    result.reserve(static_cast<size_t>(_prefetchBefore + _prefetchAfter + 1));

    const bool paused = deltaTime == 0.0;
    const bool forward = deltaTime >= 0.0;
    const int before = paused ? (_prefetchBefore + _prefetchAfter) / 2
                              : (forward ? _prefetchBefore : _prefetchAfter);
    const int after = paused ? (_prefetchBefore + _prefetchAfter + 1) / 2
                             : (forward ? _prefetchAfter : _prefetchBefore);

    for (int i = before; i > 0; --i) {
        result.push_back(currentKey - i);
    }
    result.push_back(currentKey);
    for (int i = 1; i <= after; ++i) {
        result.push_back(currentKey + i);
    }

    return result;
}

bool DynamicHelioviewerImageDownloader::shouldRequestNewWindow(double currentTimeJ2000) const {
    if (_lastRequestedCenterTime < 0.0) {
        return true;
    }

    return std::abs(currentTimeJ2000 - _lastRequestedCenterTime) >= _minRequestShift;
}

bool DynamicHelioviewerImageDownloader::shouldRetry(RequestKey key) const
{
    auto failedIt = _failedUntil.find(key);
    if (failedIt != _failedUntil.end() && failedIt->second > wallClockNow()) {
        return false;
    }

    auto retryIt = _retryCounts.find(key);
    return retryIt == _retryCounts.end() || retryIt->second <= _maxRetries;
}

void DynamicHelioviewerImageDownloader::updateStatusText() {
    _status.queuedDownloads = _queuedDownloadKeys.size();
    _status.activeDownloads = _activeDownloads.size();
    _status.displayedFrameTimeJ2000 = std::nullopt;
    _status.statusText = std::format(
        "{}: instrument='{}', sourceId={}, queued={}, active={}",
        toString(_status.availabilityState),
        _status.activeInstrument,
        _status.sourceId,
        _status.queuedDownloads,
        _status.activeDownloads
    );
}

DynamicHelioviewerImageDownloader::RequestKey
DynamicHelioviewerImageDownloader::requestKeyForTime(double j2000) const
{
    return static_cast<RequestKey>(std::floor(j2000 / _cadenceSeconds));
}

double DynamicHelioviewerImageDownloader::startTimeForKey(RequestKey key) const {
    return static_cast<double>(key) * _cadenceSeconds;
}

double DynamicHelioviewerImageDownloader::j2000FromUnix(double unixTimestamp) const {
    return Time::convertTime(isoStringFromUnixTimestamp(unixTimestamp, true));
}

std::filesystem::path DynamicHelioviewerImageDownloader::expectedFilename(
    const std::string& isoTime) const
{
    auto r = scn::scan<int, int, int, int, int, int, int>(
        isoTime,
        "{}-{}-{}T{}:{}:{}.{}"
    );
    ghoul_assert(r, "Invalid ISO timestamp");

    auto& [year, month, day, hour, minute, second, millisecond] = r->values();

    return _outputFolder / std::format(
        "{:04}_{:02}_{:02}__{:02}_{:02}_{:02}_{:03}__{}_{}.jp2",
        year,
        month,
        day,
        hour,
        minute,
        second,
        millisecond,
        _spacecraftName,
        _instrument
    );
}

std::string DynamicHelioviewerImageDownloader::j2000ToIso(double j2000) const {
    return std::string(Time(j2000).ISO8601());
}

std::string DynamicHelioviewerImageDownloader::frameListingUrl(double beginJ2000,
                                                               double endJ2000) const
{
    return std::format(
        "https://api.helioviewer.org/v2/getJPX/?startTime={}Z&endTime={}Z"
        "&sourceId={}&verbose=true&cadence=true&cadence={}",
        j2000ToIso(beginJ2000),
        j2000ToIso(endJ2000),
        _sourceId,
        _cadenceSeconds
    );
}

std::string DynamicHelioviewerImageDownloader::imageUrl(double unixTimestamp) const {
    return std::format(
        "https://api.helioviewer.org/v2/getJP2Image/?date={}Z&sourceId={}",
        isoStringFromUnixTimestamp(unixTimestamp, true),
        _sourceId
    );
}

} // namespace openspace
