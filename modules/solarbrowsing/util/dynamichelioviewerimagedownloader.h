/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#pragma once

#include <modules/solarbrowsing/util/dynamicimagerytypes.h>

#include <openspace/util/httprequest.h>

#include <chrono>
#include <cstdint>
#include <deque>
#include <filesystem>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace openspace {

class DynamicHelioviewerImageDownloader {
public:
    using RequestKey = int64_t;
    using SteadyTimePoint = std::chrono::time_point<std::chrono::steady_clock>;

    DynamicHelioviewerImageDownloader(
        std::filesystem::path outputFolder,
        std::string spacecraftName,
        int sourceId,
        std::string instrument,
        double cadenceSeconds,
        int prefetchBefore = 1,
        int prefetchAfter = 3,
        int maxConcurrentDownloads = 2,
        double retryBackoffSeconds = 30.0,
        int maxRetries = 2
    );

    void update(double currentTimeJ2000, double deltaTime);
    const std::vector<std::filesystem::path>& downloadedFiles() const;
    void clearDownloaded();
    void deinitialize(bool saveDownloadsOnShutdown);
    const RuntimeImageryStatus& status() const;

private:
    struct RemoteFrame {
        double unixTimestamp = 0.0;
        double j2000Timestamp = 0.0;
        std::filesystem::path destinationPath;
    };

    struct ListingRequest {
        std::unique_ptr<HttpMemoryDownload> request;
        double centerTimeJ2000 = 0.0;
    };

    struct ActiveDownload {
        std::unique_ptr<HttpFileDownload> request;
        RequestKey key = 0;
        double unixTimestamp = 0.0;
        std::filesystem::path temporaryPath;
        std::filesystem::path finalPath;
    };

    struct InstrumentationCounters {
        size_t localScans = 0;
        size_t listingsStarted = 0;
        size_t downloadsStarted = 0;
        size_t downloadsCompleted = 0;
    };

    void scanForNewLocalFiles();
    void pollListingRequest(double currentTimeJ2000);
    void startListingRequest(double currentTimeJ2000);
    void reprioritizeQueue(const std::vector<RequestKey>& desiredKeys, double currentTimeJ2000);
    void startQueuedDownloads();
    void pollDownloads(double currentTimeJ2000);
    void enqueueKey(RequestKey key);
    std::vector<RequestKey> desiredKeys(double currentTimeJ2000, double deltaTime) const;
    bool shouldRequestNewWindow(double currentTimeJ2000) const;
    bool shouldRetry(RequestKey key) const;
    void updateStatusText();

    RequestKey requestKeyForTime(double j2000) const;
    double startTimeForKey(RequestKey key) const;
    double j2000FromUnix(double unixTimestamp) const;
    std::filesystem::path expectedFilename(const std::string& isoTime) const;
    std::string j2000ToIso(double j2000) const;
    std::string frameListingUrl(double beginJ2000, double endJ2000) const;
    std::string imageUrl(double unixTimestamp) const;

    std::filesystem::path _outputFolder;
    std::string _spacecraftName;
    int _sourceId = -1;
    std::string _instrument;
    double _cadenceSeconds = 3600.0;
    int _prefetchBefore = 1;
    int _prefetchAfter = 3;
    int _maxConcurrentDownloads = 2;
    double _retryBackoffSeconds = 30.0;
    int _maxRetries = 2;

    double _requestWindowBefore = 12.0 * 3600.0;
    double _requestWindowAfter = 12.0 * 3600.0;
    double _minRequestShift = 1.0 * 3600.0;
    double _lastRequestedCenterTime = -1.0;
    SteadyTimePoint _nextListingAllowedTime = SteadyTimePoint::min();

    std::unique_ptr<ListingRequest> _activeListingRequest;
    std::deque<RequestKey> _queuedDownloadKeys;
    std::unordered_map<RequestKey, RemoteFrame> _availableFrames;
    std::unordered_map<RequestKey, ActiveDownload> _activeDownloads;
    std::unordered_map<RequestKey, SteadyTimePoint> _failedUntil;
    std::unordered_map<RequestKey, int> _retryCounts;
    std::unordered_map<RequestKey, FrameKnowledgeState> _frameKnowledge;
    std::unordered_set<RequestKey> _knownLocalKeys;
    std::unordered_set<RequestKey> _queuedKeys;
    std::unordered_set<std::string> _knownFiles;
    std::vector<std::filesystem::path> _downloadedFiles;
    std::vector<std::filesystem::path> _runtimeDownloadedFiles;
    InstrumentationCounters _instrumentation;
    RuntimeImageryStatus _status;
    std::vector<RequestKey> _lastDesiredKeys;
};

} // namespace openspace
