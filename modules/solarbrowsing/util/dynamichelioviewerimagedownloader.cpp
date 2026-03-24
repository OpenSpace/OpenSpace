#include <modules/solarbrowsing/util/dynamichelioviewerimagedownloader.h>

#include <openspace/json.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <scn/scan.h>

#include <algorithm>
#include <cmath>
#include <ctime>
#include <format>

namespace {
    constexpr std::string_view _loggerCat = "DynamicHelioviewerImageDownloader";

    constexpr double J2000UnixOffset = 946684800.0;
} // namespace

namespace openspace {

    DynamicHelioviewerImageDownloader::DynamicHelioviewerImageDownloader(
        std::filesystem::path outputFolder,
        std::string spacecraftName,
        int sourceId,
        std::string instrument,
        double cadenceSeconds,
        int numberOfFilesToQueue)
        : _outputFolder(std::move(outputFolder))
        , _spacecraftName(std::move(spacecraftName))
        , _sourceId(sourceId)
        , _instrument(std::move(instrument))
        , _cadenceSeconds(cadenceSeconds)
        , _numberOfFilesToQueue(numberOfFilesToQueue)
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

        // Seed known files from disk so we do not re-report them as newly downloaded
        std::vector<std::filesystem::path> existingFiles = ghoul::filesystem::walkDirectory(
            _outputFolder,
            ghoul::filesystem::Recursive::No,
            ghoul::filesystem::Sorted::Yes,
            [](const std::filesystem::path& p) {
                const std::string ext = p.extension().string();
                return (ext == ".jp2") || (ext == ".j2k");
            }
        );

        for (const std::filesystem::path& p : existingFiles) {
            _knownFiles.insert(p.filename().string());
            _allDownloadedFiles.push_back(p);
        }
    }

    void DynamicHelioviewerImageDownloader::update(double currentTimeJ2000, double) {
        if (!shouldRequestNewWindow(currentTimeJ2000)) {
            return;
        }

        const double begin = currentTimeJ2000 - _requestWindowBefore;
        const double end = currentTimeJ2000 + _requestWindowAfter;

        std::vector<double> frames = requestFrames(begin, end);
        if (frames.empty()) {
            _lastRequestedCenterTime = currentTimeJ2000;
            return;
        }

        // Download missing frames
        for (double unixTimestamp : frames) {
            const std::string iso = unixToIso(unixTimestamp);
            const std::filesystem::path outFile = expectedFilename(iso);

            if (std::filesystem::exists(outFile)) {
                _knownFiles.insert(outFile.filename().string());
                continue;
            }

            try {
                std::filesystem::path downloaded = downloadFrame(unixTimestamp);
                if (!downloaded.empty()) {
                    const std::string filename = downloaded.filename().string();
                    if (!_knownFiles.contains(filename)) {
                        _knownFiles.insert(filename);
                        _downloadedFiles.push_back(downloaded);
                        _allDownloadedFiles.push_back(downloaded);
                    }
                }
            }
            catch (const std::exception& e) {
                LERROR(std::format(
                    "Failed to download Helioviewer frame for '{}' [{}]",
                    iso, e.what()
                ));
            }
        }

        _lastRequestedCenterTime = currentTimeJ2000;

        // Optional simple cap on tracked file list size.
        // This does not remove files from disk during runtime yet; it only prevents
        // unlimited bookkeeping growth in this helper.
        if (_numberOfFilesToQueue > 0 &&
            static_cast<int>(_allDownloadedFiles.size()) > _numberOfFilesToQueue * 10)
        {
            const size_t keep = static_cast<size_t>(_numberOfFilesToQueue * 10);
            const size_t toRemove = _allDownloadedFiles.size() - keep;
            _allDownloadedFiles.erase(
                _allDownloadedFiles.begin(),
                _allDownloadedFiles.begin() + static_cast<ptrdiff_t>(toRemove)
            );
        }
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
        if (saveDownloadsOnShutdown) {
            return;
        }

        for (const std::filesystem::path& p : _allDownloadedFiles) {
            std::error_code ec;
            std::filesystem::remove(p, ec);
            if (ec) {
                LWARNING(std::format(
                    "Failed to remove downloaded file '{}' [{}]",
                    p, ec.message()
                ));
            }
        }
    }

    std::vector<double> DynamicHelioviewerImageDownloader::requestFrames(
        double beginJ2000,
        double endJ2000) const
    {
        const std::string startIso = j2000ToIso(beginJ2000);
        const std::string endIso = j2000ToIso(endJ2000);

        const std::string jpxRequest = std::format(
            "http://api.helioviewer.org/v2/getJPX/?startTime={}&endTime={}"
            "&sourceId={}&verbose=true&cadence=true&cadence={}",
            startIso,
            endIso,
            _sourceId,
            _cadenceSeconds
        );

        LINFO(std::format("Requesting Helioviewer frames: {}", jpxRequest));

        HttpMemoryDownload fileListing(jpxRequest);
        fileListing.start();
        fileListing.wait();

        if (!fileListing.hasSucceeded()) {
            throw ghoul::RuntimeError("Request to Helioviewer API failed");
        }

        const std::vector<char>& listingData = fileListing.downloadedData();
        const std::string listingString(listingData.begin(), listingData.end());

        try {
            nlohmann::json json = nlohmann::json::parse(listingString.c_str());

            std::vector<double> frames = json["frames"].get<std::vector<double>>();

            std::string* message = json["message"].get_ptr<std::string*>();
            if (message && !message->empty()) {
                LWARNING(*message);
            }

            return frames;
        }
        catch (...) {
            throw ghoul::RuntimeError(std::format(
                "Failed to parse Helioviewer frame response: {}",
                listingString
            ));
        }
    }

    std::filesystem::path DynamicHelioviewerImageDownloader::downloadFrame(
        double unixTimestamp)
    {
        const std::string iso = unixToIso(unixTimestamp);

        const std::string imageUrl = std::format(
            "https://api.helioviewer.org/v2/getJP2Image/?date={}Z&sourceId={}",
            iso,
            _sourceId
        );

        const std::filesystem::path outFile = expectedFilename(iso);

        if (std::filesystem::exists(outFile)) {
            return outFile;
        }

        LINFO(std::format("Downloading Helioviewer image: {}", imageUrl));

        HttpFileDownload imageDownload(imageUrl, outFile);
        imageDownload.start();
        imageDownload.wait();

        if (!imageDownload.hasSucceeded()) {
            throw ghoul::RuntimeError(std::format(
                "Request to image '{}' failed", imageUrl
            ));
        }

        return outFile;
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

    bool DynamicHelioviewerImageDownloader::shouldRequestNewWindow(
        double currentTimeJ2000) const
    {
        if (_lastRequestedCenterTime < 0.0) {
            return true;
        }

        return std::abs(currentTimeJ2000 - _lastRequestedCenterTime) >= _minRequestShift;
    }

    std::string DynamicHelioviewerImageDownloader::j2000ToIso(double j2000) const {
        const Time t(j2000);
        return std::string(t.ISO8601());
    }

    std::string DynamicHelioviewerImageDownloader::unixToIso(double unixTimestamp) const {
        std::time_t timestamp = static_cast<std::time_t>(unixTimestamp);
        std::tm* utcTime = std::gmtime(&timestamp);

        return std::format(
            "{:04d}-{:02d}-{:02d}T{:02d}:{:02d}:{:02d}.000",
            utcTime->tm_year + 1900,
            utcTime->tm_mon + 1,
            utcTime->tm_mday,
            utcTime->tm_hour,
            utcTime->tm_min,
            utcTime->tm_sec
        );
    }

} // namespace openspace
