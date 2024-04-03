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

#include <modules/sync/syncs/urlsynchronization.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <numeric>
#include <mutex>
#include <optional>
#include <unordered_map>
#include <variant>

namespace {
    constexpr std::string_view _loggerCat = "UrlSynchronization";
    constexpr std::string_view OssyncVersionNumber = "1.0";
    constexpr std::string_view SynchronizationToken = "Synchronized";

    struct [[codegen::Dictionary(UrlSynchronization)]] Parameters {
        // The URL or urls from where the files are downloaded. If multiple URLs are
        // provided, all files will be downloaded to the same directory and the filename
        // parameter must not be specified simultaneously
        std::variant<std::string, std::vector<std::string>> url;

        // This identifier will be part of the used folder structure and, can be used to
        // manually find the downloaded folder in the synchronization folder
        std::string identifier [[codegen::identifier()]];

        // Deprecated, use SecondsUntilResync instead
        std::optional<bool> forceOverride [[codegen::key("Override")]];

        // If this value is set to 'true' (the default), the hash of the URL is appended
        // to the directory name to produce a unique directory under all circumstances. If
        // this is not desired, the URLSynchronization use the bare directory name alone
        // if this value is 'false'. If this value is 'false', the identifier has to be
        // specified
        std::optional<bool> useHash;

        // Optional to provide filename to override the one which is otherwise
        // automatically created from the url. If this value is specified, the url
        // parameter only only contain exactly one URL
        std::optional<std::string> filename;

        // This variable determines the validity period of a file(s) in seconds before it
        // needs to be re-downloaded. The default value keeps the file permanently cached,
        // while a value of 0 forces the file to be downloaded on every startup.
        std::optional<double> secondsUntilResync [[codegen::greaterequal(0.0)]];
    };
#include "urlsynchronization_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation UrlSynchronization::Documentation() {
    return codegen::doc<Parameters>("sync_synchronization_url");
}

UrlSynchronization::UrlSynchronization(const ghoul::Dictionary& dictionary,
                                       std::filesystem::path synchronizationRoot)
    : ResourceSynchronization(std::move(synchronizationRoot))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (std::holds_alternative<std::string>(p.url)) {
        _urls.push_back(std::get<std::string>(p.url));

    }
    else if (std::holds_alternative<std::vector<std::string>>(p.url)) {
        _urls = std::get<std::vector<std::string>>(p.url);
    }
    else {
        throw ghoul::MissingCaseException();
    }

    if (p.filename.has_value() && _urls.size() > 1) {
        throw ghoul::RuntimeError(std::format(
            "UrlSynchronization ({}) requested overwrite filename but specified {} URLs "
            "to download, which is not legal",
            p.identifier, _urls.size()
        ));
    }
    _filename = p.filename.value_or(_filename);
    _forceOverride = p.forceOverride.value_or(_forceOverride);

    // (anden88 2023-11-03) TODO: When we decide to remove override variable this should
    // be cleaned up.
    // Mimic behavior of time to live if override is specified (true => force download,
    // false keeps the file permanently (default behavior)).
    _secondsUntilResync = _forceOverride ? 0 : MaxDateAsJ2000;
    // Disregard override variable if user specified a specific time to live.
    _secondsUntilResync = p.secondsUntilResync.value_or(_secondsUntilResync);

    const bool useHash = p.useHash.value_or(true);

    _identifier = p.identifier;

    if (useHash) {
        // We just merge all of the URLs together to generate a hash that works for this
        std::vector<std::string> urls = _urls;
        std::sort(urls.begin(), urls.end());

        size_t hash = std::hash<std::string>{}(
            std::accumulate(urls.begin(), urls.end(), std::string())
        );
        _identifier += std::format("({})", hash);
    }

    if (p.forceOverride.has_value()) {
        LWARNING(std::format(
            "{}: The variable ForceOverride has been deprecated. "
            "Optionally, use SecondsUntilResync instead to specify file validity date.",
            p.identifier
        ));
    }
}

UrlSynchronization::~UrlSynchronization() {
    if (_syncThread.joinable()) {
        cancel();
        _syncThread.join();
    }
}

std::filesystem::path UrlSynchronization::directory() const {
    return _synchronizationRoot / "url" / _identifier / "files";
}

void UrlSynchronization::start() {
    if (isSyncing()) {
        return;
    }
    _state = State::Syncing;

    if (isEachFileValid()) {
        _state = State::Resolved;
        return;
    }

    if (isRejected()) {
        return;
    }

    _syncThread = std::thread([this]() {
        const bool success = trySyncUrls();

        if (success) {
            createSyncFile();
            _state = State::Resolved;
        }
        else {
            _state = State::Rejected;
        }
    });
}

void UrlSynchronization::cancel() {
    _shouldCancel = true;
    _state = State::Unsynced;
}

std::string UrlSynchronization::generateUid() {
    return _identifier;
}

bool UrlSynchronization::isEachFileValid() {
    std::filesystem::path path = directory();
    path.replace_extension("ossync");
    // Check if file exists at all
    if (!std::filesystem::is_regular_file(path)) {
        return false;
    }

    std::ifstream file(path);
    std::string line;

    file >> line;
    // Update ossync files that does not have a version number to new format
    if (line == SynchronizationToken) {
        if (_secondsUntilResync == 0) {
            return false; // Force download new file
        }

        // We must close file early because createSyncFile writes to it
        file.close();
        createSyncFile();
        // File is valid until some date
        return true;
    }
    // Otherwise first line is the version number.
    std::string ossyncVersion = line;

    // Format of 1.0 ossync:
    // Version number: e.g., 1.0
    // Date that specifies how long the files are valid for in ISO8601 format
    // Valid to: yyyy-mm-ddThr:mn:sc.xxx

    if (ossyncVersion == "1.0") {
        ghoul::getline(file >> std::ws, line);
        std::string& fileIsValidToDate = line;
        const double fileValidAsJ2000 = Time::convertTime(fileIsValidToDate);

        const std::string todaysDate = Time::currentWallTime();
        const double todaysDateAsJ2000 = Time::convertTime(todaysDate);

        // Issue warning if file is kept but user changed setting to download on startup.
        if ((fileValidAsJ2000 > todaysDateAsJ2000) && _secondsUntilResync == 0) {
            LWARNING(std::format(
                "{}: File is valid to {} but asset specifies SecondsUntilResync = {} "
                "Did you mean to re-download the file? If so, remove file from sync "
                "folder to resync",
                _identifier, fileIsValidToDate, _secondsUntilResync
            ));
        }

        // Returns true if date in ossync file is 'larger' than todays date,
        // i.e. no sync needed.
        return fileValidAsJ2000 > todaysDateAsJ2000;
    }
    else if (ossyncVersion.empty()) {
        // For some reason we ended up with an empty synchronization file
        return false;
    }
    else {
        LERROR(std::format(
            "{}: Unknown ossync version number read. Got {} while {} and below are valid",
            _identifier, ossyncVersion, OssyncVersionNumber
        ));
        _state = State::Rejected;
    }

    return false;
}

void UrlSynchronization::createSyncFile(bool) const {
    std::filesystem::path dir = directory();
    std::filesystem::create_directories(dir);

    dir.replace_extension("ossync");
    std::ofstream syncFile(dir, std::ofstream::out);

    const std::string currentTimeAsISO8601 = Time::currentWallTime();
    const double currentTimeAsJ2000 = Time::convertTime(currentTimeAsISO8601);

    // With the format YYYY-MM... any year thats larger than 4 digits throws an error
    // Limit the future date to year 9999
    const double futureTimeAsJ2000 = std::min(
        currentTimeAsJ2000 + _secondsUntilResync,
        MaxDateAsJ2000
    );

    std::string fileIsValidTo = SpiceManager::ref().dateFromEphemerisTime(
        futureTimeAsJ2000,
        "YYYY-MM-DDTHR:MN:SC.###"
    );

    const std::string msg = std::format("{}\n{}\n", OssyncVersionNumber, fileIsValidTo);
    syncFile << msg;
}

bool UrlSynchronization::trySyncUrls() {
    struct SizeData {
        int64_t downloadedBytes = 0;
        std::optional<int64_t> totalBytes;
    };

    std::unordered_map<std::string, SizeData> sizeData;
    std::mutex fileSizeMutex;
    std::atomic_bool startedAllDownloads = false;
    std::vector<std::unique_ptr<HttpFileDownload>> downloads;

    for (const std::string& url : _urls) {
        if (_filename.empty() || _urls.size() > 1) {
            std::filesystem::path fn = std::filesystem::path(url).filename();
            if (fn.empty() && url.back() == '/') {
                // If the user provided a path that ends in / the `filename` will
                // result in an empty path with causes the downloading to fail
                fn = std::filesystem::path(url).parent_path().filename();
            }
            std::string name = fn.string();

            // We can not create filenames with question marks
            name.erase(std::remove(name.begin(), name.end(), '?'), name.end());
            _filename = name;
        }
        std::filesystem::path destination = directory() / (_filename + ".tmp");

        if (sizeData.find(url) != sizeData.end()) {
            LWARNING(std::format("{}: Duplicate entry for '{}'", _identifier, url));
            continue;
        }

        auto download = std::make_unique<HttpFileDownload>(
            url,
            std::move(destination),
            HttpFileDownload::Overwrite::Yes
        );
        HttpFileDownload* dl = download.get();

        downloads.push_back(std::move(download));

        sizeData[url] = SizeData();

        dl->onProgress(
            [this, url, &sizeData, &fileSizeMutex](int64_t downloadedBytes,
                                                std::optional<int64_t> totalBytes)
            {
                if (!totalBytes.has_value()) {
                    return !_shouldCancel;
                }

                const std::lock_guard guard(fileSizeMutex);
                sizeData[url] = { downloadedBytes, totalBytes };

                _nTotalBytesKnown = true;
                _nTotalBytes = 0;
                _nSynchronizedBytes = 0;
                for (const std::pair<const std::string, SizeData>& sd : sizeData) {
                    _nTotalBytesKnown = _nTotalBytesKnown &&
                                        sd.second.totalBytes.has_value();
                    _nTotalBytes += sd.second.totalBytes.value_or(0);
                    _nSynchronizedBytes += sd.second.downloadedBytes;
                }

                return !_shouldCancel;
            });

        dl->start();
    }

    startedAllDownloads = true;

    bool failed = false;
    for (const std::unique_ptr<HttpFileDownload>& d : downloads) {
        d->wait();
        if (!d->hasSucceeded()) {
            failed = true;
            LERROR(std::format("Error downloading file from URL: {}", d->url()));
            continue;
        }

        // If we are forcing the override, we download to a temporary file first, so
        // when we are done here, we need to rename the file to the original name

        std::filesystem::path tempName = d->destination();
        std::filesystem::path originalName = tempName;
        // Remove the .tmp extension
        originalName.replace_extension("");

        if (std::filesystem::is_regular_file(originalName)) {
            std::filesystem::remove(originalName);
        }

        std::error_code ec;
        std::filesystem::rename(tempName, originalName, ec);
        if (ec) {
            LERRORC(
                "URLSynchronization",
                std::format("Error renaming file '{}' to '{}'", tempName, originalName)
            );

            failed = true;
        }
    }

    return !failed;
}

} // namespace openspace
