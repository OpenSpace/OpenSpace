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

#include <modules/sync/syncs/httpsynchronization.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/httprequest.h>
#include <ghoul/logging/logmanager.h>
#include <unordered_map>

namespace {
    constexpr std::string_view _loggerCat = "HttpSynchronization";

    constexpr int ApplicationVersion = 1;

    struct [[codegen::Dictionary(HttpSynchronization)]] Parameters {
        // The unique identifier for this resource that is used to request a set of files
        // from the synchronization servers
        std::string identifier;

        // The version of this resource that should be requested
        int version;
    };
#include "httpsynchronization_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation HttpSynchronization::Documentation() {
    return codegen::doc<Parameters>("http_synchronization");
}

HttpSynchronization::HttpSynchronization(const ghoul::Dictionary& dict,
                                         std::filesystem::path synchronizationRoot,
                                      std::vector<std::string> synchronizationRepositories
)
    : ResourceSynchronization(std::move(synchronizationRoot))
    , _syncRepositories(std::move(synchronizationRepositories))
{
    const Parameters p = codegen::bake<Parameters>(dict);

    _identifier = p.identifier;
    _version = p.version;
}

HttpSynchronization::~HttpSynchronization() {
    if (_syncThread.joinable()) {
        cancel();
        _syncThread.join();
    }
}

std::filesystem::path HttpSynchronization::directory() const {
    return _synchronizationRoot / "http" / _identifier / std::to_string(_version);
}

void HttpSynchronization::start() {
    if (isSyncing()) {
        return;
    }
    _state = State::Syncing;

    if (hasSyncFile()) {
        _state = State::Resolved;
        return;
    }

    std::string query = fmt::format(
        "?identifier={}&file_version={}&application_version={}",
        _identifier, _version, ApplicationVersion
    );

    _syncThread = std::thread(
        [this](const std::string& q) {
            for (const std::string& url : _syncRepositories) {
                const bool success = trySyncFromUrl(url + q);
                if (success) {
                    createSyncFile();
                    _state = State::Resolved;
                    return;
                }
            }
            if (!_shouldCancel) {
                _state = State::Rejected;
            }
        },
        query
    );
}

void HttpSynchronization::cancel() {
    _shouldCancel = true;
    _state = State::Unsynced;
}

std::string HttpSynchronization::generateUid() {
    return fmt::format("{}/{}", _identifier, _version);
}

bool HttpSynchronization::trySyncFromUrl(std::string listUrl) {
    HttpMemoryDownload fileListDownload(std::move(listUrl));
    fileListDownload.onProgress([&c = _shouldCancel](int64_t, std::optional<int64_t>) {
        return !c;
    });
    fileListDownload.start();
    const bool success = fileListDownload.wait();

    const std::vector<char>& buffer = fileListDownload.downloadedData();
    if (!success) {
        LERRORC("HttpSynchronization", std::string(buffer.begin(), buffer.end()));
        return false;
    }

    _nSynchronizedBytes = 0;
    _nTotalBytes = 0;
    _nTotalBytesKnown = false;

    std::istringstream fileList(std::string(buffer.begin(), buffer.end()));

    struct SizeData {
        int64_t downloadedBytes = 0;
        std::optional<int64_t> totalBytes;
    };

    std::unordered_map<std::string, SizeData> sizeData;
    std::mutex mutex;

    std::atomic_bool startedAllDownloads = false;

    // Yes, it should be possible to store this in a std::vector<HttpFileDownload> but
    // C++ really doesn't like that even though all of the move constructors, move
    // assignments and everything is automatically constructed
    std::vector<std::unique_ptr<HttpFileDownload>> downloads;

    std::string line;
    while (fileList >> line) {
        if (line.empty() || line[0] == '#') {
            // Skip all empty lines and commented out lines
            continue;
        }

        std::string filename = std::filesystem::path(line).filename().string();
        std::filesystem::path destination = directory() / (filename + ".tmp");

        if (sizeData.find(line) != sizeData.end()) {
            LWARNING(fmt::format("{}: Duplicate entry for {}", _identifier, line));
            continue;
        }

        auto download = std::make_unique<HttpFileDownload>(
            line,
            destination,
            HttpFileDownload::Overwrite::Yes
        );
        HttpFileDownload* dl = download.get();
        downloads.push_back(std::move(download));

        sizeData[line] = SizeData();

        dl->onProgress(
            [this, line, &sizeData, &mutex, &startedAllDownloads](int64_t downloadedBytes,
                                                        std::optional<int64_t> totalBytes)
        {
            if (!totalBytes.has_value() || !startedAllDownloads) {
                return !_shouldCancel;
            }

            std::lock_guard guard(mutex);

            sizeData[line] = { downloadedBytes, totalBytes };

            _nTotalBytesKnown = true;
            _nTotalBytes = 0;
            _nSynchronizedBytes = 0;
            for (const std::pair<const std::string, SizeData>& sd : sizeData) {
                _nTotalBytesKnown = _nTotalBytesKnown && sd.second.totalBytes.has_value();
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
            LERROR(fmt::format("Error downloading file from URL {}", d->url()));
            failed = true;
            continue;
        }

        // If we are forcing the override, we download to a temporary file first, so when
        // we are done here, we need to rename the file to the original name

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
            LERROR(fmt::format("Error renaming {} to {}", tempName, originalName));
            failed = true;
        }
    }
    if (failed) {
        for (const std::unique_ptr<HttpFileDownload>& d : downloads) {
            d->cancel();
        }
    }
    return !failed;
}

} // namespace openspace
