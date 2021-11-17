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

#include <modules/sync/syncs/httpsynchronization.h>

#include <modules/sync/syncmodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/httprequest.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <fstream>
#include <numeric>

namespace {
    constexpr const char* _loggerCat = "HttpSynchronization";

    constexpr const char* TempSuffix = ".tmp";

    constexpr const char* QueryKeyIdentifier = "identifier";
    constexpr const char* QueryKeyFileVersion = "file_version";
    constexpr const char* QueryKeyApplicationVersion = "application_version";
    constexpr const int ApplicationVersion = 1;

    struct [[codegen::Dictionary(HttpSynchronization)]] Parameters {
        // A unique identifier for this resource
        std::string identifier;

        // The version of this resource
        int version;
    };
#include "httpsynchronization_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation HttpSynchronization::Documentation() {
    return codegen::doc<Parameters>("http_synchronization");
}

HttpSynchronization::HttpSynchronization(const ghoul::Dictionary& dict,
                                         std::string synchronizationRoot,
                                      std::vector<std::string> synchronizationRepositories
)
    : ResourceSynchronization(dict)
    , _synchronizationRoot(std::move(synchronizationRoot))
    , _synchronizationRepositories(std::move(synchronizationRepositories))
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
    std::string d = fmt::format(
        "{}/http/{}/{}", _synchronizationRoot, _identifier, _version
    );
    return absPath(d);
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

    const std::string& query = fmt::format(
        "?identifier={}&file_version={}&application_version={}",
        _identifier, _version, ApplicationVersion
    );

    _syncThread = std::thread(
        [this](const std::string& q) {
            for (const std::string& url : _synchronizationRepositories) {
                if (trySyncFromUrl(url + q)) {
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

void HttpSynchronization::clear() {
    cancel();
    // TODO: Remove all files from directory.
}

size_t HttpSynchronization::nSynchronizedBytes() const {
    return _nSynchronizedBytes;
}

size_t HttpSynchronization::nTotalBytes() const {
    return _nTotalBytes;
}

bool HttpSynchronization::nTotalBytesIsKnown() const {
    return _nTotalBytesKnown;
}

bool HttpSynchronization::trySyncFromUrl(std::string listUrl) {
    HttpRequest::RequestOptions opt = {};
    opt.requestTimeoutSeconds = 0;

    SyncHttpMemoryDownload fileListDownload(std::move(listUrl));
    fileListDownload.onProgress([&c = _shouldCancel](HttpRequest::Progress) {
        return !c;
    });
    fileListDownload.download(opt);

    if (!fileListDownload.hasSucceeded()) {
        return false;
    }

    const std::vector<char>& buffer = fileListDownload.downloadedData();
    _nSynchronizedBytes = 0;
    _nTotalBytes = 0;
    _nTotalBytesKnown = false;

    std::istringstream fileList(std::string(buffer.begin(), buffer.end()));

    struct SizeData {
        bool totalKnown;
        size_t totalBytes;
        size_t downloadedBytes;
    };

    std::unordered_map<std::string, SizeData> sizeData;
    std::mutex sizeDataMutex;

    std::atomic_bool startedAllDownloads(false);

    std::vector<std::unique_ptr<AsyncHttpFileDownload>> downloads;

    std::string line;
    while (fileList >> line) {
        std::string filename = std::filesystem::path(line).filename().string();
        std::string destination = (directory() / (filename + TempSuffix)).string();

        if (sizeData.find(line) != sizeData.end()) {
            LWARNING(fmt::format("{}: Duplicate entries: {}", _identifier, line));
            continue;
        }

        downloads.push_back(std::make_unique<AsyncHttpFileDownload>(
            line,
            destination,
            HttpFileDownload::Overwrite::Yes
        ));

        std::unique_ptr<AsyncHttpFileDownload>& fileDownload = downloads.back();

        sizeData[line] = { false, 0, 0 };

        fileDownload->onProgress(
            [this, line, &sizeData, &sizeDataMutex,
             &startedAllDownloads](HttpRequest::Progress p)
        {
            if (!p.totalBytesKnown || !startedAllDownloads) {
                return !_shouldCancel;
            }

            std::lock_guard guard(sizeDataMutex);

            sizeData[line] = { p.totalBytesKnown, p.totalBytes, p.downloadedBytes };

            SizeData size = std::accumulate(
                sizeData.begin(),
                sizeData.end(),
                SizeData{ true, 0, 0 },
                [](const SizeData& a, const std::pair<const std::string, SizeData>& b) {
                    return SizeData {
                        a.totalKnown && b.second.totalKnown,
                        a.totalBytes + b.second.totalBytes,
                        a.downloadedBytes + b.second.downloadedBytes
                    };
                }
            );

            _nTotalBytesKnown = size.totalKnown;
            _nTotalBytes = size.totalBytes;
            _nSynchronizedBytes = size.downloadedBytes;

            return !_shouldCancel;
        });

        fileDownload->start(opt);
    }
    startedAllDownloads = true;

    bool failed = false;
    for (const std::unique_ptr<AsyncHttpFileDownload>& d : downloads) {
        d->wait();
        if (d->hasSucceeded()) {
            // If we are forcing the override, we download to a temporary file
            // first, so when we are done here, we need to rename the file to the
            // original name

            std::filesystem::path tempName = d->destination();
            std::filesystem::path originalName = tempName;
            // Remove the .tmp extension
            originalName.replace_extension("");

            if (std::filesystem::is_regular_file(originalName)) {
                std::filesystem::remove(originalName);
            }
            std::error_code ec;
            std::filesystem::rename(tempName, originalName);
            if (ec) {
                LERROR(fmt::format(
                    "Error renaming file {} to {}", tempName, originalName
                ));
                failed = true;
            }
        }
        else {
            LERROR(fmt::format("Error downloading file from URL {}", d->url()));
            failed = true;
        }
    }
    if (!failed) {
        return true;
    }
    for (const std::unique_ptr<AsyncHttpFileDownload>& d : downloads) {
        d->cancel();
    }
    return false;
}

} // namespace openspace
