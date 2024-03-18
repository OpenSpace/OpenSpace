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

#include <modules/sync/syncs/httpsynchronization.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/httprequest.h>
#include <ghoul/ext/assimp/contrib/zip/src/zip.h>
#include <ghoul/logging/logmanager.h>
#include <fstream>
#include <unordered_map>

namespace {
    constexpr std::string_view _loggerCat = "HttpSynchronization";

    constexpr int ApplicationVersion = 1;

    constexpr std::string_view OssyncVersionNumber = "1.0";
    constexpr std::string_view SynchronizationToken = "Synchronized";

    struct [[codegen::Dictionary(HttpSynchronization)]] Parameters {
        // The unique identifier for this resource that is used to request a set of files
        // from the synchronization servers
        std::string identifier [[codegen::identifier()]];

        // The version of this resource that should be requested
        int version;

        // Determines whether .zip files that are downloaded should automatically be
        // unzipped. If this value is not specified, no unzipping is performed
        std::optional<bool> unzipFiles;

        // The destination for the unzipping. If this value is specified, all zip files
        // contained in the synchronization will be unzipped into the same specified
        // folder. If this value is specified, but 'unzipFiles' is false, no extaction
        // will be performed
        std::optional<std::string> unzipFilesDestination;
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
    _unzipFiles = p.unzipFiles.value_or(_unzipFiles);
    if (p.unzipFilesDestination.has_value()) {
        _unzipFilesDestination = *p.unzipFilesDestination;
    }
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

    const bool isSynced = isEachFileDownloaded();
    if (isSynced) {
        _state = State::Resolved;
        return;
    }

    if (isRejected()) {
        return;
    }

    const std::string query = fmt::format(
        "?identifier={}&file_version={}&application_version={}",
        _identifier, _version, ApplicationVersion
    );

    _syncThread = std::thread(
        [this](const std::string& q) {
            for (const std::string& url : _syncRepositories) {
                const SynchronizationState syncState = trySyncFromUrl(url + q);

                // Could not get this sync repository list of files.
                if (syncState == SynchronizationState::ListDownloadFail) {
                    continue;
                }

                if (syncState == SynchronizationState::Success) {
                    _state = State::Resolved;
                    createSyncFile(true);
                }
                else if (syncState == SynchronizationState::FileDownloadFail) {
                    // If it was not successful we should add any files that were
                    // potentially downloaded to avoid downloading from other repositories
                    _existingSyncedFiles.insert(
                        _existingSyncedFiles.end(),
                        _newSyncedFiles.begin(),
                        _newSyncedFiles.end()
                    );
                    _newSyncedFiles.clear();
                    createSyncFile(false);
                }
                break;
            }

            if (!isResolved() && !_shouldCancel) {
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

void HttpSynchronization::createSyncFile(bool isFullySynchronized) const {
    std::filesystem::path dir = directory();
    std::filesystem::create_directories(dir);

    dir.replace_extension("ossync");
    std::ofstream syncFile(dir, std::ofstream::out);

    syncFile << fmt::format(
        "{}\n{}\n",
        OssyncVersionNumber,
        (isFullySynchronized ? SynchronizationToken : "Partial Synchronized")
    );

    if (isFullySynchronized) {
        // All files successfully downloaded, no need to write anything else to file
        return;
    }

    // Store all files that successfully downloaded
    for (const std::string& fileURL : _existingSyncedFiles) {
        syncFile << fileURL << '\n';
    }
}

bool HttpSynchronization::isEachFileDownloaded() {
    std::filesystem::path path = directory();
    path.replace_extension("ossync");
    // Check if file exists at all
    if (!std::filesystem::is_regular_file(path)) {
        return false;
    }

    // Read contents of file
    std::ifstream file(path);
    std::string line;

    file >> line;
    // Ossync files that does not have a version number are already resolved
    // As they are of the previous format.
    if (line == SynchronizationToken) {
        return true;
    }
    // Otherwise first line is the version number.
    std::string ossyncVersion = line;

    //Format of 1.0 ossync:
    //Version number: E.g., 1.0
    //Synchronization status: Synchronized or Partial Synchronized
    //Optionally list of already synched files

    if (ossyncVersion == OssyncVersionNumber) {
        std::getline(file >> std::ws, line); // Read synchronization status
        if (line == SynchronizationToken) {
            return true;
        }
        // File is only partially synchronized,
        // store file urls that have been synched already
        while (file >> line) {
            if (line.empty() || line[0] == '#') {
                // Skip all empty lines and commented out lines
                continue;
            }
            _existingSyncedFiles.push_back(line);
        }
    }
    else {
        LERROR(fmt::format(
            "{}: Unknown ossync version number read."
            "Got {} while {} and below are valid.",
            _identifier,
            ossyncVersion,
            OssyncVersionNumber
        ));
        _state = State::Rejected;
    }
    return false;
}

HttpSynchronization::SynchronizationState
HttpSynchronization::trySyncFromUrl(std::string url) {
    HttpMemoryDownload fileListDownload = HttpMemoryDownload(std::move(url));
    fileListDownload.onProgress([&c = _shouldCancel](int64_t, std::optional<int64_t>) {
        return !c;
    });
    fileListDownload.start();
    const bool success = fileListDownload.wait();

    const std::vector<char>& buffer = fileListDownload.downloadedData();
    if (!success) {
        LERRORC("HttpSynchronization", std::string(buffer.begin(), buffer.end()));
        return SynchronizationState::ListDownloadFail;
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

        const std::string filename = std::filesystem::path(line).filename().string();
        const std::filesystem::path destination = directory() / (filename + ".tmp");

        if (sizeData.find(line) != sizeData.end()) {
            LWARNING(fmt::format("{}: Duplicate entry for {}", _identifier, line));
            continue;
        }

        // If the file is among the stored files in ossync we ignore that download
        auto it = std::find(
            _existingSyncedFiles.begin(),
            _existingSyncedFiles.end(),
            line
        );

        if (it != _existingSyncedFiles.end()) {
            // File has already been synced
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

            const std::lock_guard guard(mutex);

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

    constexpr int MaxDownloadRetries = 5;
    int downloadTry = 0;
    // If a download has failed try to restart it
    while (downloadTry < MaxDownloadRetries) {
        bool downloadSucceeded = true;

        for (const std::unique_ptr<HttpFileDownload>& d : downloads) {
            d->wait();

            // If the user exits the program we don't want to start new downloads
            if (_shouldCancel) {
                break;
            }

            if (d->hasFailed()) {
                d->start();
                downloadSucceeded = false;
            }
        }
        if (downloadSucceeded || _shouldCancel) {
            break;
        }
        else {
            ++downloadTry;
            std::this_thread::sleep_for(std::chrono::seconds(1));
        }
    }

    bool failed = false;
    for (const std::unique_ptr<HttpFileDownload>& d : downloads) {
        d->wait();
        if (!d->hasSucceeded()) {
            LERROR(fmt::format("Error downloading file from URL '{}'", d->url()));
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
            LERROR(fmt::format("Error renaming '{}' to '{}'", tempName, originalName));
            failed = true;
        }

        if (_unzipFiles && originalName.extension() == ".zip") {
            std::string source = originalName.string();
            const std::string dest =
                _unzipFilesDestination.has_value() ?
                (originalName.parent_path() / *_unzipFilesDestination).string() :
                originalName.replace_extension().string();;

            struct zip_t* z = zip_open(source.c_str(), 0, 'r');
            const bool is64 = zip_is64(z);
            zip_close(z);

            if (is64) {
                LERROR(fmt::format(
                    "Error while unzipping '{}': Zip64 archives are not supported", source
                ));
                continue;
            }

            int ret = zip_extract(source.c_str(), dest.c_str(), nullptr, nullptr);
            if (ret != 0) {
                LERROR(fmt::format("Error '{}' while unzipping '{}'", ret, source));
                continue;
            }

            std::filesystem::remove(source);
        }
    }
    if (failed) {
        for (const std::unique_ptr<HttpFileDownload>& d : downloads) {
            // Store all files that were synced to the ossync
            if (d->hasSucceeded()) {
                _newSyncedFiles.push_back(d->url());
            }
        }
        return SynchronizationState::FileDownloadFail;
    }
    return SynchronizationState::Success;
}

} // namespace openspace
