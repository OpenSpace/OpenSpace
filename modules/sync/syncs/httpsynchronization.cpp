/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <fstream>
#include <numeric>

namespace {
    constexpr const char* _loggerCat = "HttpSynchronization";

    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyVersion = "Version";

    constexpr const char* TempSuffix = ".tmp";

    constexpr const char* QueryKeyIdentifier = "identifier";
    constexpr const char* QueryKeyFileVersion = "file_version";
    constexpr const char* QueryKeyApplicationVersion = "application_version";
    constexpr const int ApplicationVersion = 1;
} // namespace

namespace openspace {

documentation::Documentation HttpSynchronization::Documentation() {
    using namespace openspace::documentation;
    return {
        "HttpSynchronization",
        "http_synchronization",
    {
        {
            KeyIdentifier,
            new StringVerifier,
            Optional::No,
            "A unique identifier for this resource"
        },
        {
            KeyVersion,
            new IntVerifier,
            Optional::No,
            "The version of this resource"
        }
    }
    };
}

HttpSynchronization::HttpSynchronization(const ghoul::Dictionary& dict,
                                         const std::string& synchronizationRoot,
                               const std::vector<std::string>& synchronizationRepositories
)
    : openspace::ResourceSynchronization(dict)
    , _synchronizationRoot(synchronizationRoot)
    , _synchronizationRepositories(synchronizationRepositories)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dict,
        "HttpSynchronization"
    );

    _identifier = dict.value<std::string>(KeyIdentifier);
    _version = static_cast<int>(dict.value<double>(KeyVersion));
}

HttpSynchronization::~HttpSynchronization() {
    if (_syncThread.joinable()) {
        cancel();
        _syncThread.join();
    }
}

std::string HttpSynchronization::directory() {
    ghoul::filesystem::Directory d(
        _synchronizationRoot +
        ghoul::filesystem::FileSystem::PathSeparator +
        "http" +
        ghoul::filesystem::FileSystem::PathSeparator +
        _identifier +
        ghoul::filesystem::FileSystem::PathSeparator +
        std::to_string(_version)
    );

    return FileSys.absPath(d);
}

void HttpSynchronization::start() {
    if (isSyncing()) {
        return;
    }
    begin();

    if (hasSyncFile()) {
        resolve();
        return;
    }

    const std::string& query =
        std::string("?") + QueryKeyIdentifier + "=" + _identifier + "&" +
        QueryKeyFileVersion + "=" + std::to_string(_version) + "&" +
        QueryKeyApplicationVersion + "=" + std::to_string(ApplicationVersion);

    _syncThread = std::thread(
        [this](const std::string& q) {
            for (const std::string& url : _synchronizationRepositories) {
                if (trySyncFromUrl(url + q)) {
                    createSyncFile();
                    resolve();
                    return;
                }
            }
            if (!_shouldCancel) {
                reject();
            }
        },
        query
    );
}

void HttpSynchronization::cancel() {
    _shouldCancel = true;
    reset();
}

void HttpSynchronization::clear() {
    cancel();
    // TODO: Remove all files from directory.
}

size_t HttpSynchronization::nSynchronizedBytes() {
    return _nSynchronizedBytes;
}

size_t HttpSynchronization::nTotalBytes() {
    return _nTotalBytes;
}

bool HttpSynchronization::nTotalBytesIsKnown() {
    return _nTotalBytesKnown;
}

void HttpSynchronization::createSyncFile() {
    const std::string& directoryName = directory();
    const std::string& filepath = directoryName + ".ossync";

    FileSys.createDirectory(directoryName, ghoul::filesystem::FileSystem::Recursive::Yes);

    std::ofstream syncFile(filepath, std::ofstream::out);
    syncFile << "Synchronized";
    syncFile.close();
}

bool HttpSynchronization::hasSyncFile() {
    const std::string& path = directory() + ".ossync";
    return FileSys.fileExists(path);
}

bool HttpSynchronization::trySyncFromUrl(std::string listUrl) {
    HttpRequest::RequestOptions opt;
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

    std::string line;

    struct SizeData {
        bool totalKnown;
        size_t totalBytes;
        size_t downloadedBytes;
    };

    std::unordered_map<std::string, SizeData> sizeData;
    std::mutex sizeDataMutex;

    std::atomic_bool startedAllDownloads(false);

    std::vector<std::unique_ptr<AsyncHttpFileDownload>> downloads;

    while (fileList >> line) {
        size_t lastSlash = line.find_last_of('/');
        std::string filename = line.substr(lastSlash + 1);

        std::string fileDestination = directory() +
            ghoul::filesystem::FileSystem::PathSeparator +
            filename + TempSuffix;

        if (sizeData.find(line) != sizeData.end()) {
            LWARNING(fmt::format(
                "{}: Duplicate entries: {}", _identifier, line
            ));
            continue;
        }

        downloads.push_back(std::make_unique<AsyncHttpFileDownload>(
            line,
            fileDestination,
            HttpFileDownload::Overwrite::Yes
        ));

        std::unique_ptr<AsyncHttpFileDownload>& fileDownload = downloads.back();

        sizeData[line] = {
            false,
            0,
            0,
        };

        fileDownload->onProgress(
            [this, line, &sizeData, &sizeDataMutex,
             &startedAllDownloads](HttpRequest::Progress p)
        {
            if (!p.totalBytesKnown || !startedAllDownloads) {
                return !_shouldCancel;
            }

            std::lock_guard<std::mutex> guard(sizeDataMutex);

            sizeData[line] = {
                p.totalBytesKnown,
                p.totalBytes,
                p.downloadedBytes,
            };

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
    for (std::unique_ptr<AsyncHttpFileDownload>& d : downloads) {
        d->wait();
        if (d->hasSucceeded()) {
            // If we are forcing the override, we download to a temporary file
            // first, so when we are done here, we need to rename the file to the
            // original name

            const std::string& tempName = d->destination();
            std::string originalName = tempName.substr(
                0,
                tempName.size() - strlen(TempSuffix)
            );

            FileSys.deleteFile(originalName);
            int success = rename(tempName.c_str(), originalName.c_str());
            if (success != 0) {
                LERROR(fmt::format(
                    "Error renaming file {} to {}", tempName, originalName
                ));
                failed = true;
            }
        }
        else {
            LERROR(fmt::format(
                "Error downloading file from URL {}", d->url()
            ));
            failed = true;
        }
    }
    if (!failed) {
        return true;
    }
    for (std::unique_ptr<AsyncHttpFileDownload>& d : downloads) {
        d->cancel();
    }
    return false;
}

} // namespace openspace
