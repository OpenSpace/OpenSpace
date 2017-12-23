/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include "httpsynchronization.h"

#include <modules/sync/syncmodule.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/util/httprequest.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/documentation/verifier.h>

#include <sstream>
#include <fstream>
#include <numeric>
#include <memory>

namespace {
    constexpr const char* _loggerCat = "HttpSynchronization";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyVersion = "Version";

    constexpr const char* QueryKeyIdentifier = "identifier";
    constexpr const char* QueryKeyFileVersion = "file_version";
    constexpr const char* QueryKeyApplicationVersion = "application_version";
    constexpr const int ApplicationVersion = 1;
}

namespace openspace {

HttpSynchronization::HttpSynchronization(
    const ghoul::Dictionary& dict,
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
        "HttpSynchroniztion"
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

    std::vector<std::string> listUrls = fileListUrls();
    _syncThread = std::thread([this, listUrls] {
        for (const auto& url : listUrls) {
            if (trySyncFromUrl(url)) {
                createSyncFile();
                resolve();
                return;
            }
        }
        if (!_shouldCancel) {
            reject();
        }
    });
}

void HttpSynchronization::cancel() {
    _shouldCancel = true;
    reset();
}

void HttpSynchronization::clear() {
    cancel();
    // TODO: Remove all files from directory.
}

std::vector<std::string> HttpSynchronization::fileListUrls() {
    std::string query = std::string("?") + QueryKeyIdentifier + "=" + _identifier +
        "&" + QueryKeyFileVersion + "=" + std::to_string(_version) +
        "&" + QueryKeyApplicationVersion + "=" + std::to_string(ApplicationVersion);

    std::vector<std::string> urls;
    for (const auto& repoUrl : _synchronizationRepositories) {
        urls.push_back(repoUrl + query);
    }

    return urls;
}

bool HttpSynchronization::hasSyncFile() {
    std::string path = directory() + ".ossync";
    return FileSys.fileExists(path);
}

bool HttpSynchronization::trySyncFromUrl(std::string listUrl) {
    HttpRequest::RequestOptions opt;
    opt.requestTimeoutSeconds = 0;

    SyncHttpMemoryDownload fileListDownload(listUrl);
    fileListDownload.onProgress([this](HttpRequest::Progress) {
        return !_shouldCancel;
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

    std::string line = "";

    std::unordered_map<std::string, size_t> fileSizes;
    std::mutex fileSizeMutex;
    std::atomic_bool startedAllDownloads = false;
    std::atomic_size_t nDownloads = 0;
    
    std::vector<std::unique_ptr<AsyncHttpFileDownload>> downloads;
    
    while (fileList >> line) {
        size_t lastSlash = line.find_last_of('/');
        std::string filename = line.substr(lastSlash + 1);
        
        std::string fileDestination = directory() +
        ghoul::filesystem::FileSystem::PathSeparator +
        filename;
        
        downloads.push_back(std::make_unique<AsyncHttpFileDownload>(
            line, fileDestination, HttpFileDownload::Overwrite::Yes));

        auto& fileDownload = downloads.back();
        
        ++nDownloads;
        
        fileDownload->onProgress(
            [this, line, &fileSizes, &fileSizeMutex,
             &startedAllDownloads, &nDownloads](HttpRequest::Progress p)
        {
            if (p.totalBytesKnown) {
                std::lock_guard<std::mutex> guard(fileSizeMutex);
                fileSizes[line] = p.totalBytes;
                
                if (!_nTotalBytesKnown && startedAllDownloads && fileSizes.size() == nDownloads) {
                    _nTotalBytesKnown = true;
                    _nTotalBytes = std::accumulate(fileSizes.begin(), fileSizes.end(), size_t(0),
                        [](size_t a, auto b) {
                        return a + b.second;
                    });
                }
            }
            return !_shouldCancel;
        });

        fileDownload->start(opt);
    }
    startedAllDownloads = true;
    
    bool failed = false;
    for (auto& d : downloads) {
        d->wait();
        if (!d->hasSucceeded()) {
            failed = true;
        }
    }
    if (!failed) {
        return true;
    }
    for (auto& d : downloads) {
        d->cancel();
    }
    return false;
}

void HttpSynchronization::createSyncFile() {
    std::string dir = directory();
    std::string filepath = dir + ".ossync";
    FileSys.createDirectory(dir, ghoul::filesystem::Directory::Recursive::Yes);
    std::ofstream syncFile(filepath, std::ofstream::out);
    syncFile << "Synchronized";
    syncFile.close();
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

} // namespace openspace

