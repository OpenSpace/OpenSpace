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

#include <modules/sync/syncs/urlsynchronization.h>

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
    constexpr const char* KeyUrl = "Url";
    constexpr const char* KeyIdentifier = "Identifier";
} // namespace

namespace openspace {

documentation::Documentation UrlSynchronization::Documentation() {
    using namespace openspace::documentation;
    return {
        "Url Synchronization",
        "sync_synchronization_url",
        {
            {
                KeyUrl,
                new OrVerifier(new StringVerifier, new StringListVerifier),
                Optional::No,
                "The URL or urls from where the files are downloaded. If multiple URLs "
                "are provided, all files will be downloaded to the same directory."
            },
            {
                KeyIdentifier,
                new StringVerifier,
                Optional::No,
                "This identifier must be unique among all downloaded files and will be "
                "part of the returned path."
            }
        }
    };
}

UrlSynchronization::UrlSynchronization(const ghoul::Dictionary& dict,
                                       const std::string& synchronizationRoot)
    : openspace::ResourceSynchronization(dict)
    , _nTotalBytesKnown(false)
    , _nTotalBytes(0)
    , _nSynchronizedBytes(0)
    , _shouldCancel(false)
    , _synchronizationRoot(synchronizationRoot)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dict,
        "UrlSynchroniztion"
    );

    _identifier = dict.value<std::string>(KeyIdentifier);

    if (dict.hasValue<std::string>(KeyUrl)) {
        _urls.push_back(dict.value<std::string>(KeyUrl));
    }
    else {
        ghoul::Dictionary urls = dict.value<ghoul::Dictionary>(KeyUrl);
        for (int i = 1; i <= urls.size(); ++i) {
            std::string url = urls.value<std::string>(std::to_string(i));
            _urls.push_back(std::move(url));
        }
    }
}

UrlSynchronization::~UrlSynchronization() {
    if (_syncThread.joinable()) {
        cancel();
        _syncThread.join();
    }
}



std::string UrlSynchronization::directory() {
    ghoul::filesystem::Directory d(
        _synchronizationRoot + ghoul::filesystem::FileSystem::PathSeparator +
        "http" + ghoul::filesystem::FileSystem::PathSeparator +
        _identifier + ghoul::filesystem::FileSystem::PathSeparator +
        "files"
    );

    return absPath(d);
}

void UrlSynchronization::start() {
    if (isSyncing()) {
        return;
    }
    begin();

    if (hasSyncFile()) {
        resolve();
        return;
    }


    _syncThread = std::thread([this] {
        std::unordered_map<std::string, size_t> fileSizes;
        std::mutex fileSizeMutex;
        std::atomic_size_t nDownloads(0);
        std::atomic_bool startedAllDownloads(false);
        std::vector<std::unique_ptr<AsyncHttpFileDownload>> downloads;

        for (const std::string& url : _urls) {
            size_t lastSlash = url.find_last_of('/');
            std::string filename = url.substr(lastSlash + 1);

            std::string fileDestination = directory() +
                ghoul::filesystem::FileSystem::PathSeparator +
                filename;

            std::unique_ptr<AsyncHttpFileDownload> download =
                std::make_unique<AsyncHttpFileDownload>(
                    url, fileDestination, HttpFileDownload::Overwrite::Yes
            );
            downloads.push_back(std::move(download));

            std::unique_ptr<AsyncHttpFileDownload>& fileDownload = downloads.back();

            ++nDownloads;

            fileDownload->onProgress(
                [this, url, &fileSizes, &fileSizeMutex,
                &startedAllDownloads, &nDownloads](HttpRequest::Progress p)
            {
                if (p.totalBytesKnown) {
                    std::lock_guard<std::mutex> guard(fileSizeMutex);
                    fileSizes[url] = p.totalBytes;

                    if (!_nTotalBytesKnown && startedAllDownloads &&
                        fileSizes.size() == nDownloads)
                    {
                        _nTotalBytesKnown = true;
                        _nTotalBytes = std::accumulate(
                            fileSizes.begin(),
                            fileSizes.end(),
                            size_t(0),
                            [](size_t a, auto b) {
                                return a + b.second;
                            }
                        );
                    }
                }
                return !_shouldCancel;
            });

            HttpRequest::RequestOptions opt;
            opt.requestTimeoutSeconds = 0;
            fileDownload->start(opt);

        }

        startedAllDownloads = true;

        bool failed = false;
        for (std::unique_ptr<AsyncHttpFileDownload>& d : downloads) {
            d->wait();
            if (!d->hasSucceeded()) {
                failed = true;
            }
        }

        if (!failed) {
            createSyncFile();
        }
        else {
            for (auto& d : downloads) {
                d->cancel();
            }
        }
        resolve();
    });




    //std::vector<std::string> listUrls = fileListUrls();
    //_syncThread = std::thread([this, listUrls] {
    //    for (const auto& url : listUrls) {
    //        if (trySyncFromUrl(url)) {
    //            createSyncFile();
    //            resolve();
    //            return;
    //        }
    //    }
    //    if (!_shouldCancel) {
    //        reject();
    //    }
    //});
}

void UrlSynchronization::cancel() {
    _shouldCancel = true;
    reset();
}

void UrlSynchronization::clear() {
    cancel();
    // TODO: Remove all files from directory.
}

bool UrlSynchronization::hasSyncFile() {
    std::string path = directory() + ".ossync";
    return FileSys.fileExists(path);
}

//bool UrlSynchronization::trySyncFromUrl(std::string listUrl) {
//    HttpRequest::RequestOptions opt;
//    opt.requestTimeoutSeconds = 0;
//
//    SyncHttpMemoryDownload fileListDownload(listUrl);
//    fileListDownload.onProgress([this](HttpRequest::Progress) {
//        return !_shouldCancel;
//    });
//    fileListDownload.download(opt);
//
//    if (!fileListDownload.hasSucceeded()) {
//        return false;
//    }
//
//    const std::vector<char>& buffer = fileListDownload.downloadedData();
//    _nSynchronizedBytes = 0;
//    _nTotalBytes = 0;
//    _nTotalBytesKnown = false;
//
//    std::istringstream fileList(std::string(buffer.begin(), buffer.end()));
//
//    std::string line = "";
//
//    std::unordered_map<std::string, size_t> fileSizes;
//    std::mutex fileSizeMutex;
//    std::atomic_bool startedAllDownloads(false);
//    std::atomic_size_t nDownloads(0);
//
//    std::vector<std::unique_ptr<AsyncHttpFileDownload>> downloads;
//
//    while (fileList >> line) {
//        size_t lastSlash = line.find_last_of('/');
//        std::string filename = line.substr(lastSlash + 1);
//
//        std::string fileDestination = directory() +
//        ghoul::filesystem::FileSystem::PathSeparator +
//        filename;
//
//        downloads.push_back(std::make_unique<AsyncHttpFileDownload>(
//            line, fileDestination, HttpFileDownload::Overwrite::Yes));
//
//        auto& fileDownload = downloads.back();
//
//        ++nDownloads;
//
//        fileDownload->onProgress(
//            [this, line, &fileSizes, &fileSizeMutex,
//             &startedAllDownloads, &nDownloads](HttpRequest::Progress p)
//        {
//            if (p.totalBytesKnown) {
//                std::lock_guard<std::mutex> guard(fileSizeMutex);
//                fileSizes[line] = p.totalBytes;
//
//                if (!_nTotalBytesKnown && startedAllDownloads &&
//                    fileSizes.size() == nDownloads)
//                {
//                    _nTotalBytesKnown = true;
//                    _nTotalBytes = std::accumulate(
//                        fileSizes.begin(),
//                        fileSizes.end(),
//                        size_t(0),
//                        [](size_t a, auto b) {
//                            return a + b.second;
//                        }
//                    );
//                }
//            }
//            return !_shouldCancel;
//        });
//
//        fileDownload->start(opt);
//    }
//    startedAllDownloads = true;
//
//    bool failed = false;
//    for (auto& d : downloads) {
//        d->wait();
//        if (!d->hasSucceeded()) {
//            failed = true;
//        }
//    }
//    if (!failed) {
//        return true;
//    }
//    for (auto& d : downloads) {
//        d->cancel();
//    }
//    return false;
//}

void UrlSynchronization::createSyncFile() {
    std::string dir = directory();
    std::string filepath = dir + ".ossync";
    FileSys.createDirectory(dir, ghoul::filesystem::Directory::Recursive::Yes);
    std::ofstream syncFile(filepath, std::ofstream::out);
    syncFile << "Synchronized";
    syncFile.close();
}

size_t UrlSynchronization::nSynchronizedBytes() {
    return _nSynchronizedBytes;
}

size_t UrlSynchronization::nTotalBytes() {
    return _nTotalBytes;
}

bool UrlSynchronization::nTotalBytesIsKnown() {
    return _nTotalBytesKnown;
}

} // namespace openspace

