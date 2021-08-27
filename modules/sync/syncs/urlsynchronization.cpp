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

#include <modules/sync/syncs/urlsynchronization.h>

#include <modules/sync/syncmodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/util/httprequest.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <filesystem>
#include <fstream>
#include <numeric>
#include <memory>
#include <optional>
#include <variant>

namespace {
    constexpr const char* TempSuffix = ".tmp";

    struct [[codegen::Dictionary(UrlSynchronization)]] Parameters {
        // The URL or urls from where the files are downloaded. If multiple URLs are
        // provided, all files will be downloaded to the same directory
        std::variant<std::string, std::vector<std::string>> url;

        // This optional identifier will be part of the used folder structure and, if 
        // provided, can be used to manually find the downloaded folder in the
        // synchronization folder. If this value is not specified, 'UseHash' has to be set
        // to 'true'
        std::optional<std::string> identifier;

        // If this value is set to 'true' and it is not overwritten by the global
        // settings, the file(s) pointed to by this URLSynchronization will always be 
        // downloaded, thus overwriting the local files. This is useful for files that are 
        // updated regularly remotely and should be fetch at every startup
        std::optional<bool> forceOverride [[codegen::key("override")]];

        // If this value is set to 'true' (the default), the hash of the URL is appended
        // to the directory name to produce a unique directory under all circumstances. If 
        // this is not desired, the URLSynchronization use the bare directory name alone
        // if this value is 'false'. If this value is 'false', the identifier has to be 
        // specified
        std::optional<bool> useHash;

        // Optional to provide filename to override the one which is otherwise
        // automatically created from the url
        std::optional<std::string> filename;
    };
#include "urlsynchronization_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation UrlSynchronization::Documentation() {
    return codegen::doc<Parameters>("sync_synchronization_url");
}

UrlSynchronization::UrlSynchronization(const ghoul::Dictionary& dict,
                                       std::string synchronizationRoot)
    : ResourceSynchronization(dict)
    , _synchronizationRoot(std::move(synchronizationRoot))
{
    const Parameters p = codegen::bake<Parameters>(dict);

    if (std::holds_alternative<std::string>(p.url)) {
        _urls.push_back(std::get<std::string>(p.url));

    }
    else if (std::holds_alternative<std::vector<std::string>>(p.url)) {
        _urls = std::get<std::vector<std::string>>(p.url);
    }
    else {
        throw ghoul::MissingCaseException();
    }

    _filename = p.filename.value_or(_filename);

    bool useHash = p.useHash.value_or(true);

    // We just merge all of the URLs together to generate a hash, it's not as stable to
    // reordering URLs, but every other solution would be more error prone
    std::string urlConcat = std::accumulate(_urls.begin(), _urls.end(), std::string());
    size_t hash = std::hash<std::string>{}(urlConcat);
    if (p.identifier.has_value()) {
        if (useHash) {
            _identifier = *p.identifier + "(" + std::to_string(hash) + ")";
        }
        else {
            _identifier = *p.identifier;
        }
    }
    else {
        if (useHash) {
            _identifier = std::to_string(hash);
        }
        else {
            documentation::TestResult res;
            res.success = false;
            documentation::TestResult::Offense o;
            o.offender = "Identifier|UseHash";
            o.reason = documentation::TestResult::Offense::Reason::MissingKey;
            res.offenses.push_back(o);
            throw documentation::SpecificationError(std::move(res), "UrlSynchronization");
        }
    }

    _forceOverride = p.forceOverride.value_or(_forceOverride);
}

UrlSynchronization::~UrlSynchronization() {
    if (_syncThread.joinable()) {
        cancel();
        _syncThread.join();
    }
}

void UrlSynchronization::start() {
    if (isSyncing()) {
        return;
    }
    begin();

    if (hasSyncFile() && !_forceOverride) {
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
            if (_filename.empty()) {
                const size_t lastSlash = url.find_last_of('/');
                std::string lastPartOfUrl = url.substr(lastSlash + 1);

                // We can not create filenames with questionmarks
                lastPartOfUrl.erase(
                    std::remove(lastPartOfUrl.begin(), lastPartOfUrl.end(), '?'),
                    lastPartOfUrl.end()
                );
                _filename = lastPartOfUrl;
            }
            std::string fileDestination = fmt::format(
                "{}/{}{}", directory(), _filename, TempSuffix
            );

            std::unique_ptr<AsyncHttpFileDownload> download =
                std::make_unique<AsyncHttpFileDownload>(
                    url,
                    fileDestination,
                    HttpFileDownload::Overwrite::Yes
                );

            downloads.push_back(std::move(download));

            std::unique_ptr<AsyncHttpFileDownload>& fileDownload = downloads.back();

            ++nDownloads;

            fileDownload->onProgress(
                [this, url, &fileSizes, &fileSizeMutex,
                &startedAllDownloads, &nDownloads](HttpRequest::Progress p)
            {
                if (p.totalBytesKnown) {
                    std::lock_guard guard(fileSizeMutex);
                    fileSizes[url] = p.totalBytes;

                    if (!_nTotalBytesKnown && startedAllDownloads &&
                        fileSizes.size() == nDownloads)
                    {
                        _nTotalBytesKnown = true;
                        _nTotalBytes = std::accumulate(
                            fileSizes.begin(),
                            fileSizes.end(),
                            size_t(0),
                            [](size_t a, const std::pair<const std::string, size_t> b) {
                                return a + b.second;
                            }
                        );
                    }
                }
                return !_shouldCancel;
            });

            HttpRequest::RequestOptions opt = {};
            opt.requestTimeoutSeconds = 0;
            fileDownload->start(opt);
        }

        startedAllDownloads = true;

        bool failed = false;
        for (std::unique_ptr<AsyncHttpFileDownload>& d : downloads) {
            d->wait();
            if (d->hasSucceeded()) {
                // If we are forcing the override, we download to a temporary file first,
                // so when we are done here, we need to rename the file to the original
                // name

                const std::string& tempName = d->destination();
                std::string originalName = tempName.substr(
                    0,
                    tempName.size() - strlen(TempSuffix)
                );

                if (std::filesystem::is_regular_file(originalName)) {
                    std::filesystem::remove(originalName);
                }
                int success = rename(tempName.c_str(), originalName.c_str());
                if (success != 0) {
                    LERRORC(
                        "URLSynchronization",
                        fmt::format(
                            "Error renaming file {} to {}", tempName, originalName
                        )
                    );

                    failed = true;
                }
            }
            else {
                failed = true;
            }
        }

        if (!failed) {
            createSyncFile();
        }
        else {
            for (std::unique_ptr<AsyncHttpFileDownload>& d : downloads) {
                d->cancel();
            }
        }
        resolve();
    });
}

void UrlSynchronization::cancel() {
    _shouldCancel = true;
    reset();
}

void UrlSynchronization::clear() {
    cancel();
    // TODO: Remove all files from directory.
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

void UrlSynchronization::createSyncFile() {
    std::string dir = directory();
    std::string filepath = dir + ".ossync";
    std::filesystem::create_directories(dir);
    std::ofstream syncFile(filepath, std::ofstream::out);
    syncFile << "Synchronized";
    syncFile.close();
}

bool UrlSynchronization::hasSyncFile() {
    const std::string& path = directory() + ".ossync";
    return std::filesystem::is_regular_file(path);
}

std::string UrlSynchronization::directory() {
    std::string d = fmt::format("{}/url/{}/files", _synchronizationRoot, _identifier);
    return absPath(d).string();
}

} // namespace openspace
