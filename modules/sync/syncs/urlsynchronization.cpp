/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <ghoul/logging/logmanager.h>
#include <numeric>
#include <mutex>
#include <optional>
#include <unordered_map>
#include <variant>

namespace {
    constexpr const char* TempSuffix = ".tmp";

    struct [[codegen::Dictionary(UrlSynchronization)]] Parameters {
        // The URL or urls from where the files are downloaded. If multiple URLs are
        // provided, all files will be downloaded to the same directory and the filename
        // parameter must not be specified simultaneously
        std::variant<std::string, std::vector<std::string>> url;

        // This identifier will be part of the used folder structure and, can be used to
        // manually find the downloaded folder in the synchronization folder
        std::string identifier;

        // If this value is set to 'true' and it is not overwritten by the global
        // settings, the file(s) pointed to by this URLSynchronization will always be
        // downloaded, thus overwriting the local files. This is useful for files that are
        // updated regularly remotely and should be fetch at every startup
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
        throw ghoul::RuntimeError(fmt::format(
            "UrlSynchronization ({}) requested overwrite filename but specified {} URLs "
            "to download, which is not legal",
            p.identifier, _urls.size()
        ));
    }
    _filename = p.filename.value_or(_filename);
    _forceOverride = p.forceOverride.value_or(_forceOverride);

    const bool useHash = p.useHash.value_or(true);

    _identifier = p.identifier;

    if (useHash) {
        // We just merge all of the URLs together to generate a hash that works for this
        std::vector<std::string> urls = _urls;
        std::sort(urls.begin(), urls.end());

        size_t hash = std::hash<std::string>{}(
            std::accumulate(urls.begin(), urls.end(), std::string())
        );
        _identifier += fmt::format("({})", hash);
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

    if (hasSyncFile() && !_forceOverride) {
        _state = State::Resolved;
        return;
    }

    _syncThread = std::thread([this]() {
        std::unordered_map<std::string, size_t> fileSizes;
        std::mutex fileSizeMutex;
        size_t nDownloads = 0;
        std::atomic_bool startedAllDownloads = false;
        std::vector<std::unique_ptr<HttpFileDownload>> downloads;

        for (const std::string& url : _urls) {
            if (_filename.empty() || _urls.size() > 1) {
                std::string name = std::filesystem::path(url).filename().string();

                // We can not create filenames with question marks
                name.erase(std::remove(name.begin(), name.end(), '?'), name.end());
                _filename = name;
            }
            std::filesystem::path destination = directory() / (_filename + TempSuffix);

            auto download = std::make_unique<HttpFileDownload>(
                url,
                destination,
                HttpFileDownload::Overwrite::Yes
            );
            HttpFileDownload* dl = download.get();

            downloads.push_back(std::move(download));

            ++nDownloads;

            dl->onProgress(
                [this, url, &fileSizes, &fileSizeMutex,
                &startedAllDownloads, &nDownloads](int64_t,
                                                   std::optional<int64_t> totalBytes)
            {
                if (!totalBytes.has_value()) {
                    return !_shouldCancel;
                }

                std::lock_guard guard(fileSizeMutex);
                fileSizes[url] = *totalBytes;

                if (!_nTotalBytesKnown && startedAllDownloads &&
                    fileSizes.size() == nDownloads)
                {
                    _nTotalBytesKnown = true;
                    _nTotalBytes = 0;
                    for (const std::pair<const std::string, size_t>& fs : fileSizes) {
                        _nTotalBytes += fs.second;
                    }
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
                    fmt::format("Error renaming file {} to {}", tempName, originalName)
                );

                failed = true;
            }
        }

        if (!failed) {
            createSyncFile();
        }
        else {
            for (const std::unique_ptr<HttpFileDownload>& d : downloads) {
                d->cancel();
            }
        }
        _state = State::Resolved;
    });
}

void UrlSynchronization::cancel() {
    _shouldCancel = true;
    _state = State::Unsynced;
}

std::string UrlSynchronization::generateUid() {
    return _identifier;
}

} // namespace openspace
