/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/util/versionchecker.h>

#include <openspace/openspace.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <sstream>

namespace {
    constexpr const char* _loggerCat = "VersionChecker";
} // namespace

namespace openspace {

VersionChecker::~VersionChecker() {
    cancel();
}

void VersionChecker::requestLatestVersion(const std::string& url) {
    HttpRequest::RequestOptions opt;
    opt.requestTimeoutSeconds = 0;

    const std::string fullUrl = url +
        "?client_version=" + OPENSPACE_VERSION_NUMBER +
        "&commit_hash=" + OPENSPACE_GIT_COMMIT;

    if (_request) {
        _request->cancel();
        _request->wait();
        _request = nullptr;
    }

    _request = std::make_unique<AsyncHttpMemoryDownload>(std::move(fullUrl));
    _request->start(opt);
}

void VersionChecker::cancel() {
    if (!_request) {
        return;
    }
    _request->cancel();
    _request->wait();
    _request = nullptr;
}

 bool VersionChecker::hasLatestVersionInfo() {
    if (_latestVersion.has_value()) {
        return true;
    }
    if (_request) {
        if (_request->hasSucceeded()) {
            _request->wait();
            std::vector<char> data = _request->downloadedData();
            std::string versionString(data.begin(), data.end());
            std::istringstream versionData(versionString);

            std::string token;
            std::getline(versionData, token, '.');
            int major = std::atoi(token.c_str());
            std::getline(versionData, token, '.');
            int minor = std::atoi(token.c_str());
            std::getline(versionData, token, '.');
            int patch = std::atoi(token.c_str());

            _latestVersion = { major, minor, patch };
            _request = nullptr;

            SemanticVersion currentVersion{
                OPENSPACE_VERSION_MAJOR,
                OPENSPACE_VERSION_MINOR,
                OPENSPACE_VERSION_PATCH
            };

            if (currentVersion < _latestVersion) {
                LINFO(fmt::format(
                    "Newer OpenSpace version {}.{}.{} is available. "
                    "Currently running {}.{}.{}",
                    _latestVersion->major,
                    _latestVersion->minor,
                    _latestVersion->patch,
                    currentVersion.major,
                    currentVersion.minor,
                    currentVersion.patch
                ));
            }
            else {
                LINFO(fmt::format(
                    "OpenSpace version {}.{}.{} is up to date.",
                    currentVersion.major,
                    currentVersion.minor,
                    currentVersion.patch
                ));
            }
            return true;
        }
        if (_request->hasFailed()) {
            _request->cancel();
            _request->wait();
            std::vector<char> data = _request->downloadedData();
            std::string response(data.begin(), data.end());
            LWARNING(fmt::format(
                "Failed to get latest OpenSpace version information from {}.",
                _request->url()
            ));
            _request = nullptr;
            return false;
        }
    }
    return false;
}

 VersionChecker::SemanticVersion VersionChecker::latestVersion() {
    return *_latestVersion;
}

bool operator<(const VersionChecker::SemanticVersion a,
               const VersionChecker::SemanticVersion b)
{
    if (a.major < b.major) {
        return true;
    }
    if (a.major > b.major) {
        return false;
    }

    if (a.minor < b.minor) {
        return true;
    }
    if (a.minor > b.minor) {
        return false;
    }

    return a.patch < b.patch;
}

} // namespace openspace
