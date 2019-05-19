/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <sstream>

namespace {

} // namespace

namespace openspace {

std::string SemanticVersion::format() {
    return fmt::format("{}.{}.{}", major, minor, patch);
}

void VersionChecker::requestLatestVersion(const std::string& url) {
    HttpRequest::RequestOptions opt;
    opt.requestTimeoutSeconds = 0;

    const std::string fullUrl = url +
        "?client_version=" + OPENSPACE_VERSION_STRING_FULL +
        "&commit_hash=" + OPENSPACE_GIT_COMMIT;

    if (_request) {
        _request->cancel();
        _request->wait();
        _request = nullptr;
    }

    _request = std::make_unique<AsyncHttpMemoryDownload>(std::move(fullUrl));
    _request->start(opt);
}

 bool VersionChecker::hasLatestVersionInfo() {
    if (_latestVersion.has_value()) {
        return true; 
    }
    if (_request) {
        if (_request->hasSucceeded()) {
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
            return true;
        }
        if (_request->hasFailed()) {
            _request->cancel();
            _request->wait();
            _request = nullptr;
            return false;
        }
    }
    return false;
}

SemanticVersion VersionChecker::latestVersion() {
    return _latestVersion.value();
}

bool operator<(const SemanticVersion a, const SemanticVersion b) {
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
