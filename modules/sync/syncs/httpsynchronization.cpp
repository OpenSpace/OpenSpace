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

namespace {
    const char* _loggerCat = "HttpSynchronization";
    const char* KeyIdentifier = "Identifier";
    const char* KeyVersion = "Version";

    const char* QueryKeyIdentifier = "identifier";
    const char* QueryKeyFileVersion = "file_version";
    const char* QueryKeyApplicationVersion = "application_version";
    const int ApplicationVersion = 1;
}

namespace openspace {

HttpSynchronization::HttpSynchronization(const ghoul::Dictionary& dict)
    : openspace::ResourceSynchronization()
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dict,
        "HttpSynchroniztion"
    );

    _identifier = dict.value<std::string>(KeyIdentifier);
    _version = static_cast<int>(dict.value<double>(KeyVersion));

    // Configure synchronization based on global settings in SyncModule 
    // TODO: For testability and decreaing deps, make it possible to inject this instead.
    // For example, allow this configuration to be done by the TemplateFactory.
    const SyncModule* syncModule = OsEng.moduleEngine().module<SyncModule>();
    _synchronizationRoot = syncModule->synchronizationRoot();
    _synchronizationRepositories = syncModule->httpSynchronizationRepositories();
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

void HttpSynchronization::synchronize() {
    if (hasSyncFile()) {
        resolve();
        return;
    }

    std::vector<std::string> listUrls = fileListUrls();
    for (const auto& url : listUrls) {
        if (trySyncFromUrl(url)) {
            resolve();
            return;
        }
    }
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

    HttpMemoryDownload fileListDownload(listUrl);
    fileListDownload.download(opt);

    const std::vector<char>& buffer = fileListDownload.downloadedData();

    std::istringstream fileList(std::string(buffer.begin(), buffer.end()));

    std::vector<std::thread> downloadThreads;
    std::string line = "";
    while (fileList >> line) {
        size_t lastSlash = line.find_last_of('/');
        std::string filename = line.substr(lastSlash + 1);
        
        std::string fileDestination = directory() +
        ghoul::filesystem::FileSystem::PathSeparator +
        filename;

        std::thread t([opt, line, fileDestination]() {
            HttpFileDownload fileDownload(line, fileDestination);
            fileDownload.download(opt);
        });
        downloadThreads.push_back(std::move(t));
    }
    for (auto& t : downloadThreads) {
        t.join();
    }
    createSyncFile();
    return true;
}

void HttpSynchronization::createSyncFile() {
    std::string dir = directory();
    std::string filepath = dir + ".ossync";
    FileSys.createDirectory(dir, ghoul::filesystem::Directory::Recursive::Yes);
    std::ofstream syncFile(filepath, std::ofstream::out);
    syncFile << "Synchronized";
    syncFile.close();
}

} // namespace openspace
