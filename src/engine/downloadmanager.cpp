/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/engine/downloadmanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <fstream>

#ifdef OPENSPACE_CURL_ENABLED
#include <curl/curl.h>
#endif

namespace {
    const std::string _loggerCat = "DownloadManager";
    
    const std::string RequestIdentifier = "identifier";
    const std::string RequestFileVersion = "file_version";
    const std::string RequestApplicationVersion = "application_version";

    size_t writeData(void* ptr, size_t size, size_t nmemb, FILE* stream) {
        size_t written;
        written = fwrite(ptr, size, nmemb, stream);
        return written;
    }
}

namespace openspace {

DownloadManager::DownloadManager(std::string requestURL, int applicationVersion)
    : _requestURL(std::move(requestURL))
    , _applicationVersion(std::move(applicationVersion))
{
    curl_global_init(CURL_GLOBAL_ALL);
    // Check if URL is accessible
}

bool DownloadManager::downloadFile(
    const std::string& url,
    const ghoul::filesystem::File& file,
    DownloadFinishedCallback finishedCallback,
    DownloadProgressCallback progressCallback)
{
    CURL* curl = curl_easy_init();
    if (curl) {
        FILE* fp = fopen(file.path().c_str(), "wb");
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeData);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        LDEBUG("Starting download for file: '" << url <<
            "' into file '" << file.path() << "'");
        CURLcode res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);

        // TODO: incorporate progressCallback ---abock
        // http://curl.haxx.se/libcurl/c/progressfunc.html

        if (res != CURLE_OK) {
            LERROR("Error downloading file 'url': " << curl_easy_strerror(res));
            return false;
        }

        if (finishedCallback)
            finishedCallback(file);
        return true;
    }
}

bool DownloadManager::downloadRequestFiles(
    const std::string& identifier,
    const ghoul::filesystem::Directory& destination,
    int version,
    DownloadFinishedCallback finishedCallback,
    DownloadProgressCallback progressCallback)
{
    bool s = FileSys.createDirectory(destination, true);
    // TODO: Check s ---abock
    // TODO: Escaping is necessary ---abock
    const std::string fullRequest =_requestURL + "?" +
        RequestIdentifier + "=" + identifier + "&" +
        RequestFileVersion + "=" + std::to_string(version) + "&" +
        RequestApplicationVersion + "=" + std::to_string(_applicationVersion);
    LDEBUG("Request: " << fullRequest);

    std::string requestFile = absPath("${TEMPORARY}/" + identifier);
    LDEBUG("Request File: " << requestFile);

    bool success = downloadFile(
        fullRequest,
        requestFile,
        [destination](const ghoul::filesystem::File& f) {
            LDEBUG("Finished: " << f.path());
            std::ifstream temporary(f.path());
            std::string line;
            int nFiles = 0;
            int nFinished = 0;
            while (std::getline(temporary, line)) {
                ++nFiles;
                std::string file = ghoul::filesystem::File(line).filename();

                LDEBUG("\tLine: " << line << " ; Dest: " << destination.path() + "/" + file);
                bool success = DlManager.downloadFile(
                    line,
                    destination.path() + "/" + file,
                    [&nFinished](const ghoul::filesystem::File& f) { ++nFinished; }
                );
            }
        }
    );



    return true;
}


} // namespace openspace
