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

#ifndef __DOWNLOADMANAGER_H__
#define __DOWNLOADMANAGER_H__

#include <ghoul/designpattern/singleton.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/directory.h>

#include <cstdint>
#include <functional>
#include <string>

namespace openspace {

// Multithreaded
class DownloadManager : public ghoul::Singleton<DownloadManager> {
public:
    struct FileFuture {
        FileFuture(std::string file);

        long long totalSize;
        float progress; // [0,1]
        bool isFinished;
        std::string filePath;
        std::string errorMessage;
    };

    typedef std::function<void(const FileFuture&)> DownloadProgressCallback;
    typedef std::function<void(const FileFuture&)> DownloadFinishedCallback;

    DownloadManager(std::string requestURL, int applicationVersion);

    // callers responsibility to delete
    FileFuture* downloadFile(
        const std::string& url,
        const ghoul::filesystem::File& file,
        DownloadFinishedCallback finishedCallback = DownloadFinishedCallback(),
        DownloadProgressCallback progressCallback = DownloadProgressCallback()
    );

    //bool downloadRequestFiles(
    //    const std::string& identifier,
    //    const ghoul::filesystem::Directory& destination,
    //    int version,
    //    DownloadFinishedCallback finishedCallback = DownloadFinishedCallback(),
    //    DownloadProgressCallback progressCallback = DownloadProgressCallback()
    //);

private:
    std::string _requestURL;
    int _applicationVersion;
};

#define DlManager (openspace::DownloadManager::ref())

} // namespace openspace

#endif // __DOWNLOADMANAGER_H__
