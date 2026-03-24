/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#pragma once

#include <filesystem>
#include <string>
#include <unordered_set>
#include <vector>

namespace openspace {

    class DynamicHelioviewerImageDownloader {
    public:
        DynamicHelioviewerImageDownloader(
            std::filesystem::path outputFolder,
            std::string spacecraftName,
            int sourceId,
            std::string instrument,
            double cadenceSeconds,
            int numberOfFilesToQueue = 20
        );

        void update(double currentTimeJ2000, double deltaTime);
        const std::vector<std::filesystem::path>& downloadedFiles() const;
        void clearDownloaded();
        void deinitialize(bool saveDownloadsOnShutdown);

    private:
        std::vector<double> requestFrames(double beginJ2000, double endJ2000) const;
        std::filesystem::path downloadFrame(double unixTimestamp);
        std::filesystem::path expectedFilename(const std::string& isoTime) const;

        bool shouldRequestNewWindow(double currentTimeJ2000) const;

        std::string j2000ToIso(double j2000) const;
        std::string unixToIso(double unixTimestamp) const;

        std::filesystem::path _outputFolder;
        std::string _spacecraftName;
        int _sourceId = -1;
        std::string _instrument;
        double _cadenceSeconds = 3600.0;
        int _numberOfFilesToQueue = 20;

        double _requestWindowBefore = 12.0 * 3600.0;
        double _requestWindowAfter = 12.0 * 3600.0;
        double _minRequestShift = 1.0 * 3600.0;

        double _lastRequestedCenterTime = -1.0;

        std::vector<std::filesystem::path> _downloadedFiles;
        std::unordered_set<std::string> _knownFiles;
        std::vector<std::filesystem::path> _allDownloadedFiles;
    };

} // namespace openspace
