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

#include <modules/solarbrowsing/tasks/helioviewerdownloadtask.h>

#include <openspace/documentation/documentation.h>
#include <openspace/json.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <atomic>
#include <execution>
#include <format>
#include <ctime>
#include <mutex>
#include <sstream>

namespace {
    constexpr const char* _loggerCat = "HelioviewerDownloadTask";

    struct [[codegen::Dictionary(HelioviewerDownloadTask)]] Parameters {
        // The folder where to output the downloaded jp2 files.
        std::filesystem::path outputFolder [[codegen::directory()]];

        // Name of the spacecraft or telescope.
        std::string name;

        // Name of the instrument e.g., 'AIA-171', this name is used to lookup the
        // corresponding colormaps and as such must match the naming convetion.
        std::string instrument;

        // The preferred number of seconds between each timestep. The actual timestep will
        // be determined by the availability of data products but will never be smaller
        // than this number. Use this for  Use this for temporal downsampling.
        double timeStep;

        // The beginning of the time interval to extract data from. Format:
        // YYYY-MM-DDTHH:MM:SS
        std::string startTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // The end of the time interval to extract data from. Format YYYY-MM-DDTHH:MM:SS
        std::string endTime [[codegen::annotation("A valid date in ISO 8601 format")]];

        // The unique identifier as specified in the helioviewer documentation:
        // https://api.helioviewer.org/docs/v2/appendix/data_sources.html
        int sourceId;
    };
#include "helioviewerdownloadtask_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation HelioviewerDownloadTask::documentation() {
    return codegen::doc<Parameters>("helio_viewer_download_task");
}

HelioviewerDownloadTask::HelioviewerDownloadTask(const ghoul::Dictionary& dictionary) {

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _startTime = p.startTime;
    _endTime = p.endTime;
    _timeStep = p.timeStep;
    _sourceId = p.sourceId;
    _name = p.name;
    _instrument = p.instrument;
    _outputFolder = p.outputFolder;
}

std::string HelioviewerDownloadTask::description() {
    return "Download data from helioviewer.";
}

void HelioviewerDownloadTask::perform(const Task::ProgressCallback& progressCallback) {
    const std::string jpxRequest = std::format(
        "http://api.helioviewer.org/v2/getJPX/?startTime={}&endTime={}"
        "&sourceId={}&verbose=true&cadence=true&cadence={}",
        _startTime,
        _endTime,
        _sourceId,
        _timeStep
    );

    LINFO(std::format("Requesting {}", jpxRequest));

    HttpMemoryDownload fileListing(jpxRequest);
    fileListing.start();
    fileListing.wait();

    if (!fileListing.hasSucceeded()) {
        throw ghoul::RuntimeError(std::format("Request to Helioviewer API failed."));
    }

    LDEBUG("Fetching Helioviewer JPX data");
    const std::vector<char>& listingData = fileListing.downloadedData();
    const std::string listingString(listingData.begin(), listingData.end());

    std::vector<double> frames;
    try {
        nlohmann::json json = nlohmann::json::parse(listingString.c_str());
        frames = json["frames"].get<std::vector<double>>();

        if (frames.empty()) {
            LERROR(std::format("Failed to acquire frames"));
        }
    }
    catch (...) {
        LERROR(std::format("Failed to parse json response: {}", listingString));
        return;
    }

    LDEBUG("Processing frames");
    std::vector<std::string> epochAsIsoString;
    epochAsIsoString.reserve(frames.size());

    size_t count = 0;
    for (double epoch : frames) {
        const double j2000InEpoch = 946684800.0;
        const Time time(epoch - j2000InEpoch);

        epochAsIsoString.emplace_back(time.ISO8601());
        count++;
        progressCallback(static_cast<float>(count) / static_cast<float>(frames.size()));
    }

    std::atomic<size_t> i = 0;
    std::mutex progressMutex;
    const size_t totalFrames = epochAsIsoString.size();

    // TODO anden88 2026-02-12 can we flush so the output doesn't look lile this:
    // (D) HelioviewerDow..Task Downloading image data from Helioviewer=======>] 100 %
    LDEBUG("Downloading image data from Helioviewer               ");
    std::for_each(
        std::execution::par,
        epochAsIsoString.begin(),
        epochAsIsoString.end(),
        [&](const std::string& formattedDate) {

            const std::string imageUrl = std::format(
                "http://api.helioviewer.org/v2/getJP2Image/?date={}Z&sourceId={}",
                formattedDate,
                _sourceId
            );

            // Format file name according to solarbrowsing convention. Since we cannot save
            // files with ':' we have to destruct the ISO string and reconstruct it when
            // loading the image.
            std::istringstream ss = std::istringstream(std::string(formattedDate));

            std::tm tm = {};
            int milliseconds = 0;
            const std::string format = "%Y-%m-%dT%H:%M:%S";

            ss >> std::get_time(&tm, format.c_str());
            if (ss.peek() == '.') {
                ss.get();
                ss >> milliseconds;
            }

            const std::string outFilename = std::format(
                "{}/{:04}_{:02}_{:02}__{:02}_{:02}_{:02}_{:03}__{}_{}.jp2",
                _outputFolder,
                tm.tm_year + 1900,
                tm.tm_mon + 1,
                tm.tm_mday,
                tm.tm_hour,
                tm.tm_min,
                tm.tm_sec,
                milliseconds,
                _name,
                _instrument
            );

            // Skip existing files
            if (std::filesystem::exists(outFilename)) {
                size_t done = ++i;
                {
                    std::lock_guard lock(progressMutex);
                    progressCallback(static_cast<float>(done) / static_cast<float>(totalFrames));
                }
                return;
            }

            HttpFileDownload imageDownload(imageUrl, absPath(outFilename));
            imageDownload.start();
            imageDownload.wait();
            if (!imageDownload.hasSucceeded()) {
                LERROR(std::format("Request to image {} failed.", imageUrl));
                return;
            }
            size_t done = ++i;
            {
                std::lock_guard lock(progressMutex);
                progressCallback(static_cast<float>(done) / static_cast<float>(totalFrames));
            }
        }
    );
}

} // namespace openspace
