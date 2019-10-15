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
#include <modules/solarbrowsing/tasks/helioviewerdownloadtask.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/httprequest.h>
#include <unordered_set>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/json.h>
#include <iostream>

namespace {
    constexpr const char* _loggerCat = "HelioviewerDownloadTask";

    constexpr const char* KeySourceId = "SourceId";
    constexpr const char* KeyName = "Name";
    constexpr const char* KeyInstrument = "Instrument";
    constexpr const char* KeyStartTime = "StartTime";
    constexpr const char* KeyTimeStep = "TimeStep";
    constexpr const char* KeyEndTime = "EndTime";
    constexpr const char* KeyOutputFolder = "OutputFolder";
    constexpr const char* KeyTimeKernel = "TimeKernel";
}

namespace openspace {

documentation::Documentation HelioviewerDownloadTask::documentation() {
    using namespace documentation;
    return {
        "HelioviewerDownloadTask",
        "helioviewer_download_task",
        {
            {
                "Type",
                new StringEqualVerifier("HelioviewerDownloadTask"),
                Optional::No,
                "The type of this task"
            },
            {
                KeyOutputFolder,
                new StringAnnotationVerifier("A folder on the local machine"),
                Optional::No,
                "The folder where to output the downloaded jp2 files"
            },
            {
                KeyName,
                new StringVerifier,
                Optional::No,
                "Name of the spacecraft or telescope"
            },
            {
                KeyInstrument,
                new StringVerifier,
                Optional::No,
                "Name of the intrument"
            },
            {
                KeyTimeStep,
                new DoubleAnnotationVerifier("A positive number"),
                Optional::No,
                "The preferred number of seconds between each timestep. "
                "The actual timestep will be determined by the availability of data "
                "products but will never be smaller than this number. Use this for "
                "temporal downsampling."
            },
            {
                KeyStartTime,
                new StringAnnotationVerifier("A date with format YYYY-MM-DDTHH:MM:SS"),
                Optional::No,
                "The beginning of the time interval to exteract data from"
            },
            {
                KeyEndTime,
                new StringAnnotationVerifier("A date with format YYYY-MM-DDTHH:MM:SS"),
                Optional::No,
                "The end of the time interval to exteract data from"
            },
            {
                KeySourceId,
                new IntVerifier,
                Optional::No,
                "The unique identifier as specified in "
                "https://api.helioviewer.org/docs/v2/#appendix"
            },
            {
                KeyTimeKernel,
                new StringAnnotationVerifier("A file path to a cdf file"),
                Optional::No,
                "A file path to a tls spice kernel used for time",
            },
        }
    };
}

HelioviewerDownloadTask::HelioviewerDownloadTask(const ghoul::Dictionary& dictionary) {
    _startTime = dictionary.value<std::string>(KeyStartTime);
    _endTime = dictionary.value<std::string>(KeyEndTime);
    _timeStep = dictionary.value<double>(KeyTimeStep);
    _sourceId = static_cast<int>(dictionary.value<double>(KeySourceId));
    _name = dictionary.value<std::string>(KeyName);
    _instrument = dictionary.value<std::string>(KeyInstrument);
    _outputFolder = dictionary.value<std::string>(KeyOutputFolder);
    _timeKernelPath = absPath(dictionary.value<std::string>(KeyTimeKernel));
}

std::string HelioviewerDownloadTask::description() {
    return "Download data from helioviewer.";
}

void HelioviewerDownloadTask::perform(const Task::ProgressCallback& progressCallback) {
    SpiceManager::ref().loadKernel(_timeKernelPath);
   
    const std::string jpxRequest =
        fmt::format("http://api.helioviewer.org/v2/getJPX/?startTime={}&endTime={}"
                    "&sourceId={}&verbose=true&cadence=true&cadence={}",
                    _startTime,
                    _endTime,
                    _sourceId,
                    _timeStep);

    LINFO(fmt::format("Requesting {}", jpxRequest));

    SyncHttpMemoryDownload fileListing(jpxRequest);
    const HttpRequest::RequestOptions opt = { 0 };

    fileListing.download(opt);
    if (!fileListing.hasSucceeded()) {
        LERROR(fmt::format("Request to Heliviewer API failed."));
    }

    const std::vector<char>& listingData = fileListing.downloadedData();
    const std::string listingString(listingData.begin(), listingData.end());

    std::vector<double> frames;
    try {
        nlohmann::json json = nlohmann::json::parse(listingString.c_str());
        const auto& framesIt = json.find("frames");
        if (framesIt == json.end()) {
            LERROR(fmt::format("Failed to acquire frames"));
        }

        nlohmann::json frameData = framesIt.value().get<nlohmann::json>();
        for (const auto& frame : frameData) {
            double epoch = frame.get<size_t>();
            frames.push_back(epoch);
        }
    }
    catch (...) {
        LERROR(fmt::format("Failed to parse json response: {}", listingString));
        return;
    }

    for (size_t i = 0; i < frames.size(); ++i) {
        const double epoch = frames[i];
        const double j2000InEpoch = 946684800.0;
        const Time time(epoch - j2000InEpoch);

        const std::string formattedDate = time.ISO8601();
        const std::string imageUrl = fmt::format(
            "http://api.helioviewer.org/v2/getJP2Image/?date={}Z&sourceId={}", 
            formattedDate,
            _sourceId
        );

        // Format file name according to solarbrowsing convention.
        const std::string year(formattedDate.begin(), formattedDate.begin() + 4);
        const std::string month(formattedDate.begin() + 5,  formattedDate.begin() + 7);
        const std::string day(formattedDate.begin() + 8, formattedDate.begin() + 10);
        const std::string hour(formattedDate.begin() + 11, formattedDate.begin() + 13);
        const std::string minute(formattedDate.begin() + 14, formattedDate.begin() + 16);
        const std::string second(formattedDate.begin() + 17, formattedDate.begin() + 19);
        const std::string millis(formattedDate.begin() + 20, formattedDate.begin() + 23);

        const std::string outFilename = fmt::format(
            "{}/{}_{}_{}__{}_{}_{}_{}__{}_{}.jp2",
            _outputFolder, 
            year,
            month,
            day,
            hour,
            minute,
            second,
            millis,
            _name,
            _instrument,
            static_cast<size_t>(epoch)
        );

        SyncHttpFileDownload imageDownload(imageUrl, absPath(outFilename));
        imageDownload.download(opt);
        if (!imageDownload.hasSucceeded()) {
            LERROR(fmt::format("Request to image {} failed.", imageUrl));
            continue;
        }

        progressCallback(static_cast<float>(i) / static_cast<float>(frames.size()));
    }
}

}
