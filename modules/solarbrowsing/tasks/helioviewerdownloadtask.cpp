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
#include <openspace/util/httprequest.h>
#include <unordered_set>
#include <iostream>

namespace {
    constexpr const char* _loggerCat = "HelioviewerDownloadTask";

    constexpr const char* KeyDownloadUrl = "DownloadUrl";
    constexpr const char* KeyFilenames = "Filenames";
    constexpr const char* KeyStartTime = "StartTime";
    constexpr const char* KeyTimeStep = "TimeStep";
    constexpr const char* KeyEndTime = "EndTime";
    constexpr const char* KeyOutputFolder = "OutputFolder";
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
            }
        }
    };
}

HelioviewerDownloadTask::HelioviewerDownloadTask(const ghoul::Dictionary& dictionary) {
    _startTime = dictionary.value<std::string>(KeyStartTime);
    _endTime = dictionary.value<std::string>(KeyEndTime);
    _timeStep = dictionary.value<double>(KeyTimeStep);
    _sourceId = static_cast<int>(dictionary.value<double>(KeySourceId));
    _outputFolder = dictionary.value<std::string>(KeyOutputFolder);
}

std::string HelioviewerDownloadTask::description() {
    return "Download data from helioviewer.";
}

void HelioviewerDownloadTask::perform(const Task::ProgressCallback& progressCallback) {

    // iterate through times,
    // for each time, find the expected page.   
    //    Be able to fallback on previous pages, but know when to give up.
    //    In the page, find the best file to download.

    // get best filename from 
    
    std::unordered_set<std::string> availableFiles;

    std::string jpxRequest =
        fmt::format("http://api.helioviewer.org/v2/getJPX/?startTime={}&endTime={}"
                    "&sourceId={}&verbose=true&cadence=true&cadence={}",
                    _startTime,
                    _endTime,
                    _sourceId,
                    _timeStep);

    SyncHttpMemoryDownload fileListing(jpxRequest);
    HttpRequest::RequestOptions opt = { 0 };
    fileListing.download(opt);
    if (!fileListing.hasSucceeded()) {
        LERROR(fmt::format("Request to Heliviewer API failed."));
    }

    const std::vector<char>& data = fileListing.downloadedData();
    std::string str(data.begin(), data.end());
    nlohmann::json json = nlohmann::json::parse(str.c_str());

    // https://api.helioviewer.org/v2/getJP2Header/?id=7654321
    // extract time stamp from DATE_OBS
    // request jp2 separately based on timestamp.
    // beware of 1000 image max limit.

    // std::cout << str << std::endl;
/*
    const auto& framesIt = json.find("frames");
    if (framesIt == json.end()) {
        LERROR(fmt::format("Failed to acquire frames"));
    }

    nlohmann::json frames = framesIt.value().get<nlohmann::json>();
    for (const auto& frame : frames) {
        int index = frame.get<int>();
        LINFO(fmt::format("{}", index));
    }
 */
}

}



