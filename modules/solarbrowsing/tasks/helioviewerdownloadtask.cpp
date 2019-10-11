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
                "The actual timestep will be determined by the availability of data 
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
                KeyDownloadUrl,
                new StringAnnotationVerifier(
                    "A string specifying the directory listings of files to download "
                    "`https://helioviewer.org/jp2/EUVI-A/${year}/${month}/${day}/195`"
                ),
                Optional::No,
                "The URL to download data from."
                "Use ${year}, ${month}, ${day}, ${hour}, ${minute}, ${second} "
                "and ${millisecond} as placeholders."
            },
            {
                KeyFilenames,
                new StringAnnotationVerifier(
                    "A valid filename string such as "
                    "${year}_${month}_${day}__${hour}_${minute}_${second}__"
                    "${millisecond}_STEREO-A-SECCHI_EUVI_195.jp2"
                ),
                Optional::No,
                "A string specifying the expected format of filenames on the server "
                "Use ${year}, ${month}, ${day}, ${hour}, ${minute}, ${second} "
                "and ${millisecond} as placeholders."   
            }
        }
    };
}

HelioviewerDownloadTask::HelioviewerDownloadTask(const ghoul::Dictionary& dictionary) {
    _startTime = Time(Time::convertTime(dictionary.value<std::string>(KeyStartTime)));
    _endTime = Time(Time::convertTime(dictionary.value<std::string>(KeyEndTime)));
    _timeStep = dictionary.value<double>(KeyTimeStep);
    _downloadUrl = dictionary.value<std::string>(KeyDownloadUrl);
    _filenames = dictionary.value<std::string>(KeyFilenames);
    _outputFolder = dictionary.value<std::string>(KeyOutputFolder);
}

std::string HelioviewerDownloadTask::description() {
    return "Download data from helioviewer.";
}

std::vector<std::string> HelioviewerDownloadTask::relevantDirectoryListings() const {
    std::vector<std::string> placeholders = {
        "${year}", "${month}", "${day}",
        "${hour}", "${minute}", "${second}", "${millisecond}"
    };

    std::vector<std::string> usedPlaceholders;
    for (const std::string& placeholder : placeholders) {
        const auto it =
            std::search(_downloadUrl.begin(), _downloadUrl.end(), placeholder);
        if (it != _downloadUrl.end()) {
            usedPlaceholders.push_back(placeholder);
        }
    }
    // @TODO: emiax, use
    // https://api.helioviewer.org/docs/v2/
    // for example:
    // https://api.helioviewer.org/v2/getJP2Image/
    // ?date=2014-01-01T23:59:59Z&sourceId=14&jpip=true
}

void HelioviewerDownloadTask::perform(const Task::ProgressCallback& progressCallback) {

    // iterate through times,
    // for each time, find the expected page.   
    //    Be able to fallback on previous pages, but know when to give up.
    //    In the page, find the best file to download.

    // get best filename from 
    
    std::unordered_set<std::string> availableFiles;
    std::vector<std::string> directoryListings = relevantDirectoryListings();




    SyncHttpMemoryDownload fileListing(
        "http://helioviewer.org/jp2/EUVI-A/2006/11/07/195/"
    );
    HttpRequest::RequestOptions opt = { 0 };
    fileListing.download(opt);
    if (fileListing.hasSucceeded()) {
        const std::vector<char>& data = fileListing.downloadedData();
        std::string s(data.begin(), data.end());
        std::cout << s;
    }
}

}



