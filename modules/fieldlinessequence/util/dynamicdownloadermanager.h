/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___DYNAMICDOWNLOADERMANAGER___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___DYNAMICDOWNLOADERMANAGER___H__

#include <openspace/util/httprequest.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <map>
#include <string>
#include <vector>
#include <queue>

namespace openspace {

// Two things can be generalized here instead.
// 1. the options could/should be read from a json file hosted on a server
// 2. if things other than fieldlines are being supported,
// they could be added in another struct
struct FieldlineOption {
    // Question: What can I use instead of map to guarantee the value is also unique
    // These might only be useful as a dissplay name anyway
    const std::map<int, std::string> optionMap{
        // WSA 5.x GONG
        {1768, "WSA 5.X real"}, // - time output of the field line trace from the solar surface to the source surface using GONGZ as input
        {1769, "WSA 5.X real"}, // - time output of the field line trace from the source surface to the solar surface using GONGZ as input
        {1770, "WSA 5.X real"}, // - time output of the field line trace from the SCS outer boundary to the source surface using GONGZ as input
        {1771, "WSA 5.X real"}, // - time output of the field line trace from the Earth using GONGZ as input

        // WSA 4.4
        {1176, "trace_sun_earth"},
        {1177, "trace_scs_outtoin"},
        {1178, "trace_pfss_intoout"},
        {1179, "trace_pfss_outtoin"},
        //{1180, "WSA_OUT"},              // .fits

        // WSA 4.5
        {1192, "trace_sub_psp"},        // adapt
        //{1193, "trace_scs_outtoin"},    //same value as 1177        // adapt
        //{1194, "trace_pfss_intoout"},   //same value as 1178        // adapt
        //{1195, "trace_pfss_outtoin"},   //same value as 1179        // adapt
        {1196, "ADAPT_WSA_OUT"},        // adapt

        {1229, "trace_sun_earth"},      // 4.5 gong?
        {1230, "trace_scs_outtoin"},    // 4.5 gong?
        {1231, "trace_pfss_intoout"},   // 4.5 gong?
        {1232, "trace_pfss_outtoin"},   // 4.5 gong?
        //{1233, "WSA_OUT"},            // 4.5 gong? .fits file
        //{1234, "WSA_VEL_GONG"},       // 4.5 gong? .fits file
    };
    // Assumes the string is unique as well
    int id(std::string name) {
        //std::map<int, std::string>::const_iterator pos = optionMap.find(name);
        //if (pos == optionMap.end()) {
        //    LERROR("Did not find key dataID for input name");
        //    return;
        //}
        //else {
        //    return pos.first;
        //}
        for (auto it = optionMap.begin(); it != optionMap.end(); ++it) {
            if (it->second == name) {
                return it->first;
            }
        }
    }
    std::string optionName(int id) {
        std::string name;
        try {
            return optionMap.at(id);
        }
        catch (std::out_of_range& e) {
            return "";
        }
    }
};

//struct BigWindow {
//    //This is the big window. pair.first is timestep, pair.second is url to be downloaded
//    std::vector<std::pair<std::string, std::string>> listOfFiles;
//    double cadence;
//    std::pair<std::string, std::string>* whereInList;
//    std::string absoluteMin;
//    std::string absoluteMax;
//};

//struct Info {
//    HttpFileDownload* download;
//    enum class State {
//        Available,
//        OnQueue,
//        Downloading,
//        Downloaded
//    };
//    State state;
//    std::string timestep;
//    //double timestep;
//};

struct File {
    std::unique_ptr<HttpFileDownload> download;
    //pair.first is timestep, pair.second is url to be downloaded
    std::string timestep;
    std::string URL;
    double cadence;
    int availableIndex;
    enum class State {
        Available,
        OnQueue,
        Downloading,
        Downloaded
    };
    State state;
};


class DynamicDownloaderManager {
public:
    //DynamicDownloaderManager() = default;
    DynamicDownloaderManager(
        int dataID, const std::string infoURL, const std::string dataURL, int nOfFilesToQ
    );
    void requestDataInfo(std::string httpInfoRequest);
    void requestAvailableFiles(std::string httpDataRequest);
    //std::pair<std::string, std::string> findMostRelevantFileToDownload(const double time);
    //std::unique_ptr<HttpFileDownload>&
    //    findMostRelevantFileToPutOnQueue(const double time, const double deltaTime);
    //std::pair<std::string, std::string>* closestFileToNow(const double time);
    const File& closestFileToNow(const double time);
    void update(const double time, const double deltaTime);
    const std::vector<std::filesystem::path>& downloadedFiles();
    void checkForFinishedDownloads();
    void clearDownloaded();

private:

    void downloadFile();
    double calculateCadence();
    void prioritizeQueue(const double& time);

    //int _MaxNumberOfDownloadedFiles = 20;
    //bool _deltaTimeChanged = false;
    bool _forward = true;
    bool _firstFrame = true;

    std::filesystem::path _syncDir;
    std::pair<int, std::string> _dataID;
    const std::string _infoURL;
    const std::string _dataURL;
    //BigWindow _bigWindow;

    double _dataMinTime;
    double _dataMaxTime;
    std::string _dataIdDescription;
    //temporary having a global cadence. To be replaced with a cadence for each file.
    double _tempCadence = 0;
    const int _nOfFilesToQueue = 0;

    std::vector<File> _availableData;

    //std::vector<std::unique_ptr<HttpFileDownload>> _availableFiles;
    //std::priority_queue<std::unique_ptr<HttpFileDownload>> _queuedFilesToDownload;
    //std::vector<std::unique_ptr<HttpFileDownload>> _filesCurrentlyDownloading;
    ////std::vector<std::unique_ptr<HttpFileDownload>> _downloadedFiles;
    //std::vector<std::filesystem::path> _downloadedFiles;

    // Number 0 is highest priority in Queue. Higher number is lower prio
    std::vector<File*> _queuedFilesToDownload;
    bool _queueIsPrioritized = false;
    std::vector<File*> _filesCurrentlyDownloading;
    std::vector<std::filesystem::path> _downloadedFiles;


};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___DYNAMICDOWNLOADERMANAGER___H__
