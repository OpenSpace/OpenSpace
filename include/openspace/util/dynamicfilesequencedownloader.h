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

#ifndef __OPENSPACE_MODULE_SYNC___DYNAMICFILESEQUENCEDOWNLOADER___H__
#define __OPENSPACE_MODULE_SYNC___DYNAMICFILESEQUENCEDOWNLOADER___H__

#include <openspace/util/httprequest.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <map>
#include <string>
#include <unordered_map>
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
    // Answe: Using two identical unordered map instead, except swaped key and value
    // This with a constructor with insert function, adds list to the unordered maps.
    // The vector pairs will later be replaced by a document list supplied from the server
    // as a TODO.
    std::unordered_map<int, std::string> _keyToValue;
    std::unordered_map<std::string, int> _valueToKey;
    const std::vector<std::pair<int, std::string>> pairs = {
        // WSA 5.4  GONG
        {2284,"GONG_Z/trace_pfss_intoout"}, //GONG_Z/trace_pfss_intoout
        {2285,"GONG_Z/trace_pfss_outtoin"}, //GONG_Z/trace_pfss_outtoin
        {2286,"GONG_Z/trace_scs_outtoin"}, //GONG_Z/trace_scs_outtoin
        {2287,"GONG_Z/trace_sun_earth"}, //GONG_Z/trace_sun_earth

        {2148, "GONG WSA 5.4 output fits file using GONG-Z Map (RADOUT =  5.0)"},

        // WSA 5.x GONG
        {1768, "WSA 5.X real"}, // - time output of the field line trace from the solar surface to the source surface using GONGZ as input
        {1769, "WSA 5.X real"}, // - time output of the field line trace from the source surface to the solar surface using GONGZ as input
        {1770, "WSA 5.X real"}, // - time output of the field line trace from the SCS outer boundary to the source surface using GONGZ as input
        {1771, "WSA 5.X real"}, // - time output of the field line trace from the Earth using GONGZ as input

        {1229, "trace_sun_earth"},      // 4.5 gong?
        {1230, "trace_scs_outtoin"},    // 4.5 gong?
        {1231, "trace_pfss_intoout"},   // 4.5 gong?
        {1232, "trace_pfss_outtoin"},   // 4.5 gong?
        //{1233, "WSA_OUT"},            // 4.5 gong? .fits file
        //{1234, "WSA_VEL_GONG"},       // 4.5 gong? .fits file

        // WSA 4.5
        {1192, "trace_sub_psp"},        // adapt
        //{1193, "trace_scs_outtoin"},    //same value as 1177        // adapt
        //{1194, "trace_pfss_intoout"},   //same value as 1178        // adapt
        //{1195, "trace_pfss_outtoin"},   //same value as 1179        // adapt
        {1196, "ADAPT_WSA_OUT"},        // adapt fits files.

        // WSA 4.4
        {1176, "trace_sun_earth"},
        {1177, "trace_scs_outtoin"},
        {1178, "trace_pfss_intoout"},
        {1179, "trace_pfss_outtoin"},
        //{1180, "WSA_OUT"},              // .fits
    };

    FieldlineOption() {
        for (const auto& pair : pairs) {
            insert(pair.first, pair.second);
        }
    }

    void insert(int key, std::string value) {
        _keyToValue[key] = value;
        _valueToKey[value] = key;
    }

    int id(std::string value) {
        auto it = _valueToKey.find(value);
        return (it != _valueToKey.end()) ? it->second : 0;
    }

    std::string optionName(int key) {
        auto it = _keyToValue.find(key);
        return (it != _keyToValue.end()) ? it->second : "";
    }
};

struct File {
    std::unique_ptr<HttpFileDownload> download;
    std::string timestep;
    //TODO: need both times?
    double time;
    std::string URL;
    double cadence;
    //TODO: need Cadence?
    int availableIndex;
    enum class State {
        Available,
        OnQueue,
        Downloading,
        Downloaded
    };
    State state;
};

class DynamicFileSequenceDownloader {
public:
    DynamicFileSequenceDownloader(
        int dataID, const std::string infoURL, const std::string dataURL, int nOfFilesToQ
    );
    void requestDataInfo(std::string httpInfoRequest);
    void requestAvailableFiles(std::string httpDataRequest, std::filesystem::path syncdir);
    std::vector<File>::iterator closestFileToNow(const double time); //const File&
    void update(const double time, const double deltaTime);
    const std::vector<std::filesystem::path>& downloadedFiles();
    void checkForFinishedDownloads();
    void clearDownloaded();

private:

    void downloadFile();
    double calculateCadence();
    void putOnQueue();
    bool _forward = true;
    bool _firstFrame = true;
    bool _secondFrame = true;

    std::filesystem::path _syncDir;
    std::pair<int, std::string> _dataID;
    const std::string _infoURL;
    const std::string _dataURL;

    double _dataMinTime;
    double _dataMaxTime;
    //std::string _dataIdDescription;
    //Currently have both a global cadence and it's assigned to every element too.
    double _tempCadence = 0;
    const int _nOfFilesToQueue = 0;

    //to iterate window
    std::vector<File>::iterator _thisFile;

    std::vector<File> _availableData;
    std::vector<File*> _queuedFilesToDownload;
    std::vector<File*> _filesCurrentlyDownloading;
    std::vector<std::filesystem::path> _downloadedFiles;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___DYNAMICFILESEQUENCEDOWNLOADER___H__
