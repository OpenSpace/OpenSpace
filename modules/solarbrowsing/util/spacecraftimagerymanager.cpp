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

#include <modules/solarbrowsing/util/spacecraftimagerymanager.h>
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <ghoul/opengl/texture.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem>

using namespace ghoul::opengl;

namespace {
    static const std::string _loggerCat = "SpacecraftImageryManager";
    static std::string _sampleImg = "https://sdo.gsfc.nasa.gov/assets/img/swpc/fitsfiles/0094/AIAsynoptic_20170316_113744_0094.fits";
    static const std::string _fitsUrl = "https://sdo.gsfc.nasa.gov/assets/img/swpc/fitsfiles/0094/fitsfile_list.txt";
    static const std::string baseUrl = "https://sdo.gsfc.nasa.gov/assets/img/swpc/fitsfiles/0094/";
    static std::map<std::string, std::unique_ptr<Texture>> imageFiles; // Spacecraft -> Imagebuffer
}

namespace openspace {

SpacecraftImageryManager::SpacecraftImageryManager() {
   //fetchServerImages("SDO");
}

void SpacecraftImageryManager::fetchServerImages(std::string type) {
    OsEng.downloadManager().fetchFile(
        _fitsUrl,
        [this, type](const DownloadManager::MemoryFile& file) {
            fillImageryInfo(file.buffer, type);
        }
    );
}

void SpacecraftImageryManager::fillImageryInfo(std::string buffer, std::string type) {
    std::string filename;
    std::istringstream iss(buffer);
    int count = 0;
    while (iss >> filename && count < 1) {
        OsEng.downloadManager().fetchFile(
            baseUrl + filename,
            [this, type](const DownloadManager::MemoryFile& file) {
                std::string c = file.buffer;
                imageFiles[type] = FitsFileReader::loadTextureFromMemory(c);
            }
        );
        count++;
    }
}

std::unique_ptr<Texture> SpacecraftImageryManager::getSpacecraftTexture(std::string& type) {
	//std::string texturePath = "1.fit";
	//std::string url = "https://sdo.gsfc.nasa.gov/assets/img/swpc/fitsfiles/0094/AIAsynoptic_20170315_211532_0094.fits";
	//fetchLatestImage(url);
    std::string s = "1.fit";
	return FitsFileReader::loadTexture(s);
}

/*
void SpacecraftImageryManager::fetchLatestImage(std::string& url){
	std::future<DownloadManager::MemoryFile> f = OsEng.downloadManager().fetchFile(
        url,
        [url](const DownloadManager::MemoryFile& file){
            LDEBUG("Download to memory finished for file with url: " + url);
        },
        [url](const std::string& err){
            LWARNING("Download to memory was aborted for file with url "+ url +": " + err);
        }
    );
    //std::string t = Time::ref().currentTimeUTC();
    std::string t2 = SpiceManager::ref().dateFromEphemerisTime(Time::ref().j2000Seconds());
	DownloadManager::MemoryFile res = f.get();
	std::cout << res.buffer << std::endl;
}*/

} //namespace openspace
