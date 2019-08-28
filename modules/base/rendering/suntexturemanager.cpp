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

#include <modules/base/rendering/suntexturemanager.h>

#include <openspace/util/timemanager.h>
#include <openspace/engine/globals.h>
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <openspace/util/httprequest.h>
#include <modules/sync/syncs/httpsynchronization.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <cctype>
#include <modules/base/basemodule.h>

namespace
{
constexpr const char *_loggerCat = "SunTextureManager";
} // namespace

namespace openspace
{

SunTextureManager::SunTextureManager()
{
    _syncDir = absPath("${BASE}/sync/magnetograms") + ghoul::filesystem::FileSystem::PathSeparator;
}
void SunTextureManager::update(std::unique_ptr<ghoul::opengl::Texture> &texture) {
    
    // If server is dead, we are not going to do anything
    if (_activeConnection){


            std::string currentTime = getOpenSpaceDateTime();


        if (_textureToUpload.empty() && currentTime != _activeTextureDate && (_textureListGPU.find(currentTime) != _textureListGPU.end()))
        {
            _textureToUpload = currentTime;
        }
        if ((global::timeManager.deltaTime() * _direction) < 0)
        {
            _textureToUpload = "";
        }

        _direction = global::timeManager.deltaTime();

        switch (_stage)
        {
            //This stage just checks what the next image applied should be,
        case 0:
        {
            _current = getOpenSpaceDateTime();

            if (!_textureToUpload.empty())
            {
                _stage = 3;
                break;
            }
            if (_counter % 150 == 0 && !_working && !_dldthread.joinable())
            {
                _dldthread = std::thread([=] { getNextTexture(_current, 1.0f, &_next); });
            }
            if (!_working && _dldthread.joinable())
            {
                _dldthread.join();
                if (_next != "Not found!")
                {
                    if (std::find(_textureListDisk.begin(), _textureListDisk.end(), _next) == _textureListDisk.end())
                    {
                        // We didn't find it on disk, proceed to download
                        _stage = 1;
                    }
                    else
                    {
                        if (_textureListGPU.find("20" + parseMagnetogramDate(_next).substr(0, 10)) == _textureListGPU.end())
                        {
                            // Found on disk but not on GPU, proceed to upload to GPU
                            _stage = 2;
                        }
                        else
                        {
                            // Found on disk, and on the GPU, all we wanted, all week was to swap the texture
                            _stage = 3;
                        }
                    }
                }
            }

            break;
        }
        case 1:
        {
            //LERROR("in case 1");
            if (!_working && !_dldthread.joinable())
            {
                _dldthread = std::thread([=] { downloadTexture(_next); });
            }

            if (!_working && _dldthread.joinable())
            {
                _dldthread.join();
                _textureListDisk.push_back(_next);
                _stage = 2;
            }
            break;
        }
        case 2:
        {

            if (!_working && !_dldthread.joinable())
            {
                _dldthread = std::thread([=] { processTextureFromName(_next, &_fitsImageToUpload, &_dateIDToUpload); });
            }

            if (!_working && _dldthread.joinable())
            {
                _dldthread.join();
                //LERROR(std::to_string(_textureListGPU.size()));
                uploadTexture(_fitsImageToUpload, _dateIDToUpload);
                _fitsImageToUpload.clear();
                _dateIDToUpload.clear();
                _stage = 3;
            }

            break;
        }
        case 3:
        {
            //LERROR("in case 3");
            if (((_textureListGPU.find(_textureToUpload) != _textureListGPU.end())))
            {
                _textureListGPU[_activeTextureDate] = std::move(texture);
                texture = std::move(_textureListGPU[_textureToUpload]);
                _activeTextureDate = _textureToUpload;
                _textureToUpload = "";
            }
            _stage = 0;

            break;
        }
        default:
            break;
        }

        _counter++;
    }
}

//  The same as startDownloadTexture, but uses lock_guard and flags _working
void SunTextureManager::downloadTexture(std::string textureId)
{
    _working = true;
    std::lock_guard<std::mutex> guard(_GPUListBlock);
    std::string url = "http://localhost:3000/get/" + textureId;
    std::string destinationpath = absPath(_syncDir + textureId);
    AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, destinationpath, HttpFileDownload::Overwrite::Yes);
    HttpRequest::RequestOptions opt = {};
    opt.requestTimeoutSeconds = 0;
    ashd.start(opt);
    ashd.wait();
    LERROR("Texture " + textureId + " downloaded to disk to " + destinationpath);
    _working = false;
}


void SunTextureManager::getNextTexture(std::string current, float dir, std::string *toReturn)
{
    _working = true;
    std::lock_guard<std::mutex> guard(_GPUListBlock);
    std::string url = "http://localhost:3000/getmenextfitsimage/" + current + "/" + std::to_string(dir);
    AsyncHttpMemoryDownload ashd = AsyncHttpMemoryDownload(url);
    HttpRequest::RequestOptions opt = {};
    opt.requestTimeoutSeconds = 0;
    ashd.start(opt);
    ashd.wait();
    std::string s;
    std::transform(ashd.downloadedData().begin(), ashd.downloadedData().end(), std::back_inserter(s),
                   [](char c) {
                       return c;
                   });
    LERROR("Next is " +  s );
    *toReturn = std::move(s);
    
    _working = false;
}



// Scans directory and append all FILENAMES to the _textureListDisk member vector
void SunTextureManager::checkFilesInDirectory()
{
    // We don't want to do anything if the server is not alive, flag it disconnected.
    if (checkServerAliveness()) {
        _activeConnection = true;
        // Creating a deep copy of the filevector, so we can do whatever we want with it
        std::vector<std::string> temp = ghoul::filesystem::Directory(_syncDir).readFiles();
        _textureListDisk.clear();
        std::copy(temp.begin(), temp.end(), std::back_inserter(_textureListDisk));
        // To match both Mac and PC
        std::transform(begin(_textureListDisk), end(_textureListDisk), begin(_textureListDisk), [](std::string s) -> std::string { if (s.find("\\") != std::string::npos) return s.substr(s.rfind("\\") + 1); else return s.substr(s.rfind("/") + 1); });

        //remove annoying mac file. might want to use a different strategy here
        _textureListDisk.erase(std::remove(_textureListDisk.begin(), _textureListDisk.end(), ".DS_STORE"), _textureListDisk.end());
    }
    else {
        LERROR("No connection to the magnetogram server could be made.");
    }
}

void SunTextureManager::uploadTexture(std::vector<float> imagedata, std::string id)
{
    LERROR("laddar upp texture till GPU med id: " + id);
    //auto textureFits = std::make_unique<ghoul::opengl::Texture>(std::move(imagedata.data()), glm::vec3(360, 180, 1), ghoul::opengl::Texture::Format::Red, GL_R32F, GL_FLOAT);
    auto textureFits = std::make_unique<ghoul::opengl::Texture>(std::move(imagedata.data()), glm::vec3(180, 90, 1), ghoul::opengl::Texture::Format::RGB, GL_RGB32F, GL_FLOAT);
    textureFits->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    textureFits->uploadTexture();
    //textureFits->setName(id);
    //textureFits->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
    _textureQueueGPU.push(id);
    _textureListGPU[id] = std::move(textureFits);
    trimGPUList();
    
}

void SunTextureManager::processTextureFromName(std::string filename, std::vector<float> *imagedata, std::string *id)
{
    _working = true;
    std::lock_guard<std::mutex> guard(_GPUListBlock);
    FitsFileReader fitsFileReader(false);
    
    // Since WSA has more HDUs than the primary one, we want to force use of the primary HDU,
    // so that fitsfilereader won't default to the extension HDUs.
    fitsFileReader.forceUsePHDU();

    const auto tempBild = fitsFileReader.readImageFloat(_syncDir + filename);
    //const auto tempBild = fitsFileReader.readImageFloat("/Users/shuy/Offline-dokument/fieldlines_python/WSA_OUT/wsa_201707010528R000_gong.fits");
    
    

    //*id = parseMagnetogramDate(*fitsFileReader.readHeaderValueString("DATE"));
    
    *id = parseMagnetogramDate(*fitsFileReader.readHeaderValueString("OBSTIME"));
    
    // FOR TESTING
    //*id = "2019-05-02T11:14:00";

    //const float minvalue = *fitsFileReader.readHeaderValueFloat("IMGMIN01");
    //const float maxvalue = *fitsFileReader.readHeaderValueFloat("IMGMAX01");
    //const int long0 = *fitsFileReader.readHeaderValueFloat("LONG0");
    //const float stdvalue = *fitsFileReader.readHeaderValueFloat("IMGRMS01");
    int long0 = *fitsFileReader.readHeaderValueFloat("CARRLONG");
    long0 = long0/2;
    
    //fitsFileReader.~FitsFileReader();
    
    
    
    /**** REGULAR NOMRALISATION *****/
//    float maxvalue = 0;
//    float minvalue = 17;
//    for (int i = 64800; i < 81000; i++)
//    {
//        float actual_value =tempBild->contents[i];
//        if(maxvalue < actual_value) maxvalue = actual_value;
//        if(minvalue > actual_value) minvalue = actual_value;
//    }
//    for (int i = 64800; i < 81000; i++)
//    {
//        float actual_value =tempBild->contents[i];
//        float c =(actual_value - minvalue) / (maxvalue - minvalue);
//
//        imagedata->push_back(c); // Normalized
//        //imagedata->push_back((c + stdvalue) / stdvalue); // Standard Deviation
//
//    }
    

    
    /**** REGULAR NOMRALISATION *****/
    float maxvalue = 0;
    float minvalue = 17;
    for (int i = 64800; i < 81000; i++)
    {
        float actual_value =tempBild->contents[i];
        if(maxvalue < actual_value) maxvalue = actual_value;
        if(minvalue > actual_value) minvalue = actual_value;
    }
    for (int i = 64800; i < 81000; i++)
    {
        
        float actual_value =tempBild->contents[i];
        float r, g, b;
        float norm_abs_value =  abs(actual_value)/maxvalue;
        
        if(actual_value == 0){
            r = 1.0f;
            b = 1.0f;
            g = 1.0f;
        }
        else if(actual_value > 0){
            r = 1.0f;
            g = 1 + log(1 - norm_abs_value);
            b = 1 + log(1 - norm_abs_value);
        }else {
            r = 1 + log(1 - norm_abs_value);
            g = 1 + log(1 - norm_abs_value);
            b = 1.0f;
        }
        
        imagedata->push_back(r);
        imagedata->push_back(g);
        imagedata->push_back(b);
        
        }

    
    
    
//    for (int i = 0; i < 90; i++)
//    {
//        std::rotate(imagedata->begin() + (i * 180), imagedata->begin() + ((i * 180) + (180 - long0)), imagedata->begin() + (i * 180) + 179);
//    }
    
    LERROR(std::to_string(imagedata->size()));

    _working = false;
}


std::string SunTextureManager::getOpenSpaceDateTime()
{
    std::string datetime = global::timeManager.time().ISO8601();
    datetime.erase(4, 1);
    datetime.erase(6, 1);
    datetime.erase(8, 1);
    datetime.erase(10, 1);
    datetime.erase(12);
    return datetime;
}

std::string SunTextureManager::parseMagnetogramDate(std::string name)
{

    std::string dateID;
    int magicalCounter = 0;
    for (char c : name)
    {
        if (std::isdigit(c))
        {
            if (magicalCounter >= 0 && magicalCounter < 12)
            {
                dateID += c;
            }
            magicalCounter++;
        }
    }
    return dateID;
}

bool SunTextureManager::checkServerAliveness()
{
    SyncHttpMemoryDownload mmryDld = SyncHttpMemoryDownload("http://localhost:3000/");
    HttpRequest::RequestOptions opt = {};
    opt.requestTimeoutSeconds = 0;
    mmryDld.download(opt);
    std::string s;
    std::transform(mmryDld.downloadedData().begin(), mmryDld.downloadedData().end(), std::back_inserter(s),
                   [](char c) {
                       return c;
                   });
    return s == "You are at ROOT";
}

void SunTextureManager::trimGPUList()
{
    if (_textureQueueGPU.size() > _maxTexturesOnGPU)
    {

        std::string dateId = _textureQueueGPU.front();
        _textureQueueGPU.pop();
        LERROR("popped dateId : " + dateId);
        //_textureListGPU.at(dateId).release(); // TODO: Kommentera tillbaka!
        //BaseModule::TextureManager.release(_textureListGPU.at(dateId).get());
        //_textureListGPU.erase(dateId);        // TODO: Kommentera tillbaka!
    }
}

} // namespace openspace
