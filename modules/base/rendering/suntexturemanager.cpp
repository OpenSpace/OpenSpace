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



namespace {
    constexpr const char* _loggerCat = "SunTextureManager";
} // namespace


namespace openspace {

SunTextureManager::SunTextureManager(){
    
}
    void SunTextureManager::update(std::unique_ptr<ghoul::opengl::Texture>& texture){
        
        std::string current = getOpenSpaceDateTime();
        
        if(_counter == 200){ // first time
            std::string next = checkNextTextureId(current, 1);
            if(next != "Not found!"){
                startDownloadTexture(next);
                uploadTextureFromName(next);
            }
        }
        // check if there's a texture for the current timestamp (minute)
        else if((_activeTextureDate != current) && (_textureListGPU.find(current) != _textureListGPU.end()) ){
            LERROR("switching to texture with id: " + current);
            _textureListGPU[_activeTextureDate] = std::move(texture);
            texture = std::move(_textureListGPU[current]);
            _activeTextureDate = current;
            
            float dir = global::timeManager.deltaTime();
            std::string next = checkNextTextureId(current, dir);

            if(next != "Not found!" ){
                if (std::find(_textureListDisk.begin(), _textureListDisk.end(), next) == _textureListDisk.end()) {
                    startDownloadTexture(next);
                    uploadTextureFromName(next);
                }
                else {
                    LERROR("Shit already exists!");
                }
            }
        }
        _counter++;
    
}

    // not using this right now
    void SunTextureManager::initialDownloadBatch(){
        // check what files we have in the synd directory
        checkFilesInDirectory();
        
  
        std::string current = getOpenSpaceDateTime();
        //fetch a list of the textures we need, based on openspace time, from our server
        //this is a fake list of textures, which is a sub part of the file we have on disk. namely 500 of them
        std::vector<std::string> texturesNeeded;
        std::copy(_textureListDisk.begin() + 200, _textureListDisk.begin() + 600 + 1, std::back_inserter(texturesNeeded));
        
        //compare this list to the list of files we have.
        // we want to download the files that we don't have already
        // aka we don't want to download the files we have
        std::sort(_textureListDisk.begin(), _textureListDisk.end());
        std::sort(texturesNeeded.begin(), texturesNeeded.end());
        std::vector<std::string> texturesToDownload;
        std::set_difference(texturesNeeded.begin(), texturesNeeded.end(), _textureListDisk.begin(), _textureListDisk.end(),
                                         std::inserter(texturesToDownload, texturesToDownload.begin()));
        
        for(auto textureId : texturesToDownload){
            startDownloadTexture(textureId);
        }
        
    }
    
    void SunTextureManager::startDownloadTexture(std::string textureId){
        std::string url = "http://localhost:3000/get/" + textureId;
        //std::string destinationpath = absPath("../../../../../sync/magnetograms/" + textureId); //mac
        std::string destinationpath = absPath("../../../sync/magnetograms/" + textureId);
        AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, absPath(destinationpath), HttpFileDownload::Overwrite::Yes);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        ashd.start(opt);
        LERROR("nedladdning i startDownloadTexture");
        ashd.wait();
        LERROR("Texture " + textureId + " downloaded to disk" );
        
    }
    
    std::string SunTextureManager::checkNextTextureId(std::string current, float dir){
        
        SyncHttpMemoryDownload mmryDld = SyncHttpMemoryDownload("http://localhost:3000/getmenextfitsimage/" + current + "/" + std::to_string(dir));
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        mmryDld.download(opt);
        std::string s;
        std::transform(mmryDld.downloadedData().begin(), mmryDld.downloadedData().end(), std::back_inserter(s),
                       [](char c) {
                           return c;
                       });
        return s;
    }
    
    void SunTextureManager::startUploadTextures(){
        
        checkFilesInDirectory();
 
        uploadTexturesFromList(_textureListDisk);
        
    }
    
    // Scans directory and append all FILENAMES to the _textureListDisk member vector
    void SunTextureManager::checkFilesInDirectory() {
        //const std::string fitsDir = "../../../../../sync/magnetograms/";     //Mac
        const std::string fitsDir = "../../../sync/magnetograms/";            //PC

        // Creating a deep copy of the filevector, so we can do whatever we want with it
        std::vector<std::string> temp = ghoul::filesystem::Directory(fitsDir).readFiles();
        std::copy(temp.begin(), temp.end(), std::back_inserter(_textureListDisk));
        // To match both Mac and PC
        std::transform(begin(_textureListDisk), end(_textureListDisk), begin(_textureListDisk), [](std::string s) -> std::string { if (s.rfind("\\")) return s.substr(s.rfind("\\") + 1); else return s.substr(s.rfind("/") + 1); });

        //remove annoying mac file. might want to use a different strategy here
        _textureListDisk.erase(std::remove(_textureListDisk.begin(), _textureListDisk.end(), ".DS_STORE"), _textureListDisk.end());
    }


    void SunTextureManager::uploadTextureFromName(std::string filename){
        FitsFileReader fitsFileReader(false);
        
        std::string dateID ="";
        //const auto tempBild = fitsFileReader.readImageFloat("../../../../../sync/magnetograms/" + filename); // mac
        const auto tempBild = fitsFileReader.readImageFloat("../../../sync/magnetograms/" + filename);
        
        std::string dateID = parseMagnetogramDate(*fitsFileReader.readHeaderValueString("DATE"));
        
        const float stdvalue = *fitsFileReader.readHeaderValueFloat("IMGRMS01");
        std::vector<float> fitsImage;
        for(float c : tempBild->contents){
            fitsImage.push_back((c+stdvalue)/stdvalue);
        }

        LERROR("laddar upp texture till GPU med id: " + dateID);
        
        auto textureFits =  std::make_unique<ghoul::opengl::Texture>(fitsImage.data(), glm::vec3(360, 180, 1),ghoul::opengl::Texture::Format::Red, GL_R32F,GL_FLOAT);
        textureFits->uploadTexture();
        
        _textureQueueGPU.push(dateID);
        _textureListGPU[dateID] = std::move(textureFits);
        
        trimGPUList();
    }
    


    void SunTextureManager::uploadTexturesFromList(std::vector<std::string>& filelist){
        
        LERROR("antal filer: " + std::to_string(filelist.size()));
        for (const auto & entry : filelist) {
            uploadTextureFromName(entry);
            
        }
        LERROR("Laddat upp texturerna till GPU:n");

    }
    
    std::string SunTextureManager::getOpenSpaceDateTime(){
        std:: string datetime = global::timeManager.time().ISO8601();
        datetime.erase(4, 1);
        datetime.erase(6, 1);
        datetime.erase(8, 1);
        datetime.erase(10, 1);
        datetime.erase(12);
        return datetime;
    }
    
    std::string SunTextureManager::parseMagnetogramDate(std::string name){
        
        std::string dateID;
        int magicalCounter = 0;
        for (char c : name) {
            if (std::isdigit(c)) {
                if (magicalCounter >= 0 && magicalCounter < 12) {
                    dateID += c;
                }
                magicalCounter++;
            }
        }
        return dateID;
    }
    
    void SunTextureManager::trimGPUList(){
        if(_textureQueueGPU.size() > _maxTexturesOnGPU){
            
            std::string dateId = _textureQueueGPU.front();
            _textureQueueGPU.pop();
            LERROR("popped dateId : " + dateId);
            _textureListGPU.at(dateId).release();
            //BaseModule::TextureManager.release(_textureListGPU.at(dateId).get());
            _textureListGPU.erase(dateId);
        }
    }
    
}
