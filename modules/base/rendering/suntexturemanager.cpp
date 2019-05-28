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


namespace {
    constexpr const char* _loggerCat = "SunTextureManager";
} // namespace


namespace openspace {

SunTextureManager::SunTextureManager(){

}

    void SunTextureManager::update(std::unique_ptr<ghoul::opengl::Texture>& texture){
        
        std::string current = getOpenSpaceDateTime();
    
        if(_textureListGPU.find(current) != _textureListGPU.end()){
            _textureListGPU[_activeTextureDate] = std::move(texture);
            texture = std::move(_textureListGPU[current]);
            _activeTextureDate = current;
            std::string next = checkNextTextureId(current);
            LERROR(next);
            //startDownloadTexture(next);
            
        }
        
        _counter++;
        
        if(_counter == 800){
            startUploadTextures();
            //LERROR(checkNextTextureId("201905111400"));
        }
        
        //        if(_counter == 1000){
        //            LERROR(_texturePath);
        //            LERROR("just before thread starts");
        //            _dldthread = std::thread([this](){this->startDownloadTexture();});
        //
        //        }
        //        //LERROR("just after thread starts");
        //
        //        if(_counter == 1200){
        //            if(_dldthread.joinable()){
        //                LERROR("it's joinable. counter: " + std::to_string(_counter));
        //                _dldthread.join();
        //                startUploadTexture();
        //            }
        //        }
    
}
    
    void SunTextureManager::initialDownload(){
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
        std::string url = "http://localhost:3000/getmeafitsimage";
        std::string destinationpath = absPath("../../../../../sync/magnetograms/" + textureId = ".fits.gz");
        AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, destinationpath, HttpFileDownload::Overwrite::Yes);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        ashd.start(opt);
        LERROR("nedladdning startad");
        ashd.wait();
        LERROR("efter wait");
        
    }
    
    std::string SunTextureManager::checkNextTextureId(std::string current){
        
        SyncHttpMemoryDownload mmryDld = SyncHttpMemoryDownload("http://localhost:3000/getmenextfitsimage/" + current + "/1");
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        mmryDld.download(opt);
        LERROR("nedladdning startad");
        std::string s;
        std::transform(mmryDld.downloadedData().begin(), mmryDld.downloadedData().end(), std::back_inserter(s),
                       [](char c) {
                           return c;
                       });
        return s;
        

    }
    
    void SunTextureManager::startUploadTextures(){
        
        checkFilesInDirectory();
        
        //std::sort(_textureListDisk.begin(), _textureListDisk.end());
        
        uploadTexturesFromList(_textureListDisk);
        
        
    }
    
    void SunTextureManager::checkFilesInDirectory(){
        const std::string fitsDir = "../../../../../sync/magnetograms/";     //Mac
        //const std::string fitsDir = "../../sync/magnetograms/";            //PC

        _textureListDisk = ghoul::filesystem::Directory(fitsDir).readFiles();
        //remove annoying mac file. might want to use a different strategy here
        _textureListDisk.erase(std::remove(_textureListDisk.begin(), _textureListDisk.end(), ".DS_STORE"), _textureListDisk.end());
        
    }
    
    void SunTextureManager::uploadTexturesFromList(std::vector<std::string>& filelist){
        
        LERROR("antal filer: " + std::to_string(filelist.size()));
        for (const auto & entry : filelist) {
            //LERROR(absPath(entry));
            
            FitsFileReader fitsFileReader(false);
            
            std::string dateID ="";
            const auto tempBild = fitsFileReader.readImageFloat(entry);
            //                const auto tempBild = fitsFileReader.readImageFloat(testpath);
            
            const std::string datestring = *fitsFileReader.readHeaderValueString("DATE");
            
            
            int magicalCounter = 0;
            for (char c : datestring) {
                if (std::isdigit(c)) {
                    if (magicalCounter >= 0 && magicalCounter < 12) {
                        dateID += c;
                    }
                    magicalCounter++;
                }
            }
            
            const float stdvalue = *fitsFileReader.readHeaderValueFloat("IMGRMS01");
            GLfloat fitsImage[360 * 180];
            for (int i = 0; i < 360; i++) {
                for (int j = 0; j < 180; j++) {
                    float color = tempBild->contents[(i * 180) + j];
                    color = (color+stdvalue)/stdvalue; //some semirandom operation to actually se something in the texture
                    fitsImage[(i * 180) + j] = static_cast<GLfloat>(color);
                }
            }
            
            //LERROR(dateID + " pixelvärde på position 100 100: " + std::to_string(fitsImage[10000]));
            
            auto textureFits =  std::make_unique<ghoul::opengl::Texture>(fitsImage, glm::vec3(360, 180, 1),ghoul::opengl::Texture::Format::Red, GL_R32F,GL_FLOAT);
            textureFits->uploadTexture();
            
            //LERROR(std::to_string(static_cast<int>(*textureFits)));
            
            if(_textureListGPU.find(dateID) != _textureListGPU.end()){
                _textureListGPU[dateID].release();
            }
            
            _textureListGPU[dateID] = std::move(textureFits);
            
        }
        LERROR("Laddat upp texturerna till GPU:n");
        //        _activeTextureDate = _textureList.begin()->first;
        //        _texture.release();
        //        _texture = std::move(_textureList.begin()->second);
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
    
    

}
