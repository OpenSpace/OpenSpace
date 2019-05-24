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
    std::string current = global::timeManager.time().ISO8601();
    current.erase(4, 1);
    current.erase(6, 1);
    current.erase(8, 1);
    current.erase(10, 1);
    current.erase(12);
    
    if(_textureList.find(current) != _textureList.end()){
        _textureList[_activeTextureDate] = std::move(texture);
        texture = std::move(_textureList[current]);
        _activeTextureDate = current;
    }
    
    _counter++;
    
    if(_counter == 800){
        startUploadTexture();
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
    
    void SunTextureManager::startDownloadTexture(){
        std::string url = "http://localhost:3000/getmeafitsimage";
        std::string testpath = absPath("../../../../../../httpdownload/FITSdata/iam.fits.gz");
        AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, testpath, HttpFileDownload::Overwrite::Yes);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        ashd.start(opt);
        LERROR("nedladdning startad");
        ashd.wait();
        LERROR("efter wait");
        
    }
    
    void SunTextureManager::startUploadTexture(){
        // Given that the node-part is located just outside the openspace-directory
        const std::string fitsDir = "../../../../../../node/FITSdata/";     //Mac
        //const std::string fitsDir = "../../../node/FITSdata/";            //PC
        
        
        
        // All the files in the given directory
        std::vector<std::string> fitsFiles = ghoul::filesystem::Directory(fitsDir).readFiles();
        std::sort(fitsFiles.begin(), fitsFiles.end());
        
        LERROR("antal filer: " + std::to_string(fitsFiles.size()));
        for (const auto & entry : fitsFiles) {
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
            
            if(_textureList.find(dateID) != _textureList.end()){
                _textureList[dateID].release();
            }
            
            _textureList[dateID] = std::move(textureFits);
            
        }
        LERROR("Laddat upp texturerna till GPU:n");
//        _activeTextureDate = _textureList.begin()->first;
//        _texture.release();
//        _texture = std::move(_textureList.begin()->second);
        
        
    }

}
