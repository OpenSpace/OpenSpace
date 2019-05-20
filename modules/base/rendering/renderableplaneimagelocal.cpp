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

#include <modules/base/rendering/renderableplaneimagelocal.h>

// Included for testing purposes
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <openspace/engine/globals.h>
#include <ghoul/opengl/framebufferobject.h>
#include <cctype>
#include <openspace/util/timemanager.h>
#include <openspace/util/httprequest.h>
#include <modules/sync/syncs/httpsynchronization.h>
// <- End of experimental includes

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/opengl/texture.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "RenderableImagePlaneLocal";
} // namespace

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square."
    };
    
    constexpr openspace::properties::Property::PropertyInfo RenderableTypeInfo = {
        "RenderableType",
        "RenderableType",
        "This value specifies if the plane should be rendered in the Background,"
        "Opaque, Transparent, or Overlay rendering step."
    };
} // namespace

namespace openspace {
    
    documentation::Documentation RenderablePlaneImageLocal::Documentation() {
        using namespace documentation;
        return {
            "Renderable Plane Image Local",
            "base_renderable_plane_image_local",
            {
                {
                    TextureInfo.identifier,
                    new StringVerifier,
                    Optional::No,
                    TextureInfo.description
                },
                {
                    RenderableTypeInfo.identifier,
                    new StringVerifier,
                    Optional::Yes,
                    RenderableTypeInfo.description
                }
            }
        };
    }
    
    RenderablePlaneImageLocal::RenderablePlaneImageLocal(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _texturePath(TextureInfo)
    {
        documentation::testSpecificationAndThrow(
                                                 Documentation(),
                                                 dictionary,
                                                 "RenderablePlaneImageLocal"
                                                 );
        
        _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));
        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
        
        addProperty(_texturePath);
        _texturePath.onChange([this]() {loadTexture(); });
        _textureFile->setCallback(
                                  [this](const ghoul::filesystem::File&) { _textureIsDirty = true; }
                                  );
        
        if (dictionary.hasKey(RenderableTypeInfo.identifier)) {
            std::string renderType = dictionary.value<std::string>(
                                                                   RenderableTypeInfo.identifier
                                                                   );
            if (renderType == "Background") {
                setRenderBin(Renderable::RenderBin::Background);
            } else if (renderType == "Opaque") {
                setRenderBin(Renderable::RenderBin::Opaque);
            }
            else if (renderType == "Transparent") {
                setRenderBin(Renderable::RenderBin::Transparent);
            }
            else if (renderType == "Overlay") {
                setRenderBin(Renderable::RenderBin::Overlay);
            }
        }
        else {
            setRenderBin(Renderable::RenderBin::Opaque);
        }
    }
    
    bool RenderablePlaneImageLocal::isReady() const {
        return RenderablePlane::isReady() && (_texture != nullptr);
    }
    
    void RenderablePlaneImageLocal::initializeGL() {
        RenderablePlane::initializeGL();
        
        loadTexture();
    }
    
    void RenderablePlaneImageLocal::deinitializeGL() {
        _textureFile = nullptr;
        
        BaseModule::TextureManager.release(_texture);
        RenderablePlane::deinitializeGL();
    }

    void RenderablePlaneImageLocal::bindTexture() {
        
        //_texture->bind();
        
        
        if(_texturePath.fullyQualifiedIdentifier() == "Scene.SunGlare.Renderable.Texture"){
            std::string current = global::timeManager.time().ISO8601();
            current.erase(4, 1);
            current.erase(6, 1);
            current.erase(8, 1);
            current.erase(10, 1);
            current.erase(12);
            
            if(_textureList.find(current) != _textureList.end()) _texture = _textureList[current].get();
            
            _counter++;
            
            
            if(_counter == 1000){
                LERROR(_texturePath);
                LERROR("just before thread starts");
                _dldthread = std::thread([this](){this->startDownloadTexture();});
                
            }
            //LERROR("just after thread starts");
            
            if(_counter == 1200){
                if(_dldthread.joinable()){
                    LERROR("it's joinable. counter: " + std::to_string(_counter));
                    _dldthread.join();
                    startUploadTexture();
                }
            }
            
            
        }
        
        
        _texture->bind();
        
    }

    void RenderablePlaneImageLocal::update(const UpdateData& data) {
        RenderablePlane::update(data);
        
        if (_textureIsDirty) {
            loadTexture();
            _textureIsDirty = false;
        }
    }
    
    void RenderablePlaneImageLocal::loadTexture() {
        if (!_texturePath.value().empty()) {
            ghoul::opengl::Texture* t = _texture;
            
            unsigned int hash = ghoul::hashCRC32File(_texturePath);
            
            
            
            // Given that the node-part is located just outside the openspace-directory
            const std::string fitsDir = "../../../../../../litebilder/";     //Mac
            //const std::string fitsDir = "../../../node/FITSdata/mrzqs190501/";            //PC
            
            //const std::string fitsDir = "../../../../../../httpdownload/FITSdata/";
            
            
            GLfloat fitsImage[360 * 180];
            
            // All the files in the given directory
            std::vector<std::string> fitsFiles = ghoul::filesystem::Directory(fitsDir).readFiles();
            std::sort(fitsFiles.begin(), fitsFiles.end());
            
            LERROR("antal filer: " + std::to_string(fitsFiles.size()));
            for (const auto & entry : fitsFiles) {
                LERROR(absPath(entry));
                
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
                
                const float minvalue = *fitsFileReader.readHeaderValueFloat("IMGMIN01");
                const float maxvalue = *fitsFileReader.readHeaderValueFloat("IMGMAX01");
                const float stdvalue = *fitsFileReader.readHeaderValueFloat("IMGRMS01");
                
                
                //                const int imageHeight = tempBild->height;
                //                const int imageWidth = tempBild->width;
                
                for (int i = 0; i < 360; i++) {
                    for (int j = 0; j < 180; j++) {
                        float color = tempBild->contents[(i * 180) + j];
                        //color = (color - minvalue) / (maxvalue - minvalue);
                        color = (color+stdvalue)/stdvalue; //some semirandom operation to actually se something in the texture
                        fitsImage[(i * 180) + j] = static_cast<GLfloat>(color);
                    }
                }
                
                
                LERROR(dateID + " pixelvärde på position 100 100: " + std::to_string(fitsImage[10000]));
                
                auto textureFits =  std::make_unique<ghoul::opengl::Texture>(fitsImage, glm::vec3(360, 180, 1),ghoul::opengl::Texture::Format::Red, GL_R32F,GL_FLOAT);
                textureFits->uploadTexture();
                
                LERROR(std::to_string(static_cast<int>(*textureFits)));
                
                
                _textureList[dateID] = std::move(textureFits);
                
            }
            _texture = _textureList.begin()->second.get();
            
            
            BaseModule::TextureManager.release(t);
            
            _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
            _textureFile->setCallback(
                                      [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
                                      );
        }
    }
    
    void RenderablePlaneImageLocal::startDownloadTexture(){
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
    
    void RenderablePlaneImageLocal::startUploadTexture(){
        FitsFileReader fitsFileReader(false);
        
        std::string dateID ="";
        std::string testpath = absPath("../../../../../../httpdownload/FITSdata/iam.fits.gz");
        const auto tempBild = fitsFileReader.readImageFloat(testpath);
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
        
        auto textureFits =  std::make_unique<ghoul::opengl::Texture>(fitsImage, glm::vec3(360, 180, 1),ghoul::opengl::Texture::Format::Red, GL_R32F,GL_FLOAT);
        textureFits->uploadTexture();
        
        LERROR("laddat ner och laddar upp: " + dateID);
        if(_textureList.find(dateID) != _textureList.end()){
            _textureList[dateID].release();
        }
        
        _textureList[dateID] = std::move(textureFits);
        
    }
    
} // namespace openspace
