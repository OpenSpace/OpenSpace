/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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
#include <modules/base/rendering/screenspaceimage.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem>

#include <openspace/engine/openspaceengine.h>

#include <modules/onscreengui/include/gui.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>

namespace {
    const std::string _loggerCat = "ScreenSpaceImage";
}

namespace openspace {
ScreenSpaceImage::ScreenSpaceImage(const ghoul::Dictionary& dictionary)
    :ScreenSpaceRenderable(dictionary)
    ,_texturePath("texturePath", "Texture path", "")
    ,_downloadImage(false)
{
    std::string name;
    if(dictionary.getValue("Name", name)){
        setName(name);
    }else{
        _id = id();
        setName("ScreenSpaceImage" + std::to_string(_id));
    }

    addProperty(_texturePath);
    registerProperties();

    std::string texturePath;
    bool texturesucces = (dictionary.getValue("TexturePath", texturePath));
    if(texturesucces){
        _texturePath.set(texturePath);
        OsEng.gui()._screenSpaceProperty.registerProperty(&_texturePath);    
        _texturePath.onChange([this](){ loadTexture(); });
    }

    bool urlsucces = dictionary.getValue("URL", _url);
    if(urlsucces){
        _downloadImage =true;
    }

    //screenspaceCygnet does not have url or texturePath
    // if(!texturesucces && !urlsucces){ 
    //     LERROR("Must specify TexturePath or URL");
    // }

}

ScreenSpaceImage::~ScreenSpaceImage(){}

bool ScreenSpaceImage::initialize(){
    _originalViewportSize = OsEng.windowWrapper().currentWindowResolution();

    createPlane();
    createShaders();
    updateTexture();

    return isReady();
}

bool ScreenSpaceImage::deinitialize(){
    unregisterProperties();

    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _texturePath = "";
    _texture = nullptr;

     RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    return true;
}

void ScreenSpaceImage::render(){
    if(!isReady()) return;
    if(!_enabled) return;

    glm::mat4 rotation = rotationMatrix();
    glm::mat4 translation = translationMatrix();
    glm::mat4 scale = scaleMatrix();
    glm::mat4 modelTransform = rotation*translation*scale;

    draw(modelTransform);
}


void ScreenSpaceImage::update(){
    if(_downloadImage && _futureImage.valid() && DownloadManager::futureReady(_futureImage)){
        loadTexture();
    }
}


bool ScreenSpaceImage::isReady() const{
    bool ready = true;
    if (!_shader)
        ready &= false;
    if(!_texture)
        ready &= false;
    return ready;
}

void ScreenSpaceImage::loadTexture() {
    std::unique_ptr<ghoul::opengl::Texture> texture = nullptr;
    if(!_downloadImage)
        texture = std::move(loadFromDisk());
    else
        texture = std::move(loadFromMemory());

    if (texture) {
        // LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
        texture->uploadTexture();

        // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
        texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

        _texture = std::move(texture);
    }
}

void ScreenSpaceImage::updateTexture(){
    if(!_downloadImage){
        loadTexture();
    } else {
        if(_futureImage.valid())
            return;

        std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(_url);
        if(future.valid()){
            _futureImage = std::move(future);
        }
    }
}

 std::unique_ptr<ghoul::opengl::Texture> ScreenSpaceImage::loadFromDisk(){
    if (_texturePath.value() != "")
        return (ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath.value())));
    return nullptr;
 }

 std::unique_ptr<ghoul::opengl::Texture> ScreenSpaceImage::loadFromMemory(){

    if(_futureImage.valid() && DownloadManager::futureReady(_futureImage) ){
        DownloadManager::MemoryFile imageFile = _futureImage.get();

        if(imageFile.corrupted)
            return nullptr;

        return (ghoul::io::TextureReader::ref().loadTexture(
            (void*) imageFile.buffer, 
            imageFile.size, 
            imageFile.format));

    }
 }

std::future<DownloadManager::MemoryFile> ScreenSpaceImage::downloadImageToMemory(std::string url){
    return std::move( DlManager.fetchFile(
            url,
            [url](const DownloadManager::MemoryFile& file){
                LDEBUG("Download to memory finished for screen space image");
            },
            [url](const std::string& err){
                LDEBUG("Download to memory failer for screen space image: " +err);
            }
        ) );
}

int ScreenSpaceImage::id(){
    static int id = 0;
    return id++;
}
}