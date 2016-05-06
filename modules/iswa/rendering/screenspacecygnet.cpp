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

#include <modules/iswa/rendering/screenspacecygnet.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem>
#include <openspace/util/time.h>
#include <modules/iswa/util/iswamanager.h>

namespace {
    const std::string _loggerCat = "ScreenSpaceCygnet";
}

namespace openspace {

ScreenSpaceCygnet::ScreenSpaceCygnet(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _updateInterval("updateInterval", "Update Interval", 1.0, 0.0 , 10.0)
{
    // hacky, have to first get as float and then cast to int.
    float cygnetid;
    dictionary.getValue("CygnetId", cygnetid);
    _cygnetId = (int)cygnetid;
    
    setName("iSWACygnet" + std::to_string(_cygnetId));
    addProperty(_updateInterval);

    registerProperties();
}

ScreenSpaceCygnet::~ScreenSpaceCygnet(){}

bool ScreenSpaceCygnet::initialize(){
    _originalViewportSize = OsEng.windowWrapper().currentWindowResolution();
    createPlane();
    createShaders();
    updateTexture();

    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

    return isReady();
}

bool ScreenSpaceCygnet::deinitialize(){
    OsEng.gui()._screenSpaceProperty.unregisterProperties(name());

    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _texture = nullptr;

     RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    return true;
}

void ScreenSpaceCygnet::render(){

    if(!isReady()) return;
    if(!_enabled) return;

    glm::mat4 rotation = rotationMatrix();
    glm::mat4 translation = translationMatrix();
    glm::mat4 scale = scaleMatrix();
    glm::mat4 modelTransform = rotation*translation*scale;

    draw(modelTransform);
}

void ScreenSpaceCygnet::update(){
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    int updateInterval = (int) (_updateInterval.value()*1000);
    bool timeToUpdate = ((_realTime.count()-_lastUpdateRealTime.count()) > updateInterval) &&
                        (Time::ref().deltaTime() != 0);

    if(updateInterval != 0 && (Time::ref().timeJumped() || timeToUpdate )){
        updateTexture();
        _lastUpdateRealTime = _realTime;
    }

    if(_futureImage.valid() && DownloadManager::futureReady(_futureImage)) {
        loadTexture();
    }
}

bool ScreenSpaceCygnet::isReady() const{
    bool ready = true;
    if (!_shader)
        ready &= false;
    if(!_texture)
        ready &= false;
    return ready;
}

void ScreenSpaceCygnet::updateTexture(){

    if(_futureImage.valid())
        return;

    std::future<DownloadManager::MemoryFile> future = ISWAManager::ref().fetchImageCygnet(_cygnetId);
    if(future.valid()){
        _futureImage = std::move(future);
    }
}

void ScreenSpaceCygnet::loadTexture() {

    try {

        DownloadManager::MemoryFile imageFile = _futureImage.get();
        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(
            (void*) imageFile.buffer.c_str(),
            imageFile.buffer.size(), 
            imageFile.format);

        if (texture) {
            LDEBUG("Loaded texture from iswa cygnet with id: '" << _cygnetId << "'");

            texture->uploadTexture();
            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);
        }

    } catch( std::exception& e ) {
        LWARNING( "ScreenSpaceCygnet futureImage exception: " + std::string(e.what()) );
    }
}
}