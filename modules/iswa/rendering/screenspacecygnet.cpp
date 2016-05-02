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

ScreenSpaceCygnet::ScreenSpaceCygnet(int cygnetId)
    : ScreenSpaceRenderable()
    , _updateInterval("updateInterval", "Update Interval", 1.0, 0.0 , 10.0)
    , _cygnetId(cygnetId)
{
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
    OsEng.gui()._iSWAproperty.unregisterProperties(name());

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

    _memorybuffer = "";
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

    if(_futureTexture && _futureTexture->isFinished){
        loadTexture();
        _futureTexture = nullptr;
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
    _memorybuffer = "";

    std::shared_ptr<DownloadManager::FileFuture> future = ISWAManager::ref().downloadImageToMemory(_cygnetId, _memorybuffer);
    if(future){
        _futureTexture = future;
    }
}

void ScreenSpaceCygnet::loadTexture() {

    if(_memorybuffer != ""){

        std::string format;
        std::stringstream ss(_futureTexture->format);
        getline(ss, format ,'/');
        getline(ss, format);

        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(
            (void*) _memorybuffer.c_str(), 
            _memorybuffer.size(), 
            format);
        // std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_path));

        if (texture) {
            // LDEBUG("Loaded texture from '" << absPath(_path) << "'");

            texture->uploadTexture();
            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);
        }
    }
}
}