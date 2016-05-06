// /*****************************************************************************************
//  *                                                                                       *
//  * OpenSpace                                                                             *
//  *                                                                                       *
//  * Copyright (c) 2014-2016                                                               *
//  *                                                                                       *
//  * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
//  * software and associated documentation files (the "Software"), to deal in the Software *
//  * without restriction, including without limitation the rights to use, copy, modify,    *
//  * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
//  * permit persons to whom the Software is furnished to do so, subject to the following   *
//  * conditions:                                                                           *
//  *                                                                                       *
//  * The above copyright notice and this permission notice shall be included in all copies *
//  * or substantial portions of the Software.                                              *
//  *                                                                                       *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
//  * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
//  * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
//  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
//  * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
//  * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
//  ****************************************************************************************/
#include <modules/iswa/rendering/textureplane.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
//#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/spicemanager.h>

namespace {
    const std::string _loggerCat = "TexutePlane";
}

namespace openspace {

TexturePlane::TexturePlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
{
    std::string name;
    dictionary.getValue("Name", name);
    setName(name);
    registerProperties();

    _type = ISWAManager::CygnetType::Texture;
}


TexturePlane::~TexturePlane(){}

bool TexturePlane::loadTexture() {

    DownloadManager::MemoryFile imageFile;
    try {
        imageFile = _futureObject.get();
    } catch( std::exception& e ) {
        LWARNING( "Textureplane futureImage exception: " + std::string(e.what()) );
        return false;
    }

    std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(
                                                        (void*) imageFile.buffer.c_str(),
                                                        imageFile.buffer.size(), 
                                                        imageFile.format);

    if (texture) {
        LDEBUG("Loaded texture from image iswa cygnet with id: '" << _data->id << "'");

        texture->uploadTexture();
        // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
        texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

        _textures[0]  = std::move(texture);
    }

    return false;
}



bool TexturePlane::updateTexture(){

    if(_textures.empty())
        _textures.push_back(nullptr);

    if(_futureObject.valid())
        return false;

    std::future<DownloadManager::MemoryFile> future = ISWAManager::ref().fetchImageCygnet(_data->id);

    if(future.valid()){
        _futureObject = std::move(future);
        return true;
    }

    return false;
}


bool TexturePlane::readyToRender(){
    return (isReady() && ((!_textures.empty()) && (_textures[0] != nullptr)));
}


bool TexturePlane::setUniformAndTextures(){
    ghoul::opengl::TextureUnit unit;

    unit.activate();
    _textures[0]->bind();
    _shader->setUniform("texture1", unit);

    return true;
}


bool TexturePlane::createShader(){
    if (_shader == nullptr) {
        // Plane Program
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("PlaneProgram",
            "${MODULE_ISWA}/shaders/cygnetplane_vs.glsl",
            "${MODULE_ISWA}/shaders/cygnetplane_fs.glsl"
            );
        if (!_shader) return false;
    }
    return true;
}

}// namespace openspace