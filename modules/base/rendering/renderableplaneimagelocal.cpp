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
                TextureInfo.description,
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
    if(global::timeManager.isPaused()){

        glBindTexture(GL_TEXTURE_2D, *_textureList[_counter]);
        _counter2++;
        if(_counter2 == 50){
            _counter++;
            _counter2 = 0;
            
        }
        if(_counter == 23) _counter = 0;

        
    }else glBindTexture(GL_TEXTURE_2D, *_textureList[1]);

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
  const std::string fitsDir = "../../../../../../node/FITSdata/mrzqs190501/";     //Mac
  //const std::string fitsDir = "../../../node/FITSdata/mrzqs190501/";            //PC
  std::string testpath = absPath(fitsDir + "mrzqs190501t0014c2216_007.fits");
  LERROR(testpath);
  GLfloat fitsImage[360][180];
  
  // All the files in the given directory
  std::vector<std::string> fitsFiles = ghoul::filesystem::Directory(fitsDir).readFiles();
  LERROR("antal filer: " + std::to_string(fitsFiles.size()));
    for (const auto & entry : fitsFiles) {
        LERROR(entry);
        
        FitsFileReader fitsFileReader(false);

        std::string dateID ="";
        const auto tempBild = fitsFileReader.readImageFloat(entry);
        //                const auto tempBild = fitsFileReader.readImageFloat(testpath);

        int magicalCounter = -6;
        for (char c : entry) {
            if (std::isdigit(c)) {
                if (magicalCounter >= 0 && magicalCounter < 10) {
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

        float color;
        for (int i = 0; i < 360; i++) {
            for (int j = 0; j < 180; j++) {
                color = tempBild->contents[(i * 180) + j];
                color = (color - minvalue) / (maxvalue - minvalue);
                //color = (color+stdvalue)/stdvalue; //some semirandom operation to actually se something in the texture
                fitsImage[i][j] = static_cast<GLfloat>(color);
            }
        }


        LERROR(dateID + " pixelvärde på position 100 100: " + std::to_string(fitsImage[100][100]));

        auto textureFits =  std::make_unique<ghoul::opengl::Texture>(std::move(fitsImage), glm::vec3(360, 180, 1),ghoul::opengl::Texture::Format::Red, GL_R32F,GL_FLOAT);
        textureFits->uploadTexture();

        LERROR(std::to_string(static_cast<int>(*textureFits)));


        _textureList.push_back(std::move(textureFits));
        
    }
        _texture = _textureList[0].get();


        BaseModule::TextureManager.release(t);

        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
        _textureFile->setCallback(
            [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
        );
    }
}

} // namespace openspace
