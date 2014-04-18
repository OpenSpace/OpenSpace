/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

// open space includes
#include <openspace/rendering/renderablevolume.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/engine/openspaceengine.h>
#include <sgct.h>

namespace {
    std::string _loggerCat = "RenderableVolume";
}

namespace openspace {

RenderableVolume::RenderableVolume(const ghoul::Dictionary& dictionary) {
    
    // get path if available
    _relativePath = "";
    if(dictionary.hasKey("Path")) {
       dictionary.getValue("Path", _relativePath);
       _relativePath += "/";
    }
}

RenderableVolume::~RenderableVolume() {
}

std::string RenderableVolume::findPath(const std::string& path) {
    std::string tmp = absPath(path);
    if(FileSys.fileExists(tmp))
        return tmp;
    
    tmp = absPath(_relativePath + path);
    if(FileSys.fileExists(tmp))
        return tmp;
    
    LERROR("Could not find file '" << path << "'");
    
    return "";
}

ghoul::RawVolumeReader::ReadHints RenderableVolume::readHints(const ghoul::Dictionary& dictionary) {
    ghoul::RawVolumeReader::ReadHints hints;
    hints._dimensions = glm::ivec3(1, 1, 1);
    hints._format = ghoul::opengl::Texture::Format::Red;
    hints._internalFormat = GL_R8;
    
    // parse hints
    double tempValue;
    if (dictionary.hasKey("Dimensions.1") && dictionary.getValue("Dimensions.1", tempValue)) {
        int intVal = static_cast<int>(tempValue);
        if(intVal > 0)
            hints._dimensions[0] = intVal;
    }
    if (dictionary.hasKey("Dimensions.2") && dictionary.getValue("Dimensions.2", tempValue)) {
        int intVal = static_cast<int>(tempValue);
        if(intVal > 0)
            hints._dimensions[1] = intVal;
    }
    if (dictionary.hasKey("Dimensions.3") && dictionary.getValue("Dimensions.3", tempValue)) {
        int intVal = static_cast<int>(tempValue);
        if(intVal > 0)
            hints._dimensions[2] = intVal;
    }
    
    std::string format;
    if (dictionary.hasKey("Format") && dictionary.getValue("Format", format)) {
        if(format == "RED") {
            hints._format = ghoul::opengl::Texture::Format::Red;
        } else if(format == "RG") {
            hints._format = ghoul::opengl::Texture::Format::RG;
        } else if(format == "RGB") {
            hints._format = ghoul::opengl::Texture::Format::RGB;
        } else if(format == "RGBA") {
            hints._format = ghoul::opengl::Texture::Format::RGBA;
        }
    }
    
    format = "";
    if (dictionary.hasKey("InternalFormat") && dictionary.getValue("InternalFormat", format)) {
        if(format == "R8") {
            hints._internalFormat = GL_R8;
        } else if(format == "RG8") {
            hints._internalFormat = GL_RG8;
        } else if(format == "RGB8") {
            hints._internalFormat = GL_RGB8;
        } else if(format == "RGBA8") {
            hints._internalFormat = GL_RGB8;
        } else if(format == "R32F") {
            hints._internalFormat = GL_R32F;
        } else if(format == "RG32F") {
            hints._internalFormat = GL_RG32F;
        } else if(format == "RGB32F") {
            hints._internalFormat = GL_RGB32F;
        } else if(format == "RGBA32F") {
            hints._internalFormat = GL_RGB32F;
        }
    }
    return hints;
}
    
ghoul::opengl::Texture* RenderableVolume::loadTransferFunction(const std::string& filepath) {
    if ( ! FileSys.fileExists(filepath)) {
        return nullptr;
    }
    
    return ghoul::opengl::loadTexture(filepath);
}

} // namespace openspace