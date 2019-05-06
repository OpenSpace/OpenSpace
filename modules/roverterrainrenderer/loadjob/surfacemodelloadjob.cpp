/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#include <modules/roverterrainrenderer/loadjob/surfacemodelloadjob.h>
#include <modules/base/rendering/multimodelgeometry.h>
#include <ghoul/io/texture/texturereader.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>

namespace {
    const std::string _loggerCat = "SurfaceModelLoadJob";
    const char* keyPathToTexture = "PathToTexture";
    const char* keyGeometryFile = "GeometryFile";
    const char* keyType = "Type";
}

namespace openspace {

void SurfaceModelLoadJob::execute() {
    std::string levelString = std::to_string(_level);
    std::string pathToGeometryFolder = _subsite->pathToGeometryFolder + "level" + levelString + "//" + "site" + _subsite->site +
    "//" + "drive" + _subsite->drive + "//";
    std::string pathToTextureFolder = _subsite->pathToTextureFolder;
    std::string roverSurfaceModelGeometry = "AsyncMultiModelGeometry";

    _subsiteModels->site = _subsite->site;
    _subsiteModels->drive = _subsite->drive;
    _subsiteModels->geodetic = _subsite->geodetic;
    _subsiteModels->siteGeodetic = _subsite->siteGeodetic;
    _subsiteModels->cameraInfoVector = _subsite->cameraInfoVector;
    _subsiteModels->level = _level;
    _subsiteModels->fileNames = _subsite->fileNames;
    _subsiteModels->coloredCameraInfoVector = _subsite->cameraColoredInfoVector;
    _subsiteModels->rotationMatrix = _subsite->rotationMatrix;

    ghoul::Dictionary dictionary;
    std::string pathToGeometry2 = pathToGeometryFolder + "OBJ.obj";
    dictionary.setValue(keyGeometryFile, pathToGeometry2);
    dictionary.setValue(keyType, roverSurfaceModelGeometry);

    _subsiteModels->model = std::make_shared<modelgeometry::AsyncMultiModelGeometry>(dictionary);

    glbinding::Binding::useCurrentContext();
    glbinding::Binding::initialize(global::windowDelegate.openGLProcedureAddress);

    for (auto fileName : _subsite->fileNames) {
        // Load all textures
        std::string tempFileName = fileName;
        tempFileName[13] = 'R';
        tempFileName[14] = 'A';
        tempFileName[15] = 'S';

        std::string textureFormat = SurfaceModelLoadJob::textureFormat(_subsite->site);
        std::string pathToTexture = pathToTextureFolder + "site" + _subsite->site +
        "/" + "drive" + _subsite->drive + "/" + tempFileName + textureFormat;
        _subsiteModels->textures.push_back(ghoul::io::TextureReader::ref().loadTexture(pathToTexture));
    }

    for (auto coloredFileName : _subsite->coloredTextureFileNames) {
        std::string pathToTexture = pathToTextureFolder + "site" + _subsite->site +
            "//" + "drive" + _subsite->drive + "//" + "colored//" + coloredFileName + ".jpg";

        _subsiteModels->coloredTextures.push_back(ghoul::io::TextureReader::ref().loadTexture(pathToTexture));
    }
}

std::shared_ptr<SubsiteModels> SurfaceModelLoadJob::product() {
    return _subsiteModels;
}

std::string SurfaceModelLoadJob::textureFormat(const std::string site) {
    int siteNumber = std::stoi(site);
    std::string textureFormat;
    if (siteNumber <= 21)
    textureFormat = ".jpg";
    else if (siteNumber > 21)
    textureFormat = ".png";

    return textureFormat;
}

} // openspace
