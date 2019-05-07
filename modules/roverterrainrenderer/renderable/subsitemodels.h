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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___SUBSITEMODELS__H_
#define __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___SUBSITEMODELS__H_

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <modules/base/rendering/modelgeometry.h>
#include <modules/roverterrainrenderer/renderable/model.h>
#include <modules/roverterrainrenderer/opengl/texturearray.h>
#include <modules/roverterrainrenderer/filehandler/pointcloudinfo.h>

#include <memory>

struct Model;

namespace openspace {

struct SubsiteModels {
public:
    using SubsiteHashKey = uint64_t;

    std::vector<std::shared_ptr<Model>> models;

    std::vector <std::shared_ptr<ghoul::opengl::Texture>> textures;
    std::vector <std::shared_ptr<ghoul::opengl::Texture>> coloredTextures;
    std::shared_ptr<modelgeometry::AsyncMultiModelGeometry> model;
    std::shared_ptr<TextureArray> textureArray;
    std::shared_ptr<TextureArray> coloredTextureArray;
    glm::dmat4 rotationMatrix;


    glm::dvec3 cartesianPosition;

    globebrowsing::Geodetic2 geodetic;
    globebrowsing::Geodetic2 siteGeodetic;

    // The file names of the .obj models and textures for this subsite
    std::vector<std::string> fileNames;

    // Information needed for texture projection
    std::vector<PointCloudInfo> cameraInfoVector;

    // Information needed for colored texture projection
    std::vector<PointCloudInfo> coloredCameraInfoVector;

    GLuint textureID;
    GLuint coloredTextureID;

    uint64_t tileHashKey;
    std::string site;
    std::string drive;
    int level;

    float alpha() const;

    void setFadeDirection(const int direction);

    void fade();

    SubsiteModels::SubsiteHashKey hashKey() const;

private:
    int _fadeDirection = -1;
    float _alpha = 0.0f;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___SUBSITEMODELS__H_
