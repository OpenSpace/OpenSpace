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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___SUBSITE___H__
#define __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___SUBSITE___H__

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/roverterrainrenderer/filehandler/pointcloudinfo.h>

#include <stdint.h>
#include <string>
#include <vector>

namespace openspace {

struct Subsite {
    using SubsiteHashKey = uint64_t;

    // Site number, drive number and which frame the
    std::string site, drive, frame, sol;

    // Latitude and longitude for this subsite
    globebrowsing::Geodetic2 geodetic;

    // Latitude and longitude for the site that this subsite belongs to
    globebrowsing::Geodetic2 siteGeodetic;

    // Stores what levels are available for this subsite
    std::vector<int> availableLevels;

    // The file names of the .obj models and textures for this subsite
    std::vector<std::string> fileNames;

    // The file names of the .obj models and textures for this subsite
    std::vector<std::string> coloredTextureFileNames;

    // Information needed for texture projection
    std::vector<PointCloudInfo> cameraInfoVector;

    // Information needed for texture projection
    std::vector<PointCloudInfo> cameraColoredInfoVector;

    // Rotation matrix needed for placement of subsites
    glm::dmat4 rotationMatrix;

    std::string pathToGeometryFolder;
    std::string pathToTextureFolder;

    SubsiteHashKey hashKey(const int level) const;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___SUBSITE___H__
