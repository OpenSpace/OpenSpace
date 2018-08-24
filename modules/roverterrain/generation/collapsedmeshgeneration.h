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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN___COLLAPSED_MESH_GENERATION___H__
#define __OPENSPACE_MODULE_ROVER_TERRAIN___COLLAPSED_MESH_GENERATION___H__

#include <modules/roverterrain/filehandler/binaryreader.h>

#include <ghoul/misc/dictionary.h>

#include <pcl/point_cloud.h>
#include <pcl/point_types.h>

#include <vector>

// #include <pcl/TextureMesh.h>
// #include <pcl/point_cloud.h>
// #include <pcl/point_types.h>

namespace openspace {

class CollapsedMeshGeneration {
public:
    static void generateMeshFromBinaries(ghoul::Dictionary);

    static void extractCoordinatesFromArray(pcl::PointCloud<pcl::PointXYZ>::Ptr inputCloud, std::vector<std::vector<float>> xyz, BinaryReader::PointCloudInfo mInfo);

    static void writeTxtFile(const std::string filename, std::string output_path, BinaryReader::PointCloudInfo mInfo);

    static void writeMatrixFile(std::string output_path, glm::dmat4 outputMatrix);
private:

    // static double computeCloudResolution(pcl::PointCloud<pcl::PointXYZ>::Ptr inCloud);

    static std::string correctPath(const std::string filename, std::string output_path);

    int _levelOfDetail;
};

}

#endif //__OPENSPACE_MODULE_ROVER_TERRAIN___COLLAPSED_MESH_GENERATION___H__
