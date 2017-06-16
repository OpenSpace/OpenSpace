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

#ifndef __OPENSPACE___COLLAPSED_MESH_GENERATION_2___H__
#define __OPENSPACE___COLLAPSED_MESH_GENERATION_2___H__

#include <ghoul/misc/dictionary.h>

#include <modules/globebrowsing/tasks/imgreader.h>

#include <vector>

#include <pcl/TextureMesh.h>
#include <pcl/point_cloud.h>
#include <pcl/point_types.h>

namespace {
	const std::string _outputPath2 = "";
}

namespace openspace {
	namespace globebrowsing {
		class CollapsedMeshGeneration {
		public:
			
			static std::string correctPath(const std::string filename, std::string output_path);

			static void generateMeshFromBinary(ghoul::Dictionary);

			static void extractCoordinatesFromArray(pcl::PointCloud<pcl::PointXYZ>::Ptr inputCloud, std::vector<std::vector<float>> xyz, ImgReader::PointCloudInfo mInfo);

			static void writeTxtFile(const std::string filename, std::string output_path);

			static void writeMatrixFile(std::string output_path, glm::dmat4 outputMatrix);
		private:
			int _levelOfDetail;
		};
	}
}

#endif //__OPENSPACE___COLLAPSED_MESH_GENERATION_2___H__
