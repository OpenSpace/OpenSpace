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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LEVEL_OF_DETAIL___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LEVEL_OF_DETAIL___H__

#include <glm/glm.hpp>
#include <unordered_map>

namespace openspace {
namespace globebrowsing {

//Basic struct for LOD when generating meshes
static const struct LevelOfDetail {
  glm::dvec3 innerRadiusVoxelGridFilterLeafSize;
  glm::dvec3 middleRadiusVoxelGridFilterLeafSize;
  glm::dvec3 outerRadiusVoxelGridFilterLeafSize;
  double greedySearchRadius;
  double greedyMU;
  int greedyMaxNeighbours;
};

static const LevelOfDetail l1 = {
	glm::dvec3(0.2,0.2,0.2),
	glm::dvec3(0.2,0.2,0.2),
	glm::dvec3(0.5,0.5,0.5),
	2.0,
	2.5,
	750
};
static const LevelOfDetail l2 = {
	glm::dvec3(0.2,0.2,0.2),
	glm::dvec3(0.2,0.2,0.2),
	glm::dvec3(0.15,0.15,0.15),
	0.6,
	2.5,
	750
};

static const LevelOfDetail l3 = {
	glm::dvec3(0.02,0.02,0.02),
	glm::dvec3(0.05,0.05,0.05),
	glm::dvec3(0.15,0.15,0.15),
	//Organized
	//glm::dvec3(0.00001,0.00001,0.00001),
	0.4,
	2.5,
	750
};

typedef const std::unordered_map<std::string, LevelOfDetail> LevelOfDetailMap;

//Contains levels of details params that are predefined and proven to work for most pointclouds
static const LevelOfDetailMap _levelOfDetailMap = {
	{ "level1", l1 },
	{ "level2", l2 },
	{ "level3", l3 },
};

}
}

#endif //__OPENSPACE_MODULE_GLOBEBROWSING___LEVEL_OF_DETAIL___H__
