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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___SUBSITEMODELS__H_
#define __OPENSPACE_MODULE_GLOBEBROWSING___SUBSITEMODELS__H_

#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/base/rendering/modelgeometry.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/models/model.h>
#include <modules/globebrowsing/tasks/imgreader.h>

#include <memory>

namespace openspace {
namespace globebrowsing {

struct SubsiteModels {
	using SubsiteHashKey = uint64_t;

	std::vector<std::shared_ptr<Model>> models;

	std::vector <std::shared_ptr<ghoul::opengl::Texture>> textures;
	std::shared_ptr<openspace::modelgeometry::AsyncMultiModelGeometry> model;

	glm::dvec3 cartesianPosition;

	Geodetic2 geodetic;
	Geodetic2 siteGeodetic;

	// The file names of the .obj models and textures for this subsite
	std::vector<std::string> fileNames;

	// Information needed for texture projection
	std::vector<ImgReader::PointCloudInfo> cameraInfoVector;

	GLuint textureID;

	uint64_t tileHashKey;
	std::string site;
	std::string drive;
	int level;
	float alpha = 0.0f;
	int _dir = -1;

	inline bool operator==(const SubsiteModels& other) const {
		return (site == other.site) && (drive == other.drive) && (level == other.level);
	}

	void fade() {
		if (_dir == 1 && alpha < 1.0f)
			alpha = alpha + 0.01f;
		else if (_dir == -1 && alpha > 0.0f)
			alpha = alpha - 0.01f;
	}
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___SUBSITEMODELS__H_