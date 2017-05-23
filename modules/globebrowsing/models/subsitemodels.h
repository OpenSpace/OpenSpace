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

#include <memory>

namespace openspace {
namespace globebrowsing {

struct SubsiteModels {
	using SubsiteHashKey = uint64_t;

	std::vector<std::shared_ptr<Model>> models;

	glm::dvec3 cartesianPosition;

	Geodetic2 subsiteCoordinate;
	Geodetic2 siteCoordinate;

	uint64_t tileHashKey;
	std::string site;
	std::string drive;
	int level;
	float _alpha;
	int _dir;

	bool status = false;

	SubsiteModels() {
		cartesianPosition = glm::dvec3(0.0, 0.0, 0.0);
		subsiteCoordinate = Geodetic2{ 0.0, 0.0 };
		siteCoordinate = Geodetic2{ 0.0, 0.0 };
		tileHashKey = 7367823;
		site = "0";
		drive = "0";
		level = 0;
		status = true;
		_alpha = 0.0f;
		_dir = 1;
	}
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___SUBSITEMODELS__H_