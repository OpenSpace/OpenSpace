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

#include <modules/globebrowsing/models/singlemodelprovider.h>

#include <ghoul/logging/logmanager.h>

#include <ghoul/glm.h>

namespace {
	const std::string _loggerCat = "SingleModelProvider";
}

namespace openspace {
namespace globebrowsing {
	SingleModelProvider::SingleModelProvider(const ghoul::Dictionary& dictionary)
		: ModelProvider(dictionary) {
	}

	std::vector<Subsite> SingleModelProvider::calculate(const std::vector<std::vector<Subsite>> subsites) {
		std::vector<Subsite> ss;
		float minDist = 100000000;
		int minIdx1 = 0, minIdx2 = 0;
		Subsite smallest;
		glm::dvec2 cameraPos = glm::dvec2(-4.668293194, 137.370020299);
		Geodetic2 cameraPosOnSurface = Geodetic2{ -4.668266198 , 137.370017922 } / 180.0f * glm::pi<double>();

		for (auto s : subsites) {
			for (auto s1 : s) {
				glm::dvec2 temp = glm::dvec2(s1.lat, s1.lon);
				if (glm::distance(cameraPos, temp) < minDist) {
					minDist = glm::distance(cameraPos, temp);
					smallest = s1;
				}
			}
		}
		ss.push_back(smallest);
		return ss;
	}

	bool SingleModelProvider::initialize() {
		return true;
	}
}
}