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

#include <modules/globebrowsing/models/subsitemodels.h>

namespace openspace {
namespace globebrowsing {

float SubsiteModels::alpha() const {

	return _alpha;
}

void SubsiteModels::setFadeDirection(const int direction) {
	_fadeDirection = direction;
}

void SubsiteModels::fade() {
	if (_fadeDirection == 1 && _alpha < 1.0f)
		_alpha = _alpha + 0.01f;
	else if (_fadeDirection == -1 && _alpha > 0.0f)
		_alpha = _alpha - 0.01f;
}

SubsiteModels::SubsiteHashKey SubsiteModels::hashKey() const {
	uint64_t key = 0LL;
	int siteNumber = std::stoi(site);
	int driveNumber = std::stoi(drive);

	key |= level;
	key |= siteNumber << 5;
	key |= ((uint64_t)driveNumber) << 35;

	return key;
}

} // namespace globebrowsing
} // namespace openspace
