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

#include <modules/globebrowsing/models/subsite.h>
#include <modules/globebrowsing/geometry/geodetic2.h>

namespace openspace {
namespace globebrowsing {

Subsite::Subsite(const Geodetic2& subsitePoint, const Geodetic2& sitePoint,
	const std::string site, const std::string drive, const std::string frame,
		const std::vector<std::string> fileNames)
			: site(site), drive(drive), frame(frame), fileNames(fileNames)
{ 
	lat = subsitePoint.lat;
	lon = subsitePoint.lon;
	siteLat = sitePoint.lat;
	siteLon = sitePoint.lon;
}

Subsite::SubsiteHashKey Subsite::hashKey() const {
	SubsiteHashKey key = 0LL;
	int siteNumber = std::stoi(site);
	int driveNumber = std::stoi(drive);

	return SubsiteHashKey();
}

} // namespace globebrowsing
} // namespace openspace
