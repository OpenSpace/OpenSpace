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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___SUBSITE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___SUBSITE___H__

#include <stdint.h>
#include <string>
#include <vector>

namespace openspace {
namespace globebrowsing {

struct Geodetic2;

struct Subsite {
	using SubsiteHashKey = uint64_t;
	
	// Site number, drive number and which frame the 
	std::string site, drive, frame;

	// Latitude and longitude for this subsite
	double lat, lon;

	// Stores what levels are available for this subsite
	std::vector<int> availableLevels;

	// Latitude and longitude for the site that this subsite belongs to
	double siteLat, siteLon;

	// The file names of the .obj models and textures for this subsite
	std::vector<std::string> fileNames;

	std::string pathToGeometryFolder;
	std::string pathToTextureFolder;

	SubsiteHashKey hashKey(const int level) const;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___SUBSITE___H__
