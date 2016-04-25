/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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



#include <modules/globebrowsing/globes/clipmappyramid.h>

#define _USE_MATH_DEFINES
#include <math.h>

// ghoul includes
#include <ghoul/misc/assert.h>

namespace {
	const std::string _loggerCat = "ClipMapPyramid";
}

namespace openspace {
	ClipMapPyramid::ClipMapPyramid(Geodetic2 sizeLevel0)
		: _sizeLevel0(sizeLevel0)
	{

	}

	ClipMapPyramid::~ClipMapPyramid()
	{}

	Geodetic2 ClipMapPyramid::getPatchSizeAtLevel(int level)
	{
		return Geodetic2(_sizeLevel0.lat / pow(2, level), _sizeLevel0.lon / pow(2, level));
	}


}  // namespace openspace
