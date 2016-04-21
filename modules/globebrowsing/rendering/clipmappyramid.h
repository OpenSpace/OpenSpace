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

#ifndef __CLIPMAPPYRAMID_H__
#define __CLIPMAPPYRAMID_H__

// open space includes
#include <modules/globebrowsing/datastructures/latlon.h>

namespace openspace {

	class ClipMapPyramid {
	public:
		/**
			\Param sizeLevel0 is the size of the biggest patch in the pyramid.
			The parameter needs to be M_PI / pow(2, i) where i is a positive or zero
			valued integer.
		*/
		ClipMapPyramid(LatLon sizeLevel0);
		~ClipMapPyramid();

		LatLon getPatchSizeAtLevel(int level);

	private:
		const LatLon _sizeLevel0;
	};

}  // namespace openspace

#endif  // __CLIPMAPPYRAMID_H__