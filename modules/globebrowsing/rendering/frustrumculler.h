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

#ifndef __FRUSTRUMCULLER_H__
#define __FRUSTRUMCULLER_H__

#include <memory>
#include <glm/glm.hpp>

// open space includes

#include <openspace/rendering/renderable.h>

#include <modules/globebrowsing/datastructures/latlon.h>



namespace openspace {

	class LonLatPatch;
	using namespace glm;
	

	class FrustrumCuller {
	public:
		
		FrustrumCuller(float tolerance = 1.0f);
		~FrustrumCuller();
		
		bool isVisible(const vec3& point, const mat4x4& modelViewProjection);
		bool isVisible(const LatLonPatch& patch, double radius, const mat4x4& modelViewProjection);
		
		
	protected:
		float _tolerance;
	};


}  // namespace openspace

#endif  // __FRUSTRUMCULLER_H__