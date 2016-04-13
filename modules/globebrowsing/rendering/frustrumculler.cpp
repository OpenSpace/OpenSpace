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


#include <modules/globebrowsing/rendering/frustrumculler.h>

namespace {
	const std::string _loggerCat = "FrustrumCuller";
}

namespace openspace {

	//////////////////////////////////////////////////////////////////////////////////////
	//							PATCH RENDERER											//
	//////////////////////////////////////////////////////////////////////////////////////
	FrustrumCuller::FrustrumCuller(float tolerance)
		: _tolerance(tolerance)
	{

	}

	FrustrumCuller::~FrustrumCuller() {
		
	}


	bool FrustrumCuller::isVisible(const vec3& point, const mat4x4& modelViewProjection) {
		vec4 pointModelSpace(point, 1.0f);
		vec4 pointProjectionSpace = modelViewProjection * pointModelSpace;
		vec2 pointScreenSpace = (1.0f / pointProjectionSpace.w) * pointProjectionSpace.xy;
		

		// just for readability
		const vec2& p = pointScreenSpace;
		return ((-_tolerance < p.x && p.x < _tolerance) &
				(-_tolerance < p.y && p.y < _tolerance));
		
	}

	bool FrustrumCuller::isVisible(const LatLonPatch& patch, double radius, const mat4x4& modelViewProjection) {
		return isVisible(radius * patch.northWestCorner().asUnitCartesian(), modelViewProjection)
			|| isVisible(radius * patch.northEastCorner().asUnitCartesian(), modelViewProjection)
			|| isVisible(radius * patch.southWestCorner().asUnitCartesian(), modelViewProjection)
			|| isVisible(radius * patch.southEastCorner().asUnitCartesian(), modelViewProjection);
	}
}  // namespace openspace
