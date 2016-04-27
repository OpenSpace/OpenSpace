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

#include <modules/globebrowsing/geodetics/geodetic2.h>


namespace openspace {

	class LonLatPatch;
	using namespace glm;
	

	class FrustumCuller {
	public:
		
		FrustumCuller();
		~FrustumCuller();
		
		/**
			Returns true if the point is inside the view frustrum defined in RenderData. 
			The third argument marginScreenSpace is added to the default screen space 
			boundaries. E.g. for marginScreenSpace = {0.2, 0.2}, all points with
			1.2 < x,y < 1.2 would cause isVisible to return true.
		*/
		bool isVisible(
			const RenderData& data,
			const vec3& point,
			const vec2& marginScreenSpace = vec2(0));

		/**
			Returns false if the patch element is guaranteed to be outside the view 
			frustrum, and true is the patch element MAY be inside the view frustrum.
		*/
		bool isVisible(
			const RenderData& data,
			const GeodeticPatch& patch, 
			double radius);
		
	private:

		/**
			Returns true if the point in screen space is inside the view frustrum.
			The optional screen space margin vector is used to resize area defining 
			what is considered to be inside the view frustrum.
		*/
		bool testPoint(
			const glm::vec2& pointScreenSpace, 
			const glm::vec2& marginScreenSpace) const;


		glm::vec2 transformToScreenSpace(const vec3& point, const mat4x4& modelViewProjection) const;

	};


}  // namespace openspace

#endif  // __FRUSTRUMCULLER_H__