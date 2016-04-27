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


#include <modules/globebrowsing/rendering/frustumculler.h>

namespace {
	const std::string _loggerCat = "FrustrumCuller";
}

namespace openspace {

	//////////////////////////////////////////////////////////////////////////////////////
	//							PATCH RENDERER											//
	//////////////////////////////////////////////////////////////////////////////////////
	FrustumCuller::FrustumCuller(){

	}

	FrustumCuller::~FrustumCuller() {
		
	}


	bool FrustumCuller::isVisible(const RenderData& data, const vec3& point, const glm::vec2& marginScreenSpace) {

		mat4 modelTransform = translate(mat4(1), data.position.vec3());
		mat4 viewTransform = data.camera.combinedViewMatrix();
		mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
			* viewTransform * modelTransform;

		vec2 pointScreenSpace = transformToScreenSpace(point, modelViewProjectionTransform);
		return testPoint(pointScreenSpace, marginScreenSpace);
	}


	bool FrustumCuller::isVisible(const RenderData& data, const GeodeticPatch& patch, double radius) {
		// An axis aligned bounding box based on the patch's minimum boudning sphere is
		// used for testnig
	
		// Calculate the MVP matrix
		mat4 modelTransform = translate(mat4(1), data.position.vec3());
		mat4 viewTransform = data.camera.combinedViewMatrix();
		mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
			* viewTransform * modelTransform;

		// Calculate the patch's center point in screen space
		vec4 patchCenterModelSpace = vec4(radius * patch.center().asUnitCartesian(), 1);
		vec4 patchCenterProjectionSpace = modelViewProjectionTransform * patchCenterModelSpace;
		vec2 pointScreenSpace = (1.0f / patchCenterProjectionSpace.w) * patchCenterProjectionSpace.xy();
		
		// Calculate the screen space margin that represents an axis aligned bounding 
		// box based on the patch's minimum boudning sphere
		double boundingRadius = radius * patch.minimalBoundingRadius();
		vec4 marginProjectionSpace = vec4(vec3(boundingRadius), 0) * data.camera.projectionMatrix();
		vec2 marginScreenSpace = (1.0f / patchCenterProjectionSpace.w) * marginProjectionSpace.xy();

		// Test the bounding box by testing the center point and the corresponding margin
		return testPoint(pointScreenSpace, marginScreenSpace);
	}





	bool FrustumCuller::testPoint(const glm::vec2& pointScreenSpace,
		const glm::vec2& marginScreenSpace) const 
	{
		
		const vec2& p = pointScreenSpace;

		vec2 cullBounds = vec2(1) + marginScreenSpace;
		return ((-cullBounds.x < p.x && p.x < cullBounds.x) &&
				(-cullBounds.y < p.y && p.y < cullBounds.y));
	}


	glm::vec2 FrustumCuller::transformToScreenSpace(const vec3& point, 
		const mat4x4& modelViewProjection) const 
	{
		vec4 pointProjectionSpace = modelViewProjection * vec4(point, 1.0f);
		vec2 pointScreenSpace = (1.0f / pointProjectionSpace.w) * pointProjectionSpace.xy();
		return pointScreenSpace;
	}


}  // namespace openspace
