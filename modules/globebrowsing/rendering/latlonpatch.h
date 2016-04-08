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

#ifndef __LATLONPATCH_H__
#define __LATLONPATCH_H__

#include <glm/glm.hpp>

// open space includes
#include <openspace/rendering/renderable.h>

#include <modules/globebrowsing/rendering/gridgeometry.h>

namespace ghoul {
namespace opengl {
	class ProgramObject;
}
}


namespace openspace {
	class LatLonPatch : public Renderable
	{
	public:
		LatLonPatch(
			unsigned int xRes,
			unsigned int yRes,
			double posLat,
			double posLon,
			double sizeLat,
			double sizeLon,
			double globeRadius);
		~LatLonPatch();

		bool initialize() override;
		bool deinitialize() override;
		bool isReady() const override;

		void render(const RenderData& data) override;
		void update(const UpdateData& data) override;

		void setPositionLatLon(glm::dvec2 posLatLon);
		void setSizeLatLon(glm::dvec2 posLatLon);
		void setGlobeRadius(double globeRadius);

		glm::dvec3 calculateCornerPointBottomLeft();
		glm::dvec3 calculateCornerPointBottomRight();
		glm::dvec3 calculateCornerPointTopLeft();
		glm::dvec3 calculateCornerPointTopRight();

		//!
		//! Finds a third control point between the two parameter points assuming
		//! they both lie on a sphere with the origin in the center.
		//! The three control points will then be used to interpolate NURBS curves
		//! that exactly describes circle segments.
		//! \param p0 is the first control point
		//! \param n0 is the normal corresponding to the first control point. The normal
		//! should point out from the center of the circle and be normalized
		//! \param p2 is the third control point
		//! \param n2 is the normal corresponding to the third control point. The normal
		//! should point out from the center of the circle and be normalized
		//!	\returns the second control point to be used for circle segment interpolation.
		//!
		glm::dvec3 calculateCenterPoint(glm::dvec3 p0, glm::dvec3 n0, glm::dvec3 p2, glm::dvec3 n2);
	private:
		std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

		glm::dvec2 _posLatLon;
		glm::dvec2 _sizeLatLon;
		double _globeRadius;
		GridGeometry _grid;
	};
}  // namespace openspace

#endif  // __LATLONPATCH_H__