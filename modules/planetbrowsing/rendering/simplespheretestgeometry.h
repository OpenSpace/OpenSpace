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

#ifndef __SIMPLESPHERETESTGEOMETRY_H__
#define __SIMPLESPHERETESTGEOMETRY_H__

#include <modules/planetbrowsing/rendering/planettestgeometry.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/scalarproperty.h>

namespace openspace {

	class RenderablePlanet;
	class PowerScaledSphere;

	namespace planettestgeometry {

		class SimpleSphereTestGeometry : public PlanetTestGeometry {
		public:
			SimpleSphereTestGeometry(const ghoul::Dictionary& dictionary);
			~SimpleSphereTestGeometry();


			bool initialize(RenderableTestPlanet* parent) override;
			void deinitialize() override;
			void render() override;
			PowerScaledSphere* _planet;

		private:
			void createSphere();

			glm::vec2 _modRadius;
			properties::Vec4Property _realRadius;
			properties::IntProperty _segments;
			std::string _name;

			PowerScaledSphere* _sphere;
		};

	}  // namespace planetgeometry
}  // namespace openspace

#endif // __SIMPLESPHERETESTGEOMETRY_H__
