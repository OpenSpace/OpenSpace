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

#ifndef __GLOBEMESH_H__
#define __GLOBEMESH_H__

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/util/updatestructures.h>

#include <modules/globebrowsing/meshes/trianglesoup.h>
#include <modules/globebrowsing/meshes/basicgrid.h>
#include <modules/globebrowsing/other/distanceswitch.h>

namespace ghoul {
	namespace opengl {
		class ProgramObject;
	}
}

namespace openspace {

	class GlobeMesh : public Renderable {
	public:
		GlobeMesh(const ghoul::Dictionary& dictionary);
		~GlobeMesh();

		bool initialize() override;
		bool deinitialize() override;
		bool isReady() const override;

		void render(const RenderData& data) override;
		void update(const UpdateData& data) override;

	private:
		std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

		//std::unique_ptr<Geometry> _testGeometry;
		BasicGrid _grid;
		

		properties::IntProperty _rotation;

		glm::dmat3 _stateMatrix;
		std::string _frame;
		std::string _target;
		double _time;
	};

}  // namespace openspace

#endif  // __GLOBEMESH_H__