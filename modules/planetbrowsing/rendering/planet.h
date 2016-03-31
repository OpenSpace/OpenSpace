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

#ifndef __PLANET_H__
#define __PLANET_H__

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/util/updatestructures.h>

#include <modules/planetbrowsing/rendering/geometry.h>

// ghoul includes
namespace ghoul {
	namespace opengl {
		class ProgramObject;
		class Texture;
	}
}

namespace openspace {

	class Planet : public Renderable {
	public:
		Planet(const ghoul::Dictionary& dictionary);
		~Planet();

		bool initialize() override;
		bool deinitialize() override;
		bool isReady() const override;

		void render(const RenderData& data) override;
		void update(const UpdateData& data) override;

	protected:
		void loadTexture();

	private:
		properties::StringProperty _colorTexturePath;
		std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
		std::unique_ptr<ghoul::opengl::Texture> _texture;
		std::unique_ptr<ghoul::opengl::Texture> _nightTexture;

		std::unique_ptr<Geometry> _testGeometry;

		properties::BoolProperty _performShading;
		properties::IntProperty _rotation;
		float _alpha;

		glm::dmat3 _stateMatrix;
		std::string _nightTexturePath;
		std::string _frame;
		std::string _target;
		bool _hasNightTexture;
		double _time;
	};

}  // namespace openspace

#endif  // __PLANET_H__