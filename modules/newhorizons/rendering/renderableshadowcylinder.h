/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef RENDERABLESHADOWCYLINDER_H_
#define RENDERABLESHADOWCYLINDER_H_

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/spicemanager.h>

namespace ghoul {
	namespace filesystem {
		class File;
	}
	namespace opengl {
		class ProgramObject;
		class Texture;
	}
}

namespace openspace {
	struct LinePoint;
	class RenderableShadowCylinder : public Renderable {

	public:
		RenderableShadowCylinder(const ghoul::Dictionary& dictionary);
		~RenderableShadowCylinder();

		bool initialize() override;
		bool deinitialize() override;

		bool isReady() const override;

		void render(const RenderData& data) override;
		void update(const UpdateData& data) override;

	private:
		struct CylinderVBOLayout {
			CylinderVBOLayout(double a1, double a2, double a3, double a4){
				x = a1;
				y = a2;
				z = a3;
				e = a4;
			}
			float x, y, z, e;
		};

		void createCylinder();
		properties::IntProperty _numberOfPoints;
		properties::FloatProperty _shadowLength;

        std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
		
		glm::dmat3 _stateMatrix;

		GLuint _vao;
		GLuint _vbo;

		std::vector<CylinderVBOLayout> _vertices;

		std::string _terminatorType;
		std::string _lightSource;
		std::string _observer;
		std::string _body;
		std::string _bodyFrame;
		std::string _mainFrame;
        SpiceManager::AberrationCorrection _aberration;
		
		double _time;
	};

} // namespace openspace
#endif // RENDERABLESHADOWCYLINDER_H_

