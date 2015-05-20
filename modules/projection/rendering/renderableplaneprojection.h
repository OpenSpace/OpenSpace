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

#ifndef _RENDERABLEPLANEPROJECTION_H_
#define _RENDERABLEPLANEPROJECTION_H_

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/util/imagesequencer2.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/util/updatestructures.h>

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

struct target {
	std::string body;
	std::string frame;
	std::string node;
};

class RenderablePlaneProjection : public Renderable {


public:
	RenderablePlaneProjection(const ghoul::Dictionary& dictionary);
	~RenderablePlaneProjection();

	bool initialize() override;
	bool deinitialize() override;

	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

private:
	void loadTexture();
	void updatePlane(const Image img, double currentTime);
	std::string findClosestTarget(double currentTime);
	void setTarget(std::string body);

	std::string _texturePath;
		
	bool _planeIsDirty;

	glm::dmat3 _stateMatrix;
	std::string _frame;

	ghoul::opengl::ProgramObject* _shader;
	bool _textureIsDirty;
	ghoul::opengl::Texture* _texture;
	ghoul::filesystem::File* _textureFile;
	GLuint _quad;
	GLuint _vertexPositionBuffer;
	std::string _spacecraft;
	std::string _instrument;

	double _previousTime;
	target _target;
	std::string _name;
	bool _moving;
};

} // namespace openspace
#endif 
