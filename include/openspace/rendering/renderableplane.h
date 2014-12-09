/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef RENDERABLEPLANE_H_
#define RENDERABLEPLANE_H_

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/util/updatestructures.h>

// ghoul includes
#include <openspace/properties/stringproperty.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {
	struct LinePoint;

class RenderablePlane : public Renderable {

	enum class Origin {
		LowerLeft, LowerRight, UpperLeft, UpperRight, Center
	};

public:
	RenderablePlane(const ghoul::Dictionary& dictionary);
	~RenderablePlane();

	bool initialize() override;
	bool deinitialize() override;

	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

private:
	void loadTexture();

	properties::StringProperty _texturePath;
	properties::BoolProperty _billboard;

	glm::vec2 _size;
	Origin _origin;

	ghoul::opengl::ProgramObject* _shader;
	ghoul::opengl::Texture* _texture;
	GLuint _quad;
};

} // namespace openspace
#endif // RENDERABLEFIELDLINES_H_
