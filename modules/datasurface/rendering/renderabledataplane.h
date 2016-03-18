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

#ifndef __RENDERABLEDATAPLANE_H__
#define __RENDERABLEDATAPLANE_H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <ghoul/opengl/texture.h>

 namespace openspace{
 
 class RenderableDataPlane : public Renderable {
 public:
 	RenderableDataPlane(const ghoul::Dictionary& dictionary);
 	~RenderableDataPlane();

 	bool initialize() override;
    bool deinitialize() override;

	bool isReady() const override;

	virtual void render(const RenderData& data) override;
	virtual void update(const UpdateData& data) override;
 
 private:
 	void loadTexture();
    void createPlane();

 	properties::StringProperty _texturePath;
 	properties::Vec2Property _size;
 	properties::Vec3Property _roatation;
 	properties::Vec2Property _origin;
 	// Origin _origin;

	std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
	std::unique_ptr<ghoul::opengl::Texture> _texture;

	GLuint _quad;
	GLuint _vertexPositionBuffer;

	bool _planeIsDirty;
 };
 
 } // namespace openspace

#endif