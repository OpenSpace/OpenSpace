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

#ifndef __RENDERABLEWAVEFRONTOBJECT_H__
#define __RENDERABLEWAVEFRONTOBJECT_H__

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/util/updatestructures.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

class RenderableWavefrontObject : public Renderable {
public:
	RenderableWavefrontObject(const ghoul::Dictionary& dictionary);
	~RenderableWavefrontObject();

    bool initialize() override;
    bool deinitialize() override;

	void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

	typedef struct
	{
		GLfloat location[4];
		GLfloat tex[2];
		GLfloat normal[3];
	/*	GLfloat color[4];
		GLfloat attribute[3];
		GLfloat float_attribute;*/
		//GLubyte padding[4]; // Pads the struct out to 64 bytes for performance increase
	} Vertex;

protected:
    void loadTexture();
	void loadObj(const char *filename);

private:
    properties::StringProperty _colorTexturePath;
    ghoul::opengl::ProgramObject* _programObject; 
    ghoul::opengl::Texture* _texture;

	ghoul::opengl::ProgramObject* _fovProgram;


	GLuint _vaoID = 6;
	GLuint _vBufferID = 7;
	GLuint _iBufferID = 8;

	GLenum _mode;
	unsigned int _isize;
	unsigned int _vsize;
	Vertex* _varray;
	int* _iarray;

	glm::dmat3 _stateMatrix; // might need this

	std::string _source;
	std::string _destination;

};

}  // namespace openspace

#endif  // __RENDERABLEWAVEFRONTOBJECT_H__