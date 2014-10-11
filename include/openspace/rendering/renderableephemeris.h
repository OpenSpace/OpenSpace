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

#ifndef __RENDERABLEEPHEMERIS_H__
#define __RENDERABLEEPHEMERIS_H__

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
//#include <openspace/util/runtimedata.h>

namespace openspace {
class RenderableEphemeris : public Renderable{
public:
	RenderableEphemeris(const ghoul::Dictionary& dictionary);
	~RenderableEphemeris();

	bool initialize() override;
	bool deinitialize() override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;
 private:
	 properties::StringProperty _colorTexturePath; // not used now, will be later though.

	 ghoul::opengl::ProgramObject* _programObject;
	 ghoul::opengl::Texture* _texture;
	 void loadTexture();

	 typedef struct {
		 GLfloat location[4];
		 GLfloat velocity[4];
		 GLubyte padding[32];  // Pads the struct out to 64 bytes for performance increase
	 } Vertex;

	 GLuint _vaoID = 6;
	 GLuint _vBufferID = 7;
	 GLuint _iBufferID = 8;

	 void nextIndex();


	 GLenum _mode;
	 unsigned int _isize;
	 unsigned int _vsize;

	 Vertex* _varray;
	 int* _iarray;

	 Vertex*  batchArray;
	 bool* _updated;

	 psc _pscpos, _pscvel;

	 std::vector<std::pair<int, double>> _intervals;
	 double _increment;
	 // etc...
	 int _index[2];
	 int _prev[2];
	 int _delta;
	 double _time = 0;
	 double _previousTime = 0;
};
}
#endif