/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#ifndef VBO_H
#define VBO_H

// sgct includes
//#include "sgct.h"
#include <ghoul/opengl/opengl>


#define BUFFER_OFFSET(i) ((char *)NULL + (i))

typedef struct
{
	GLfloat location[3];
	GLfloat tex[2];
	GLfloat normal[3];
	GLfloat color[4];
	GLfloat attribute[3];
	GLfloat float_attribute;
	//GLubyte padding[4]; // Pads the struct out to 64 bytes for performance increase
} Vertex;

namespace gl4
{
	class VBO 
	{
	public:
		//initializers
		VBO();
		~VBO();

		// init VBO
		virtual void init();
		void setProportions(float w, float h) { _w = w; _h = h;};

		// render
		void render();
	private:

		GLuint _vaoID;
		GLuint _vBufferID;
		GLuint _iBufferID;
		float _w;
		float _h;

	protected:
		// arrays with all triangles and indices
		GLenum _mode;
		unsigned int _isize;
		unsigned int _vsize;
		Vertex *_varray;
		int *_iarray;
	};
}

#endif