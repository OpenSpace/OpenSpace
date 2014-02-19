/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// open space includes
#include "util/vbo.h"

// ghoul includes
#include "ghoul/logging/logmanager.h"
#include "ghoul/logging/consolelog.h"

// std includes
#include <string>
#include <cstdlib>
#include <iostream>

gl4::VBO::VBO() 
{
	_vBufferID = 0;
	_iBufferID = 0;
	_vaoID = 0;
	_isize = 0;
	_vsize = 0;
	_varray = NULL;
	_iarray = NULL;
	_mode = GL_POINTS;
	_w = 1.0f;
	_h = 1.0f;
}
gl4::VBO::~VBO() 
{
	free(_varray);
	free(_iarray);
}

void gl4::VBO::init() 
{
	
	//ghoul logging
	std::string _loggerCat = "VBO::init";

	//LOG("VBO Init()\n");
	//LOG("   _vsize = %d\n", _vsize);
	//LOG("   _isize   = %d\n", _isize);

	// if arrays not set from sub-class initialize with a colored quad
	if(_vsize == 0 || _isize == 0 || _varray == NULL || _iarray == NULL) {

		//LOG("VBO: Init color quad\n");
		_mode = GL_TRIANGLES;

		_vsize = 4;
		_isize = 6;
		_varray = (Vertex*) std::malloc(_vsize*sizeof(Vertex));
		_iarray = (int*) std::malloc(_isize*sizeof(int));

		_varray[0].location[0] = 0.0f;
		_varray[0].location[1] = 0.0f;
		_varray[0].location[2] = 0.0f;
		_varray[1].location[0] = _w;
		_varray[1].location[1] = 0.0f;
		_varray[1].location[2] = 0.0f;
		_varray[2].location[0] = 0.0f;
		_varray[2].location[1] = _h;
		_varray[2].location[2] = 0.0f;
		_varray[3].location[0] = _w;
		_varray[3].location[1] = _h;
		_varray[3].location[2] = 0.0f;

		_varray[0].tex[0] = 0.0f;
		_varray[0].tex[1] = 0.0f;
		_varray[1].tex[0] = 1.0f;
		_varray[1].tex[1] = 0.0f;
		_varray[2].tex[0] = 0.0f;
		_varray[2].tex[1] = 1.0f;
		_varray[3].tex[0] = 1.0f;
		_varray[3].tex[1] = 1.0f;

		_iarray[0] = 0;
		_iarray[1] = 1;
		_iarray[2] = 2;
		_iarray[3] = 2;
		_iarray[4] = 1;
		_iarray[5] = 3;

		for (int i = 0; i < 4; ++i)
		{
			_varray[i].normal[0] = 1.0f;
			_varray[i].normal[1] = 0.0f;
			_varray[i].normal[2] = 0.0f;
			_varray[i].color[3] = 1.0f;
			_varray[i].attribute[0] = 1.0f;
			_varray[i].attribute[1] = 0.0f;
			_varray[i].attribute[2] = 0.0f;
			_varray[i].float_attribute = 0.0f;
		}
		// red
		_varray[0].color[0] = 1.0f;
		_varray[0].color[1] = 0.0f;
		_varray[0].color[2] = 0.0f;

		// green
		_varray[1].color[0] = 0.0f;
		_varray[1].color[1] = 1.0f;
		_varray[1].color[2] = 0.0f;

		// blue
		_varray[2].color[0] = 0.0f;
		_varray[2].color[1] = 0.0f;
		_varray[2].color[2] = 1.0f;

		// white
		_varray[3].color[0] = 1.0f;
		_varray[3].color[1] = 1.0f;
		_varray[3].color[2] = 1.0f;

	}

	GLuint errorID = glGetError();
	glGenVertexArrays(1, &_vaoID);

	// First VAO setup
	glBindVertexArray(_vaoID);

	glGenBuffers(1, &_vBufferID);

	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferData(GL_ARRAY_BUFFER, _vsize*sizeof(Vertex), _varray, GL_STATIC_DRAW);

	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), BUFFER_OFFSET(0));
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), BUFFER_OFFSET(12));
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), BUFFER_OFFSET(20));
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), BUFFER_OFFSET(32));
	glEnableVertexAttribArray(3);
	glVertexAttribPointer(4, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), BUFFER_OFFSET(48));
	glEnableVertexAttribArray(4);
	glVertexAttribPointer(5, 1, GL_FLOAT, GL_FALSE, sizeof(Vertex), BUFFER_OFFSET(60));
	glEnableVertexAttribArray(5);

	glGenBuffers(1, &_iBufferID);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize*sizeof(int), _iarray, GL_STATIC_DRAW);

	if(_vBufferID == 0)
	{
		LERROR("Vertex buffer not initialized");
	}
	if(_iBufferID == 0) 
	{
		LERROR("Index buffer not initialized");
	}

	glBindVertexArray(0);

	errorID = glGetError();
	if(errorID != GL_NO_ERROR)
	{
		LERROR("OpenGL error: " << glewGetErrorString(errorID));
		LERROR("Attempting to proceed anyway. Expect rendering errors or a crash.");
	}

}

void gl4::VBO::render() 
{
	glBindVertexArray(_vaoID);		// select first VAO
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glDrawElements(_mode, _isize, GL_UNSIGNED_INT, BUFFER_OFFSET(0));
	glBindVertexArray(0);
}
