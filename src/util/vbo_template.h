/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#ifndef VBOTEMPLATE_H
#define VBOTEMPLATE_H

// std includes
#include <string>
#include <vector>
#include <tuple>

// sgct includes
#include "ghoul/logging/consolelog.h"

// sgct includes
#include "sgct.h"


#define BUFFER_OFFSET(i) ((char *)NULL + (i))

namespace openspace
{

template<typename T>
class VBO {
public:
	//initializers
	VBO(const std::vector<std::tuple<int, GLenum, int> > descriptor, T *varray, int vsize, int *iarray, int isize): descriptor_(descriptor), isize_(isize), vsize_(vsize), varray_(varray), iarray_(iarray) {
		static_assert(std::is_pod<T>::value, "Template typename should be a POD");
		vBufferID_ = 0;
		iBufferID_ = 0;
		vaoID_ = 0;
	}
	~VBO() {};

	// init VBO
	void init() {
	
		//ghoul logging
		std::string _loggerCat = "VBO::init";

		GLuint errorID = glGetError();
		glGenVertexArrays(1, &vaoID_);

		// First VAO setup
		glBindVertexArray(vaoID_);

		glGenBuffers(1, &vBufferID_);

		glBindBuffer(GL_ARRAY_BUFFER, vBufferID_);
		glBufferData(GL_ARRAY_BUFFER, vsize_*sizeof(T), varray_, GL_STATIC_DRAW);


		for(size_t i = 0; i < descriptor_.size(); ++i) {
			glVertexAttribPointer(i,std::get<0>(descriptor_.at(i)), std::get<1>(descriptor_.at(i)), GL_FALSE, sizeof(T), BUFFER_OFFSET(std::get<2>(descriptor_.at(i))));
			glEnableVertexAttribArray(i);
		}

		glGenBuffers(1, &iBufferID_);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iBufferID_);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, isize_*sizeof(int), iarray_, GL_STATIC_DRAW);

		if(vBufferID_ == 0) {
			LERROR("Vertex buffer not initialized");
		}
		if(iBufferID_ == 0) {
			LERROR("Index buffer not initialized");
		}

		glBindVertexArray(0);

		errorID = glGetError();
		if(errorID != GL_NO_ERROR)
		{
			LERROR("OpenGL error: " << glewGetErrorString(errorID));
			LERROR("Attempting to proceed anyway. Expect rendering errors or a crash.");
		}

	};

	// render
	void render() {
		glBindVertexArray(vaoID_);		// select first VAO
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iBufferID_);
		glDrawElements(GL_TRIANGLES, isize_, GL_UNSIGNED_INT, BUFFER_OFFSET(0));
		glBindVertexArray(0);
	};
private:

	GLuint vaoID_;
	GLuint vBufferID_;
	GLuint iBufferID_;

	unsigned int isize_;
	unsigned int vsize_;

	std::vector<std::tuple<int, GLenum, int> > descriptor_;
	T *varray_;
	int *iarray_;
};
	
} // namespace openspace

#endif