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
#include <openspace/rendering/screenspacerenderable.h>

namespace openspace {
	ScreenSpaceRenderable::ScreenSpaceRenderable()
		: _enabled("enabled", "Is Enabled", true)
		, _position("position", "Position", glm::vec3(0,0,0))
		, _size("size", "Size" , glm::vec2(0.5,1.0))
		, _quad(0)
		, _vertexPositionBuffer(0)
	{
		addProperty(_enabled);
		addProperty(_position);
		addProperty(_size);
	}

	ScreenSpaceRenderable::~ScreenSpaceRenderable(){}

	void ScreenSpaceRenderable::createPlane() {
	    // ============================
	    // 		GEOMETRY (quad)
	    // ============================
	    const GLfloat size = _size.value()[0];
	    const GLfloat w = _size.value()[1];
	    const GLfloat vertex_data[] = { // square of two triangles (sigh)
	        //	  x      y     z     w     s     t
	        -size, -size, 0.0f, w, 0, 1,
	        size, size, 0.0f, w, 1, 0,
	        -size, size, 0.0f, w, 0, 0,
	        -size, -size, 0.0f, w, 0, 1,
	        size, -size, 0.0f, w, 1, 1,
	        size, size, 0.0f, w, 1, 0,
	    };

	    glBindVertexArray(_quad); // bind array
	    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
	    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
	    glEnableVertexAttribArray(0);
	    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
	    glEnableVertexAttribArray(1);
	    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
	}
}