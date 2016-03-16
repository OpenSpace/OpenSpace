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
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/screenspacerenderable.h>

namespace openspace {
ScreenSpaceRenderable::ScreenSpaceRenderable()
	: _enabled("enabled", "Is Enabled", true)
	, _flatScreen("flatScreen", "Flat Screen", true)
	, _euclideanPosition("euclideanPosition", "Euclidean coordinates", glm::vec2(0),glm::vec2(-4),glm::vec2(4))
	, _sphericalPosition("sphericalPosition", "Spherical coordinates", glm::vec2(0),glm::vec2(-M_PI),glm::vec2(M_PI))
	, _depth("depth", "Depth", 0, 0, 1)
	, _scale("scale", "Scale" , 0.5, 0, 1)
	// , _texturePath("texturePath", "Texture path", texturePath)
	, _quad(0)
	, _vertexPositionBuffer(0)
	,_shader(nullptr)
	,_radius(-.2f)

{
	addProperty(_enabled);
	addProperty(_flatScreen);
	addProperty(_euclideanPosition);
	addProperty(_sphericalPosition);
	addProperty(_depth);
	addProperty(_scale);
	// addProperty(_texturePath);

	_useEuclideanCoordinates = _flatScreen.value();
}

ScreenSpaceRenderable::~ScreenSpaceRenderable(){}

bool ScreenSpaceRenderable::isEnabled() const {
	return _enabled;
}

void ScreenSpaceRenderable::createPlane() {
    // ============================
    // 		GEOMETRY (quad)
    // ============================
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //	  x      y     z     w     s     t
        -1, -1, 0.0f, 1, 0, 1,
         1,  1, 0.0f, 1, 1, 0,
        -1,  1, 0.0f, 1, 0, 0,
        -1, -1, 0.0f, 1, 0, 1,
         1, -1, 0.0f, 1, 1, 1,
         1,  1, 0.0f, 1, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

glm::vec2 ScreenSpaceRenderable::toEuclidean(glm::vec2 polar, float r){
	float x = r*sin(polar[0])*sin(polar[1]);
	float y = r*cos(polar[1]);
	
	return glm::vec2(x, y);
}

glm::vec2 ScreenSpaceRenderable::toSpherical(glm::vec2 euclidean){	
	_radius = -sqrt(pow(euclidean[0],2)+pow(euclidean[1],2)+pow(_planeDepth,2));
	float theta	= atan2(-_planeDepth,euclidean[0])-M_PI/2.0;
	float phi = acos(euclidean[1]/_radius);

	return glm::vec2(theta, phi);
}

}// namespace openspace