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

#include <openspace/abuffer/abuffervisualizer.h>
#include <openspace/util/constants.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowhandler.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>

#include <sgct.h>

#define MARKER_POINTS
#define MARKER_LINES
#define COLOR_BOX

namespace {
	const std::string _loggerCat = "ABufferVisualizer";
}

namespace openspace {

ABufferVisualizer::ABufferVisualizer()
	: _pointcloud(0)
	, _pointcloudSize(0)
	, _markers(0)
	, _markersSize(0)
	, _pointcloudProgram(nullptr)
{
	
}

ABufferVisualizer::~ABufferVisualizer() {
	if (_pointcloud)
		glDeleteVertexArrays(1, &_pointcloud);
	if (_markers)
		glDeleteVertexArrays(1, &_markers);
	if (_pointcloudProgram)
		delete _pointcloudProgram;
}

void ABufferVisualizer::updateData(const std::vector<ABuffer::fragmentData>& data) {

	if (_pointcloud)
		glDeleteVertexArrays(1, &_pointcloud);

	_pointcloudSize = static_cast<GLsizei>(data.size());
	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_pointcloud); // generate array
	glBindVertexArray(_pointcloud); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(ABuffer::fragmentData)*_pointcloudSize, data.data(), GL_STATIC_DRAW);


	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(ABuffer::fragmentData),
		reinterpret_cast<const GLvoid*>(offsetof(ABuffer::fragmentData, _position)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(ABuffer::fragmentData),
		reinterpret_cast<const GLvoid*>(offsetof(ABuffer::fragmentData, _color)));

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);



	if (!_markers)
		initializeMarkers();

	if (!_pointcloudProgram) {
		_pointcloudProgram = ghoul::opengl::ProgramObject::Build(
			"pointcloudProgram",
			"${SHADERS}/pointcloud_vs.glsl",
			"${SHADERS}/pointcloud_fs.glsl");
		if (!_pointcloudProgram)
			LERROR("Could not compile _pointcloudProgram");
	}
}

void ABufferVisualizer::render() {
	if (!_pointcloudProgram)
		return;

	glDisable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);
	_pointcloudProgram->activate();

	glm::mat4 modelMatrix = glm::mat4(1.0);
	static glm::mat4 rotation = glm::mat4(1.0);
	rotation = glm::rotate(rotation, 0.3f, glm::vec3(0, 1, 0));
	static glm::mat4 rotationText = glm::mat4(1.0);
	rotationText = glm::rotate(rotationText, -0.3f, glm::vec3(0, 1, 0));

	modelMatrix = glm::translate(modelMatrix, glm::vec3(0, 0, -1));
	modelMatrix = modelMatrix * rotation;

    _pointcloudProgram->setUniform("ViewProjection", OsEng.windowWrapper()->viewProjectionMatrix());
	_pointcloudProgram->setUniform("ModelTransform", modelMatrix);

#if defined(MARKER_POINTS)
	glPointSize(2.0);
	glBindVertexArray(_markers);
	glDrawArrays(GL_POINTS, 0, _markersSize);
	glBindVertexArray(0);
#endif

#if defined(MARKER_LINES)
	glBindVertexArray(_markers);  // select first VAO
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _imarkers);
	glDrawElements(GL_LINES, _imarkersSize, GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
#endif


	glPointSize(1.0);
	glBindVertexArray(_pointcloud);
	glDrawArrays(GL_POINTS, 0, _pointcloudSize);
	glBindVertexArray(0);


	_pointcloudProgram->deactivate();

	const int font_size_light = 8;
	const sgct_text::Font* fontLight = sgct_text::FontManager::instance()->getFont(constants::fonts::keyLight, font_size_light);

	const glm::mat4 scale = glm::scale(glm::mat4(1.0), glm::vec3(0.04, 0.04, 0.04));
	glm::mat4 translate, mvp;

    const glm::mat4 viewProjMatrix = OsEng.windowWrapper()->viewProjectionMatrix();
	translate = glm::translate(glm::mat4(1.0), glm::vec3(0, 0, 0));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(0,0,0)");
	translate = glm::translate(glm::mat4(1.0), glm::vec3(0, 0, 1));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(0,0,1)");
	translate = glm::translate(glm::mat4(1.0), glm::vec3(0, 1, 0));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(0,1,0)");
	translate = glm::translate(glm::mat4(1.0), glm::vec3(1, 0, 0));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(1,0,0)");
	translate = glm::translate(glm::mat4(1.0), glm::vec3(0, 1, 1));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(0,1,1)");
	translate = glm::translate(glm::mat4(1.0), glm::vec3(1, 0, 1));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(1,0,1)");
	translate = glm::translate(glm::mat4(1.0), glm::vec3(1, 1, 0));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(1,1,0)");
	translate = glm::translate(glm::mat4(1.0), glm::vec3(1, 1, 1));
	mvp = viewProjMatrix*modelMatrix*translate*rotationText*scale;
	Freetype::print3d(fontLight, mvp, "(1,1,1)");
}

void ABufferVisualizer::initializeMarkers() {
	ABuffer::fragmentData fd;
	fd._color[0] = 0.3f;
	fd._color[1] = 0.3f;
	fd._color[2] = 0.3f;
	fd._color[3] = 1.0f;
	std::vector<ABuffer::fragmentData> markers(8, fd);

#ifdef COLOR_BOX
	// First 4
	markers.at(0)._position[0] = 0.0f; markers.at(0)._color[0] = 0.0f;
	markers.at(0)._position[1] = 0.0f; markers.at(0)._color[1] = 0.0f;
	markers.at(0)._position[2] = 0.0f; markers.at(0)._color[2] = 0.0f;

	markers.at(1)._position[0] = 1.0f; markers.at(1)._color[0] = 1.0f;
	markers.at(1)._position[1] = 0.0f; markers.at(1)._color[1] = 0.0f;
	markers.at(1)._position[2] = 0.0f; markers.at(1)._color[2] = 0.0f;

	markers.at(2)._position[0] = 1.0f; markers.at(2)._color[0] = 1.0f;
	markers.at(2)._position[1] = 1.0f; markers.at(2)._color[1] = 1.0f;
	markers.at(2)._position[2] = 0.0f; markers.at(2)._color[2] = 0.0f;

	markers.at(3)._position[0] = 0.0f; markers.at(3)._color[0] = 0.0f;
	markers.at(3)._position[1] = 1.0f; markers.at(3)._color[1] = 1.0f;
	markers.at(3)._position[2] = 0.0f; markers.at(3)._color[2] = 0.0f;


	// last 4
	markers.at(4)._position[0] = 0.0f; markers.at(4)._color[0] = 0.0f;
	markers.at(4)._position[1] = 0.0f; markers.at(4)._color[1] = 0.0f;
	markers.at(4)._position[2] = 1.0f; markers.at(4)._color[2] = 1.0f;

	markers.at(5)._position[0] = 1.0f; markers.at(5)._color[0] = 1.0f;
	markers.at(5)._position[1] = 0.0f; markers.at(5)._color[1] = 0.0f;
	markers.at(5)._position[2] = 1.0f; markers.at(5)._color[2] = 1.0f;

	markers.at(6)._position[0] = 1.0f; markers.at(6)._color[0] = 1.0f;
	markers.at(6)._position[1] = 1.0f; markers.at(6)._color[1] = 1.0f;
	markers.at(6)._position[2] = 1.0f; markers.at(6)._color[2] = 1.0f;

	markers.at(7)._position[0] = 0.0f; markers.at(7)._color[0] = 0.0f;
	markers.at(7)._position[1] = 1.0f; markers.at(7)._color[1] = 1.0f;
	markers.at(7)._position[2] = 1.0f; markers.at(7)._color[2] = 1.0f;
#else
	// First 4
	markers.at(0)._position[0] = 0.0f;
	markers.at(0)._position[1] = 0.0f; 
	markers.at(0)._position[2] = 0.0f; 

	markers.at(1)._position[0] = 1.0f;
	markers.at(1)._position[1] = 0.0f; 
	markers.at(1)._position[2] = 0.0f; 

	markers.at(2)._position[0] = 1.0f; 
	markers.at(2)._position[1] = 1.0f; 
	markers.at(2)._position[2] = 0.0f;

	markers.at(3)._position[0] = 0.0f;
	markers.at(3)._position[1] = 1.0f;
	markers.at(3)._position[2] = 0.0f;


	// last 4
	markers.at(4)._position[0] = 0.0f;
	markers.at(4)._position[1] = 0.0f; 
	markers.at(4)._position[2] = 1.0f;

	markers.at(5)._position[0] = 1.0f; 
	markers.at(5)._position[1] = 0.0f; 
	markers.at(5)._position[2] = 1.0f; 

	markers.at(6)._position[0] = 1.0f; 
	markers.at(6)._position[1] = 1.0f; 
	markers.at(6)._position[2] = 1.0f; 

	markers.at(7)._position[0] = 0.0f; 
	markers.at(7)._position[1] = 1.0f; 
	markers.at(7)._position[2] = 1.0f; 
#endif 

	_markersSize = static_cast<GLsizei>(markers.size());
	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_markers); // generate array
	glBindVertexArray(_markers); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(ABuffer::fragmentData)*_markersSize, markers.data(), GL_STATIC_DRAW);


	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(ABuffer::fragmentData),
		reinterpret_cast<const GLvoid*>(offsetof(ABuffer::fragmentData, _position)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(ABuffer::fragmentData),
		reinterpret_cast<const GLvoid*>(offsetof(ABuffer::fragmentData, _color)));

	//glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 3, reinterpret_cast<void*>(0));
	//glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 4, reinterpret_cast<void*>(sizeof(GLfloat) * 3));
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);

	std::vector<GLsizei> ivector;
	// front
	ivector.push_back(0);
	ivector.push_back(1);

	ivector.push_back(1);
	ivector.push_back(2);

	ivector.push_back(2);
	ivector.push_back(3);

	ivector.push_back(3);
	ivector.push_back(0);
	
	// back
	ivector.push_back(4);
	ivector.push_back(5);

	ivector.push_back(5);
	ivector.push_back(6);

	ivector.push_back(6);
	ivector.push_back(7);

	ivector.push_back(7);
	ivector.push_back(4);

	// connections between front and back
	ivector.push_back(0);
	ivector.push_back(4);

	ivector.push_back(3);
	ivector.push_back(7);

	ivector.push_back(1);
	ivector.push_back(5);

	ivector.push_back(2);
	ivector.push_back(6);
	

	_imarkersSize = static_cast<GLsizei>(ivector.size());
	glBindVertexArray(0);

	glGenBuffers(1, &_imarkers);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _imarkers);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _imarkersSize * sizeof(GLsizei), ivector.data(), GL_STATIC_DRAW);
}

} // openspace