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

#include <openspace/rendering/stars/renderableconstellationbounds.h>

#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <SpiceUsr.h>

#define _USE_MATH_DEFINES
#include <math.h>

#include <fstream>
#include <vector>

namespace {
	const std::string _loggerCat = "RenderableConstellationBounds";

	const std::string keyFile = "File";
	const std::string keyOriginalReferenceFrame = "OriginalReferenceFrame";

	float deg2rad(float deg) {
		return (deg / 360.f) * 2.f * 3.1415926f;
	}
	float convertHrsToRadians(float rightAscension) {
		// 360 degrees / 24h = 15 degrees/h
		return deg2rad(rightAscension * 15);
	}
}

namespace openspace {

RenderableConstellationBounds::RenderableConstellationBounds(
			const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _filename("")
	, _programIsDirty(false)
	, _distance("distance", "Distance to the celestial Sphere", 15.f, 0.f, 30.f)
	, _originReferenceFrame("")
	, _vao(0)
	, _vbo(0)
{
	bool success = dictionary.getValue(keyFile, _filename);
	if (!success) {
		LERROR("RenderableConstellationBounds did not contain a key '" <<
			keyFile << "'");
	}

	success = dictionary.getValue(keyOriginalReferenceFrame, _originReferenceFrame);
	if (!success) {
		LERROR("RenderableConstellationBounds did not contain a key '" <<
			keyOriginalReferenceFrame << "'");
	}

	addProperty(_distance);
}

bool RenderableConstellationBounds::initialize() {
	_program = ghoul::opengl::ProgramObject::Build("ConstellationBounds",
		"${SHADERS}/constellationbounds_vs.glsl",
		"${SHADERS}/constellationbounds_fs.glsl");
	if (!_program)
		return false;
	_program->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*){ this->_programIsDirty = true; });


	if (_vao == 0) {
		glGenVertexArrays(1, &_vao);
		LDEBUG("Generating Vertex Array id '" << _vao << "'");
	}
	if (_vbo == 0) {
		glGenBuffers(1, &_vbo);
		LDEBUG("Generating Vertex Buffer Object id '" << _vbo << "'");
	}

	bool loadSuccess = loadFile();
	if (!loadSuccess)
		return false;

	glBindVertexArray(_vao);
	glBindBuffer(GL_ARRAY_BUFFER, _vbo);
	glBufferData(GL_ARRAY_BUFFER,
			_vertexValues.size() * 3 * sizeof(float),
			&_vertexValues[0],
			GL_STATIC_DRAW
			);

	GLint positionAttrib = _program->attributeLocation("in_position");
	glEnableVertexAttribArray(positionAttrib);
	glVertexAttribPointer(positionAttrib, 3, GL_FLOAT, GL_FALSE, 0, 0);

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);

	return true;
}

bool RenderableConstellationBounds::deinitialize() {
	glDeleteBuffers(1, &_vbo);
	_vbo = 0;
	glDeleteVertexArrays(1, &_vao);
	_vao = 0;

	delete _program;
	_program = nullptr;
	return true;
}

bool RenderableConstellationBounds::isReady() const {
	return (_vao != 0) && (_vbo != 0);
}

void RenderableConstellationBounds::render(const RenderData& data) {
	_program->activate();

	glm::mat4 modelMatrix      = data.camera.modelMatrix();
	glm::mat4 viewMatrix       = data.camera.viewMatrix();
	glm::mat4 projectionMatrix = data.camera.projectionMatrix();

	setPscUniforms(_program, &data.camera, data.position);

	_program->setUniform("exponent", _distance);
	_program->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_program->setUniform("ModelTransform", glm::mat4(glm::dmat4(_stateMatrix)));

	glBindVertexArray(_vao);
	for (auto bound : _constellationBounds)
		glDrawArrays(GL_LINE_STRIP, bound.startIndex , bound.nVertices);
	glBindVertexArray(0);
	_program->deactivate();
}

void RenderableConstellationBounds::update(const UpdateData& data) {
	if (_programIsDirty) {
		_program->rebuildFromFile();
		_programIsDirty = false;
	}

	SpiceManager::ref().getPositionTransformMatrix(
		_originReferenceFrame,
		"GALACTIC",
		data.time,
		_stateMatrix
	);
}

bool RenderableConstellationBounds::loadFile() {
	if (_filename.empty())
		return false;

	std::string fileName = absPath(_filename);
	std::ifstream file(fileName);
	if (!file.good()) {
		LERROR("Could not open file '" << fileName << "' for reading");
		return false;
	}

	ConstellationBound currentBound;
	currentBound.constellation = "";
	std::string line;
	int lineNumber = 1;
	while (file.good()) {
		std::getline(file, line);
		if (line.empty())
			continue;

		// @CHECK: Is this the best way of doing this? ---abock
		float ra;
		float dec;
		std::string constellation;
		std::stringstream s(line);
		s >> ra;
		s >> dec;
		s >> constellation;

		if (!s.good()) {
			// If this evaluates to true, the stream was not completely filled, which
			// means that the line was incomplete, so there was an error
			LERROR("Error reading file '" << fileName << "' at line #" << lineNumber);
		}

		if (constellation != currentBound.constellation) {
			// @CHECK: Does this work? ---abock
			currentBound.nVertices = (_vertexValues.size() - currentBound.startIndex);
			_constellationBounds.push_back(currentBound);
			currentBound = ConstellationBound();
			currentBound.constellation = constellation;
			currentBound.startIndex = _vertexValues.size();
		}

		ra = convertHrsToRadians(ra);
		dec = deg2rad(dec);

		SpiceDouble values[3];
		radrec_c(1.0, ra, dec, values);

		std::array<float, 3> v;
		v[0] = values[0];
		v[1] = values[1];
		v[2] = values[2];

		_vertexValues.push_back(v);
		++lineNumber;
	}

	// remove the first one
	_constellationBounds.erase(_constellationBounds.begin());

	return true;
}

} // namespace openspace
