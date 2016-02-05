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

// openspace
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/base/rendering/renderableconstellationbounds.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>

// ghoul
#include <ghoul/filesystem/filesystem.h>

// std
#include <fstream>
#define _USE_MATH_DEFINES
#include <math.h>

namespace {
	const std::string _loggerCat = "RenderableConstellationBounds";

	const std::string keyVertexFile = "File";
	const std::string keyConstellationFile = "ConstellationFile";
	const std::string keyReferenceFrame = "ReferenceFrame";

	const std::string defaultReferenceFrame = "J2000";

	float deg2rad(float deg) {
		return static_cast<float>((deg / 360.f) * 2.f * M_PI);
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
	, _vertexFilename("")
	, _constellationFilename("")
	, _distance("distance", "Distance to the celestial Sphere", 15.f, 0.f, 30.f)
	, _constellationSelection("constellationSelection", "Constellation Selection")
	, _originReferenceFrame("")
	, _vao(0)
	, _vbo(0)
{
	bool success = dictionary.getValue(keyVertexFile, _vertexFilename);
	if (!success) {
		LERROR("RenderableConstellationBounds did not contain a key '" <<
			keyVertexFile << "'");
	}

	dictionary.getValue(keyConstellationFile, _constellationFilename);

	success = dictionary.getValue(keyReferenceFrame, _originReferenceFrame);
	if (!success) {
		_originReferenceFrame = defaultReferenceFrame;
	}

	addProperty(_distance);
	addProperty(_constellationSelection);
	_constellationSelection.onChange(
		std::bind(&RenderableConstellationBounds::selectionPropertyHasChanged, this)
	);
}

RenderableConstellationBounds::~RenderableConstellationBounds() {
}

bool RenderableConstellationBounds::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _program = renderEngine.buildRenderProgram("ConstellationBounds",
        "${MODULE_BASE}/shaders/constellationbounds_vs.glsl",
        "${MODULE_BASE}/shaders/constellationbounds_fs.glsl");

	if (!_program)
		return false;

	bool loadSuccess = loadVertexFile();
	if (!loadSuccess)
		return false;
	loadSuccess = loadConstellationFile();
	if (!loadSuccess)
		return false;

	fillSelectionProperty();

	if (_vao == 0) {
		glGenVertexArrays(1, &_vao);
		LDEBUG("Generating Vertex Array id '" << _vao << "'");
	}
	if (_vbo == 0) {
		glGenBuffers(1, &_vbo);
		LDEBUG("Generating Vertex Buffer Object id '" << _vbo << "'");
	}

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

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }

	return true;
}

bool RenderableConstellationBounds::isReady() const {
	return (_vao != 0) && (_vbo != 0) && (_program != nullptr);
}

void RenderableConstellationBounds::render(const RenderData& data) {
	_program->activate();

	//glm::mat4 modelMatrix      = data.camera.modelMatrix();
	glm::mat4 viewMatrix       = data.camera.viewMatrix();
	glm::mat4 projectionMatrix = data.camera.projectionMatrix();

	setPscUniforms(_program.get(), &data.camera, data.position);

	_program->setUniform("exponent", _distance);
	_program->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_program->setUniform("ModelTransform", glm::mat4(glm::dmat4(_stateMatrix)));

	glBindVertexArray(_vao);
	for (const ConstellationBound& bound : _constellationBounds) {
		if (bound.isEnabled) {
			glDrawArrays(
				//GL_LINE_STRIP,
				GL_LINE_LOOP,
				static_cast<GLsizei>(bound.startIndex),
				static_cast<GLsizei>(bound.nVertices)
			);
		}
	}
	glBindVertexArray(0);
	_program->deactivate();
}

void RenderableConstellationBounds::update(const UpdateData& data) {
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(
		_originReferenceFrame,
		"GALACTIC",
		data.time
	);
}

bool RenderableConstellationBounds::loadVertexFile() {
	if (_vertexFilename.empty())
		return false;

	std::string fileName = absPath(_vertexFilename);
	std::ifstream file(fileName);
	if (!file.good()) {
		LERROR("Could not open file '" << fileName << "' for reading");
		return false;
	}

	ConstellationBound currentBound;
	currentBound.constellationAbbreviation = "";

	std::string currentLine;
	int currentLineNumber = 1;

	float ra;
	float dec;
	std::string constellationName;
	SpiceDouble rectangularValues[3];

	// Overview of the reading algorithm:
	// We keep an active ConstellationBound (currentBound) and update it until we read
	// a new constellation name, at which point the currentBound is stored away, a new,
	// empty ConstellationBound is created and set at the currentBound
	while (file.good()) {
		std::getline(file, currentLine);
		if (currentLine.empty())
			continue;

		// @CHECK: Is this the best way of doing this? ---abock
		std::stringstream s(currentLine);
		s >> ra;
		s >> dec;
		s >> constellationName;

		if (!s.good()) {
			// If this evaluates to true, the stream was not completely filled, which
			// means that the line was incomplete, so there was an error
			LERROR("Error reading file '" << fileName << "' at line #" << currentLineNumber);
			break;
		}

		// Did we arrive at a new constellation?
		if (constellationName != currentBound.constellationAbbreviation) {
			// Store how many vertices we read during the active time of the constellation
			currentBound.nVertices = (_vertexValues.size() - currentBound.startIndex);
			// Store the constellation and start a new one
			_constellationBounds.push_back(currentBound);
			currentBound = ConstellationBound();
			currentBound.isEnabled = true;
			currentBound.constellationAbbreviation = constellationName;
			currentBound.constellationFullName = constellationName;
			currentBound.startIndex = _vertexValues.size();
		}

		// The file format stores the right ascension in hours, while SPICE expects them
		// to be in radians
		ra = convertHrsToRadians(ra);

		// Likewise, the declination is stored in degrees and needs to be converted
		dec = deg2rad(dec);

		// Convert the (right ascension, declination) to rectangular coordinates)
		// The 1.0 is the distance of the celestial sphere, we will scale that in the
		// render function
		radrec_c(1.0, ra, dec, rectangularValues);

		// Add the new vertex to our list of vertices
		_vertexValues.push_back({{
			static_cast<float>(rectangularValues[0]),
			static_cast<float>(rectangularValues[1]),
			static_cast<float>(rectangularValues[2])
		}});
		++currentLineNumber;
	}

	// Due to the way we read the file, the first (empty) constellation bounds will not
	// contain any valid values. So we have to remove it
	_constellationBounds.erase(_constellationBounds.begin());

	// And we still have the one value that was left when we exited the loop
	currentBound.nVertices = (_vertexValues.size() - currentBound.startIndex);
	_constellationBounds.push_back(currentBound);

	return true;
}

bool RenderableConstellationBounds::loadConstellationFile() {
	if (_constellationFilename.empty())
		return true;

	std::string fileName = absPath(_constellationFilename);
	std::ifstream file(fileName);
	if (!file.good()) {
		LERROR("Could not open file '" << fileName << "' for reading");
		return false;
	}

	std::string line;
	int index = 0;
	while (file.good()) {
 		std::getline(file, line);
		if (line.empty())
			continue;

		std::string abbreviation;
		std::stringstream s(line);
		s >> abbreviation;

		auto it = std::find_if(_constellationBounds.begin(), _constellationBounds.end(),
			[abbreviation](const ConstellationBound& bound) {
				return bound.constellationAbbreviation == abbreviation;
			});
		if (it == _constellationBounds.end()) {
			LERROR("Could not find constellation '" << abbreviation << "' in list");
			return false;
		}

		// Update the constellations full name
		s >> it->constellationFullName;

		++index;
	}

	return true;
}

void RenderableConstellationBounds::fillSelectionProperty() {
	// Each constellation is associated with its position in the array as this is unique
	// and will be constant during the runtime
	for (int i = 0 ; i < _constellationBounds.size(); ++i) {
		const ConstellationBound& bound = _constellationBounds[i];
		_constellationSelection.addOption( { i, bound.constellationFullName } );
	}
}

void RenderableConstellationBounds::selectionPropertyHasChanged() {
	const std::vector<int>& values = _constellationSelection;
	// If no values are selected (the default), we want to show all constellations
	if (values.size() == 0) {
		for (ConstellationBound& b : _constellationBounds)
			b.isEnabled = true;
	}
	else {
		// In the worst case, this algorithm runs with 2 * nConstellations, which is
		// acceptable as the number of constellations is < 100
		// First disable all constellations
		for (ConstellationBound& b : _constellationBounds)
			b.isEnabled = false;
		// then re-enable the ones for which we have indices
		for (int value : values)
			_constellationBounds[value].isEnabled = true;
	}
}

} // namespace openspace
