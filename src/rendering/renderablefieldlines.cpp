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

#include <openspace/rendering/renderablefieldlines.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/kameleonwrapper.h>

#include <openspace/util/constants.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/texture.h>

namespace {
	const std::string _loggerCat = "RenderableFieldlines";

	const std::string keyFieldlines = "Fieldlines";
	const std::string keyFilename = "File";
	const std::string keyHints = "Hints";
}

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary) 
	: Renderable(dictionary)
	, _fieldlineVAO(0)
	, _shader(nullptr)
	, _texture(nullptr)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	assert(success);

	ghoul::Dictionary fieldlines;
	success = dictionary.getValue(keyFieldlines, fieldlines);
	if (!success) {
		LERROR("RenderableFieldlines '" << name << "' did not contain a '" <<
			keyFieldlines << "' key");
		return;
	}
	for (const std::string& key : fieldlines.keys()) {
		ghoul::Dictionary fieldline;
		success = fieldlines.getValue(key, fieldline);

		if (!success) {
			LERROR("Key '" << key << "' in '" << keyFieldlines <<
				"' of the RenderableFieldlines '" << name <<
				"' does not have a table as value");
			continue;
		}

		std::string fileName;
		fieldline.getValue(keyFilename, fileName);
		fileName = findPath(fileName);
		if (fileName.empty())
			LERROR("File not found!");
		else {
			ghoul::Dictionary hintsDictionary;
			fieldline.getValue(keyHints, hintsDictionary);

			_filenames.push_back(fileName);
			_hintsDictionaries.push_back(hintsDictionary);
		}
	}

	setBoundingSphere(PowerScaledScalar::CreatePSS(250.f*6371000.f)); // FIXME a non-magic number perhaps
}

RenderableFieldlines::~RenderableFieldlines() {
}

bool RenderableFieldlines::initialize() {
	assert(_filenames.size() != 0);
	assert(_hintsDictionaries.size() != 0);

	int prevEnd = 0;
	std::vector<LinePoint> vertexData, seedPointsData;
	std::vector<std::vector<LinePoint> > fieldlinesData;
	glm::vec4 seedPointsColor = glm::vec4(1.0, 0.5, 0.0, 1.0);

	for (int i = 0; i < _filenames.size(); ++i) {
		fieldlinesData = getFieldlinesData(_filenames[i], _hintsDictionaries[i]);

		for (int j = 0; j < fieldlinesData.size(); ++j) {
			_lineStart.push_back(prevEnd);
			_lineCount.push_back(fieldlinesData[j].size());
			prevEnd = prevEnd + fieldlinesData[j].size();
			vertexData.insert( vertexData.end(), fieldlinesData[j].begin(), fieldlinesData[j].end());
		}
		// Give seedpoints a color for visualizing as GL_POINTS
		for (glm::vec3 seedPoint : _seedPoints) {
			seedPointsData.push_back(LinePoint(seedPoint, seedPointsColor));
		}
	}
	LDEBUG("Number of vertices : " << vertexData.size());

	//	------ FIELDLINES -----------------
	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_fieldlineVAO); // generate array
	glBindVertexArray(_fieldlineVAO); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, vertexData.size()*sizeof(LinePoint), &vertexData.front(), GL_STATIC_DRAW);

	// Vertex positions
	GLuint vertexLocation = 0;
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, sizeof(LinePoint), reinterpret_cast<void*>(0));

	// Vertex colors
	GLuint colorLocation = 1;
	glEnableVertexAttribArray(colorLocation);
	glVertexAttribPointer(colorLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(sizeof(glm::vec3)));

	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array

	//	------ SEEDPOINTS -----------------
	GLuint seedpointPositionBuffer;
	glGenVertexArrays(1, &_seedpointVAO); // generate array
	glBindVertexArray(_seedpointVAO); // bind array
	glGenBuffers(1, &seedpointPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, seedpointPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, seedPointsData.size()*sizeof(LinePoint), &seedPointsData.front(), GL_STATIC_DRAW);

	// Vertex positions
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, sizeof(LinePoint), reinterpret_cast<void*>(0));

	// Vertex colors
	glEnableVertexAttribArray(colorLocation);
	glVertexAttribPointer(colorLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(sizeof(glm::vec3)));

	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array	

	OsEng.ref().configurationManager().getValue("FieldlineProgram", _shader);
	assert(_shader);

	loadTexture("C:/openspace/openspace-data/test.png");

	return true;
}

bool RenderableFieldlines::deinitialize() {
	return true;
}

void RenderableFieldlines::render(const RenderData& data) {
	if (!_shader)
		return;
	
	_shader->activate();
	_shader->setUniform("modelViewProjection", data.camera.viewProjectionMatrix());
	_shader->setUniform("modelTransform", glm::mat4(1.0));
	_shader->setUniform("cameraViewDir", data.camera.viewDirection());
	setPscUniforms(_shader, &data.camera, data.position);

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

	//	------ DRAW FIELDLINES -----------------
	glEnable(GL_POLYGON_SMOOTH);
	glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
	glBindVertexArray(_fieldlineVAO);
	glMultiDrawArrays(GL_LINE_STRIP_ADJACENCY, &_lineStart[0], &_lineCount[0], _lineStart.size());

	////	------ DRAW SEEDPOINTS -----------------
	//glBindVertexArray(_seedpointVAO);
	//glMultiDrawArrays(GL_LINE_STRIP_ADJACENCY, &_lineStart[0], &_lineCount[0], _seedPoints.size());

	glDisable(GL_POLYGON_SMOOTH);
	glBindVertexArray(0);
	_shader->deactivate();
}

std::vector<std::vector<LinePoint> > RenderableFieldlines::getFieldlinesData(std::string filename, ghoul::Dictionary hintsDictionary) {
	std::string modelString;
	float stepSize = 0.5; // default if no stepsize is specified in hints
	std::string xVariable, yVariable, zVariable;
	KameleonWrapper::Model model;
	std::vector<std::vector<LinePoint> > fieldlinesData;

	bool classification = false, lorentz = false;
	glm::vec4 fieldlineColor = glm::vec4(1.0, 1.0, 1.0, 1.0); // default color if no color or classification is specified

	if (hintsDictionary.hasKey("Model") && hintsDictionary.getValue("Model", modelString)) {
		//	------ MODEL -----------------
		if (modelString == "BATSRUS") {
			model = KameleonWrapper::Model::BATSRUS;
		} else if (modelString == "ENLIL") {
			LWARNING("ENLIL model not supported for fieldlines");
			return fieldlinesData;
		} else {
			LWARNING("Hints does not specify a valid 'Model'");
			return fieldlinesData;
		}

		//	------ VARIBLES / LORENTZ -----------------
		if (hintsDictionary.hasKey("Variables")) {
			bool xVar, yVar, zVar;
			xVar = hintsDictionary.getValue("Variables.1", xVariable);
			if (xVar && xVariable == "Lorentz") {
				lorentz = true;
			} else {

				yVar = hintsDictionary.getValue("Variables.2", yVariable);
				zVar = hintsDictionary.getValue("Variables.3", zVariable);

				if (!xVar || !yVar || !zVar) {
					LWARNING("Error reading variables! Must be 3 and must exist in CDF data");
					return fieldlinesData;
				}
			}
		} else {
			LWARNING("Hints does not specify  valid 'Variables'");
			return fieldlinesData;
		}

		//	------ STEPSIZE -----------------
		if (!hintsDictionary.hasKey("Stepsize") || !hintsDictionary.getValue("Stepsize", stepSize)) {
			LDEBUG("No stepsize set for fieldlines. Setting to default value (" << stepSize << ")");
		}

		//	------ SEEDPOINTS ---------------
		ghoul::Dictionary seedpointsDictionary;
		_seedPoints.clear();
		if (hintsDictionary.hasKey("Seedpoints") && hintsDictionary.getValue("Seedpoints", seedpointsDictionary)) {
			glm::vec3 seedPos;
			for (auto index : seedpointsDictionary.keys()) {
				hintsDictionary.getValue("Seedpoints."+index, seedPos);
				_seedPoints.push_back(seedPos);
			}
		}

		//	------ CLASSIFICATION & COLOR -----------
		hintsDictionary.getValue("Color", fieldlineColor);
		hintsDictionary.getValue("Classification", classification);
	} else {
        // model unitialized!
        assert(false);
    }

	KameleonWrapper kw(filename);
	if (lorentz) {
		fieldlinesData = kw.getLorentzTrajectories(_seedPoints, fieldlineColor, stepSize);
	} else {
		if (classification)
			fieldlinesData = kw.getClassifiedFieldLines(xVariable, yVariable, zVariable, _seedPoints, stepSize);
		else
			fieldlinesData = kw.getFieldLines(xVariable, yVariable, zVariable, _seedPoints, stepSize, fieldlineColor);
	}

	return fieldlinesData;
}

// TEST
void RenderableFieldlines::loadTexture(std::string path) {
	if (path != "") {
		ghoul::opengl::Texture* texture = ghoul::opengl::loadTexture(path);
		if (texture) {
			LDEBUG("Loaded texture from '" << path << "'");
			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

			if (_texture)
				delete _texture;
			_texture = texture;
		}
	}
}

} // namespace openspace
