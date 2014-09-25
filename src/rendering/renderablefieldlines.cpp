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

namespace {
	const std::string _loggerCat = "RenderableFieldlines";

	const std::string keyFieldlines = "Fieldlines";
	const std::string keyFilename = "File";
	const std::string keyHints = "Hints";
	const std::string keyShaders = "Shaders";
	const std::string keyVertexShader = "VertexShader";
	const std::string keyFragmentShader = "FragmentShader";
}

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary) 
	: Renderable(dictionary)
	, _VAO(0)
	, _programUpdateOnSave(false)
	, _update(false)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	assert(success);

	ghoul::Dictionary fieldlines;
	success = dictionary.getValueSafe(keyFieldlines, fieldlines);
	if (!success) {
		LERROR("RenderableFieldlines '" << name << "' did not contain a '" <<
			keyFieldlines << "' key");
		return;
	}
	for (const std::string& key : fieldlines.keys()) {
		ghoul::Dictionary fieldline;
		success = fieldlines.getValueSafe(key, fieldline);

		if (!success) {
			LERROR("Key '" << key << "' in '" << keyFieldlines <<
				"' of the RenderableFieldlines '" << name <<
				"' does not have a table as value");
			continue;
		}

		std::string fileName;
		fieldline.getValueSafe(keyFilename, fileName);
		fileName = findPath(fileName);
		if (fileName.empty())
			LERROR("File not found!");
		else {
			ghoul::Dictionary hintsDictionary;
			fieldline.getValueSafe(keyHints, hintsDictionary);

			_filenames.push_back(fileName);
			_hintsDictionaries.push_back(hintsDictionary);
		}
	}

	ghoul::Dictionary shaderDictionary;
	success = dictionary.getValueSafe(keyShaders, shaderDictionary);
	if (!success) {
		LERROR("RenderableFieldlines '" << name << "' does not contain a '" <<
			keyShaders << "' table");
		return;
	}

	std::string vshaderpath;
	success = shaderDictionary.getValueSafe(keyVertexShader, vshaderpath);
	if (!success) {
		LERROR("RenderableFieldlines '" << name << "' does not have a '" <<
			keyVertexShader << "'");
		return;
	}
	vshaderpath = findPath(vshaderpath);

	std::string fshaderpath;
	success = shaderDictionary.getValueSafe(keyFragmentShader, fshaderpath);
	if (!success) {
		LERROR("RenderableFieldlines '" << name << "' does not have a '" <<
			keyFragmentShader << "'");
		return;
	}
	fshaderpath = findPath(fshaderpath);

	_vertexSourceFile = new ghoul::filesystem::File(vshaderpath, false);
	_fragmentSourceFile = new ghoul::filesystem::File(fshaderpath, false);


    ShaderCreator sc = OsEng.shaderBuilder();
    _fieldlinesProgram = sc.buildShader("FieldlinesProgram", vshaderpath, fshaderpath);

	dictionary.getValueSafe("UpdateOnSave", _programUpdateOnSave);

	setBoundingSphere(PowerScaledScalar::CreatePSS(5)); // FIXME a non-magic number perhaps
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
	glGenVertexArrays(1, &_VAO); // generate array
	glBindVertexArray(_VAO); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, vertexData.size()*sizeof(LinePoint), &vertexData.front(), GL_STATIC_DRAW);

	// Vertex positions
	GLuint vertexLocation = 0;
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, sizeof(LinePoint), reinterpret_cast<void*>(0));

	// Texture coordinates
	GLuint texcoordLocation = 1;
	glEnableVertexAttribArray(texcoordLocation);
	glVertexAttribPointer(texcoordLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(sizeof(glm::vec3)));

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

	// Texture coordinates
	glEnableVertexAttribArray(texcoordLocation);
	glVertexAttribPointer(texcoordLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(3*sizeof(float)));

	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array

	glPointSize(5); // size of seedpoints

	//	------ SETUP SHADERS -----------------
	auto privateCallback = [this](const ghoul::filesystem::File& file) {
		_update = true;
	};
	if(_programUpdateOnSave) {
		_vertexSourceFile->setCallback(privateCallback);
		_fragmentSourceFile->setCallback(privateCallback);
	}

	_fieldlinesProgram->compileShaderObjects();
	_fieldlinesProgram->linkProgramObject();

	return true;
}

bool RenderableFieldlines::deinitialize() {
	return true;
}

void RenderableFieldlines::render(const Camera* camera, const psc& thisPosition, RuntimeData* runtimeData) {
	if(_update) {
		_update = false;
		safeShaderCompilation();
	}

	glm::mat4 transform = camera->viewProjectionMatrix();
	glm::mat4 camTransform = camera->viewRotationMatrix();
	psc relative = thisPosition-camera->position();

	transform = transform*camTransform;
	transform = glm::mat4(1.0);
	transform = glm::scale(transform, glm::vec3(0.01));

	psc currentPosition = thisPosition;
    psc campos = camera->position();
    glm::mat4 camrot = camera->viewRotationMatrix();
    PowerScaledScalar scaling = camera->scaling();

	// Activate shader
	_fieldlinesProgram->activate();

    _fieldlinesProgram->setUniform("modelViewProjection", camera->viewProjectionMatrix());
    _fieldlinesProgram->setUniform("modelTransform", transform);
    _fieldlinesProgram->setUniform("campos", campos.vec4());
    _fieldlinesProgram->setUniform("objpos", currentPosition.vec4());
    _fieldlinesProgram->setUniform("camrot", camrot);
    _fieldlinesProgram->setUniform("scaling", scaling.vec2());

	//	------ FIELDLINES -----------------
	glBindVertexArray(_VAO);
	glMultiDrawArrays(GL_LINE_STRIP, &_lineStart[0], &_lineCount[0], _lineStart.size());

//	//	------ SEEDPOINTS -----------------
//	glBindVertexArray(_seedpointVAO);
//	glMultiDrawArrays(GL_POINTS, &_lineStart[0], &_lineCount[0], _seedPoints.size());
	glBindVertexArray(0);

	// Deactivate shader
	_fieldlinesProgram->deactivate();
}

void RenderableFieldlines::update() {
}

void RenderableFieldlines::safeShaderCompilation() {
	_fieldlinesProgram->rebuildFromFile();
	_fieldlinesProgram->compileShaderObjects();
	_fieldlinesProgram->linkProgramObject();
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
	}

	KameleonWrapper kw(filename, model);
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

} // namespace openspace
