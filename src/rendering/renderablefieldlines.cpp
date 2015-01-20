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

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/assert.h>

#include <fstream>

namespace {
	const std::string _loggerCat = "RenderableFieldlines";

	const std::string keyVectorField = "VectorField";
	const std::string keyVectorFieldType = "Type";
	const std::string keyVectorFieldFile = "File";

	const std::string keySeedPoints = "SeedPoints";
	const std::string keySeedPointsStepSize = "Stepsize";
	const std::string keySeedPointsClassification = "Classification";
	const std::string keySeedPointsSource = "Seedpoints";

	const int SeedPointSourceFile = 0;
}

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary) 
	: Renderable(dictionary)
	, _seedPointSource("source", "Seedpoint Source")
	, _seedPointSourceFile("sourceFile", "Seedpoint File")
	, _fieldlineVAO(0)
	, _vertexPositionBuffer(0)
	, _program(nullptr)
	, _programIsDirty(false)
{
	ghoul_assert(
		dictionary.hasKeyAndValue<std::string>(constants::scenegraphnode::keyName),
		"Renderable does not have a name"
	);

	std::string name;
	dictionary.getValue(constants::scenegraphnode::keyName, name);

	bool success = dictionary.getValue(keyVectorField, _vectorFieldInformation);
	if (!success) {
		LERROR("RenderableFieldlines '" << name << "' does not contain a key for '" <<
			keyVectorField << "'");
	}

	success = dictionary.getValue(keySeedPoints, _seedPointInformation);
	if (!success) {
		LERROR("RenderableFieldlines '" << name << "' does not contain a key for '" <<
			keySeedPoints << "'");
	}

	// @TODO a non-magic number perhaps ---abock
	setBoundingSphere(PowerScaledScalar::CreatePSS(250.f*6371000.f));

	_seedPointSource.addOption(SeedPointSourceFile, "File");
	addProperty(_seedPointSource);

	//_seedPointSourceFile = 
	addProperty(_seedPointSourceFile);
}

bool RenderableFieldlines::isReady() const {
	return _program != nullptr;
}

bool RenderableFieldlines::initialize() {
	int prevEnd = 0;
	std::vector<LinePoint> vertexData;
	std::vector<std::vector<LinePoint> > fieldlinesData;

	// Read data from fieldlines dictionary
	fieldlinesData = getFieldlinesData();

	if (fieldlinesData.empty())
		return false;

	// Arrange data for glMultiDrawArrays
	for (int j = 0; j < fieldlinesData.size(); ++j) {
		_lineStart.push_back(prevEnd);
		_lineCount.push_back(static_cast<int>(fieldlinesData[j].size()));
		prevEnd = prevEnd + static_cast<int>(fieldlinesData[j].size());
		vertexData.insert( vertexData.end(), fieldlinesData[j].begin(), fieldlinesData[j].end());
	}
	LDEBUG("Number of vertices : " << vertexData.size());

	//	------ FIELDLINES -----------------
	glGenVertexArrays(1, &_fieldlineVAO); // generate array
	glBindVertexArray(_fieldlineVAO); // bind array
	glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
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

	_program = ghoul::opengl::ProgramObject::Build(
		"Fieldline",
		"${SHADERS}/modules/fieldlines/fieldline_vs.glsl",
		"${SHADERS}/modules/fieldlines/fieldline_fs.glsl",
		"${SHADERS}/modules/fieldlines/fieldline_gs.glsl"
	);
	if (!_program)
		return false;

	_program->setProgramObjectCallback(
		[&](ghoul::opengl::ProgramObject*) {
			this->_programIsDirty = true;
		}
	);

	return true;
}

bool RenderableFieldlines::deinitialize() {
	glDeleteVertexArrays(1, &_fieldlineVAO);
	_fieldlineVAO = 0;
	glDeleteBuffers(1, &_vertexPositionBuffer);
	_vertexPositionBuffer = 0;
	return true;
}

void RenderableFieldlines::render(const RenderData& data) {
	
	_program->activate();
	_program->setUniform("modelViewProjection", data.camera.viewProjectionMatrix());
	_program->setUniform("modelTransform", glm::mat4(1.0));
	_program->setUniform("cameraViewDir", data.camera.viewDirection());
	setPscUniforms(_program, &data.camera, data.position);

	//	------ DRAW FIELDLINES -----------------
	glBindVertexArray(_fieldlineVAO);
	glMultiDrawArrays(GL_LINE_STRIP_ADJACENCY, &_lineStart[0], &_lineCount[0], static_cast<GLsizei>(_lineStart.size()));
	glBindVertexArray(0);

	_program->deactivate();
}

void RenderableFieldlines::update(const UpdateData&) {
	if (_programIsDirty) {
		_program->rebuildFromFile();
		_programIsDirty = true;
	}
}

std::vector<std::vector<LinePoint>> RenderableFieldlines::getFieldlinesData() {
	ghoul::Dictionary hintsDictionary = _vectorFieldInformation;
	std::string filename;
	_vectorFieldInformation.getValue("File", filename);
	filename = absPath(filename);
	
	std::string modelString, xVariable, yVariable, zVariable;
	KameleonWrapper::Model model;
	std::vector<std::vector<LinePoint> > fieldlinesData;
	bool classification = false, lorentz = false;
	glm::vec4 fieldlineColor = glm::vec4(1.0, 1.0, 1.0, 1.0); // default color if no color or classification is specified
	float stepSize = 0.5; // default if no stepsize is specified in hints

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
			bool xVar;
			xVar = hintsDictionary.getValue("Variables.1", xVariable);
			if (xVar && xVariable == "Lorentz") {
				lorentz = true;
			} else {
				bool yVar, zVar;

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
		if (!_seedPointInformation.hasKey("Stepsize") || !_seedPointInformation.getValue("Stepsize", stepSize)) {
			LDEBUG("No stepsize set for fieldlines. Setting to default value (" << stepSize << ")");
		}

		//	------ SEEDPOINTS ---------------
		_seedPoints.clear();
		if (_seedPointInformation.hasKey("Seedpoints")) {
			if (_seedPointInformation.hasKeyAndValue<ghoul::Dictionary>("Seedpoints")) {
				LINFO("Loading provided list of seed points");
				ghoul::Dictionary seedpointsDictionary;
				_seedPointInformation.getValue("Seedpoints", seedpointsDictionary);
				glm::vec3 seedPos;
				for (const std::string& index : seedpointsDictionary.keys()) {
					_seedPointInformation.getValue("Seedpoints." + index, seedPos);
					_seedPoints.push_back(seedPos);
				}
			}
			else if (_seedPointInformation.hasKeyAndValue<std::string>("Seedpoints")) {
				std::string seedPointsFile;
				_seedPointInformation.getValue("Seedpoints", seedPointsFile);
				seedPointsFile = absPath(seedPointsFile);
				LINFO("Reading seed points from file '" << seedPointsFile << "'");

				std::ifstream seedFile(seedPointsFile);
				if (!seedFile.good())
					LERROR("Could not open seed points file '" << seedPointsFile << "'");
				else {
					std::string line;
					glm::vec3 point;
					while (std::getline(seedFile, line)) {
						std::stringstream s(line);
						s >> point.x;
						s >> point.y;
						s >> point.z;
						_seedPoints.push_back(std::move(point));
					}
				}
			}
		}
		else
			LERROR("Fieldlines did not provide seed points");

		//	------ CLASSIFICATION & COLOR -----------
		_seedPointInformation.getValue("Color", fieldlineColor);
		_seedPointInformation.getValue("Classification", classification);
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

} // namespace openspace
